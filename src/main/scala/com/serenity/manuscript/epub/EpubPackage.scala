package com.serenity.manuscript.epub

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.UUID

import com.serenity.manuscript.{ManuscriptMeta, NavigationLabels}

/** The package-level parts of an EPUB 3: container, package document, navigation document, stylesheet and the XHTML
  * wrapper around each content document.
  */
private[epub] object EpubPackage:

  private val Xhtml = "http://www.w3.org/1999/xhtml"
  private val Ops   = "http://www.idpf.org/2007/ops"

  val ContentPath: String = "OEBPS/content.opf"

  val container: String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
       |  <rootfiles>
       |    <rootfile full-path="$ContentPath" media-type="application/oebps-package+xml"/>
       |  </rootfiles>
       |</container>
       |""".stripMargin

  /** Without a configured identifier, a name-based UUID of the title and author: the same book keeps the same id across
    * exports, so a reader's library treats a re-export as the same book rather than a new one, and nothing random or
    * time-dependent leaks into the bytes.
    */
  def identifier(meta: ManuscriptMeta): String =
    meta.identifier.map(_.trim).filter(_.nonEmpty).getOrElse {
      val name = s"serenity-manuscript\u0000${meta.title}\u0000${meta.author.legal}"
      s"urn:uuid:${UUID.nameUUIDFromBytes(name.getBytes(StandardCharsets.UTF_8))}"
    }

  def language(meta: ManuscriptMeta): String =
    Option(meta.language.trim).filter(_.nonEmpty).getOrElse("en")

  def page(meta: ManuscriptMeta, document: EpubDocument): String =
    val tag = if document.body.exists(_.startsWith("<h")) then "section" else "div"
    xhtml(
      language(meta),
      document.title,
      "../css/style.css",
      List(s"""  <$tag epub:type="${document.epubType}">""") ++ document.body.map("    " + _) :+ s"  </$tag>"
    )

  def navigation(meta: ManuscriptMeta, content: EpubContent): String =
    val labels = NavigationLabels.resolve(meta.language, meta.labels)
    val landmarks = (content.front.find(_.epubType == "titlepage").map(("titlepage", labels.titlePage, _)) ++
      content.body.headOption.map(("bodymatter", labels.startOfContent, _))).map { (kind, label, document) =>
      s"""      <li><a epub:type="$kind" href="${document.path}">${EpubXml.line(label)}</a></li>"""
    }
    xhtml(
      language(meta),
      labels.contents,
      "css/style.css",
      List(
        """  <nav epub:type="toc" id="toc">""",
        s"    <h1>${EpubXml.line(labels.contents)}</h1>",
        tocList(content.toc, "    "),
        "  </nav>",
        """  <nav epub:type="landmarks" hidden="hidden">""",
        s"    <h2>${EpubXml.line(labels.guide)}</h2>",
        "    <ol>"
      ) ++ landmarks ++ List("    </ol>", "  </nav>")
    )

  private def tocList(entries: Vector[TocEntry], indent: String): String =
    val items = entries.map { entry =>
      val link = s"""<a href="${entry.path}">${EpubXml.line(entry.title)}</a>"""
      if entry.children.isEmpty then s"$indent  <li>$link</li>"
      else s"$indent  <li>$link\n${tocList(entry.children, indent + "    ")}\n$indent  </li>"
    }
    s"$indent<ol>\n${items.mkString("\n")}\n$indent</ol>"

  def packageDocument(meta: ManuscriptMeta, content: EpubContent, modified: Instant): String =
    val lang    = EpubXml.line(language(meta))
    val creator = Option(meta.byline.trim).filter(_.nonEmpty).orElse(Option(meta.author.legal.trim).filter(_.nonEmpty))
    val metadata = List(
      s"""    <dc:identifier id="book-id">${EpubXml.line(identifier(meta))}</dc:identifier>""",
      s"    <dc:title>${EpubXml.line(title(meta))}</dc:title>",
      s"    <dc:language>$lang</dc:language>"
    ) ++ creator.map(name => s"    <dc:creator>${EpubXml.line(name)}</dc:creator>") :+
      s"""    <meta property="dcterms:modified">${modifiedStamp(modified)}</meta>"""
    val items = content.all.map(document =>
      s"""    <item id="${document.id}" href="${document.path}" media-type="application/xhtml+xml"/>"""
    )
    val spine = content.all.map(document => s"""    <itemref idref="${document.id}"/>""")
    (List(
      """<?xml version="1.0" encoding="UTF-8"?>""",
      s"""<package xmlns="http://www.idpf.org/2007/opf" version="3.0" unique-identifier="book-id" xml:lang="$lang">""",
      """  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">"""
    ) ++ metadata ++ List(
      "  </metadata>",
      "  <manifest>",
      """    <item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav"/>""",
      """    <item id="style" href="css/style.css" media-type="text/css"/>"""
    ) ++ items ++ List("  </manifest>", "  <spine>") ++ spine ++ List("  </spine>", "</package>", "")).mkString("\n")

  /** EPUB wants `CCYY-MM-DDThh:mm:ssZ`: whole seconds, UTC. */
  def modifiedStamp(modified: Instant): String =
    modified.truncatedTo(ChronoUnit.SECONDS).toString

  private def title(meta: ManuscriptMeta): String =
    Option(meta.title.trim).filter(_.nonEmpty).getOrElse("Untitled")

  private def xhtml(lang: String, title: String, stylesheet: String, body: List[String]): String =
    val tag = EpubXml.line(lang)
    (List(
      """<?xml version="1.0" encoding="UTF-8"?>""",
      s"""<html xmlns="$Xhtml" xmlns:epub="$Ops" lang="$tag" xml:lang="$tag">""",
      "<head>",
      """  <meta charset="utf-8"/>""",
      s"  <title>${EpubXml.line(title)}</title>",
      s"""  <link rel="stylesheet" type="text/css" href="$stylesheet"/>""",
      "</head>",
      "<body>"
    ) ++ body ++ List("</body>", "</html>", "")).mkString("\n")

  val stylesheet: String =
    """body {
      |  margin: 5%;
      |  line-height: 1.4;
      |}
      |h1, h2 {
      |  text-align: center;
      |  font-weight: bold;
      |  margin: 3em 0 2em;
      |}
      |h1.title {
      |  margin-top: 30%;
      |}
      |p {
      |  margin: 0;
      |  text-indent: 1.5em;
      |}
      |p.first, p.byline, p.centered, p.dedication, p.end-marker, p.scene-break {
      |  text-indent: 0;
      |}
      |p.byline, p.centered, p.dedication, p.end-marker, p.scene-break {
      |  text-align: center;
      |}
      |p.byline {
      |  margin-top: 1em;
      |}
      |p.dedication {
      |  margin-top: 30%;
      |  font-style: italic;
      |}
      |p.scene-break {
      |  margin: 1.4em 0;
      |}
      |p.end-marker {
      |  margin-top: 2em;
      |}
      |blockquote {
      |  margin: 1em 2em;
      |}
      |blockquote p {
      |  text-indent: 0;
      |}
      |pre {
      |  font-family: monospace;
      |  white-space: pre-wrap;
      |}
      |.underline {
      |  text-decoration: underline;
      |}
      |""".stripMargin
