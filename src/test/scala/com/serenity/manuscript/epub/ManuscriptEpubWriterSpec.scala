package com.serenity.manuscript.epub

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import java.util.UUID
import java.util.zip.{ZipEntry, ZipInputStream}
import javax.xml.parsers.DocumentBuilderFactory

import com.serenity.manuscript.{AuthorName, Manuscript}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.w3c.dom.{Document, Element}

class ManuscriptEpubWriterSpec extends AnyFlatSpec with Matchers:

  private val OpfNs   = "http://www.idpf.org/2007/opf"
  private val XhtmlNs = "http://www.w3.org/1999/xhtml"
  private val OpsNs   = "http://www.idpf.org/2007/ops"

  private lazy val bytes = ManuscriptEpubWriter.write(ManuscriptEpubFixture.plain, ManuscriptEpubFixture.modified)

  final private case class Entry(name: String, method: Int, extra: Int, time: LocalDateTime, content: String)

  private def entries(archive: Array[Byte]): List[Entry] =
    val input = ZipInputStream(ByteArrayInputStream(archive))
    try
      Iterator
        .continually(Option(input.getNextEntry))
        .takeWhile(_.isDefined)
        .flatten
        .map { entry =>
          Entry(
            entry.getName,
            entry.getMethod,
            Option(entry.getExtra).fold(0)(_.length),
            entry.getTimeLocal,
            String(input.readAllBytes(), StandardCharsets.UTF_8)
          )
        }
        .toList
    finally input.close()

  private def part(name: String, archive: Array[Byte] = bytes): String =
    entries(archive)
      .collectFirst { case entry if entry.name == name => entry.content }
      .getOrElse(fail(s"missing $name"))

  private def xml(content: String): Document =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.newDocumentBuilder().parse(ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8)))

  private def elements(document: Document, namespace: String, localName: String): List[Element] =
    val nodes = document.getElementsByTagNameNS(namespace, localName)
    (0 until nodes.getLength).toList.map(nodes.item).collect { case element: Element => element }

  private def golden(name: String): String =
    Files.readString(Path.of(getClass.getResource(s"/export/golden/epub-$name").toURI), StandardCharsets.UTF_8)

  private def canonical(content: String): String =
    content.replace("\r\n", "\n").linesIterator.map(_.stripTrailing).mkString("\n").trim

  "ManuscriptEpubWriter" should "write mimetype first, stored, uncompressed, with no extra field" in {
    val first = entries(bytes).headOption.getOrElse(fail("empty archive"))

    first.name shouldBe "mimetype"
    first.method shouldBe ZipEntry.STORED
    first.extra shouldBe 0
    first.content shouldBe "application/epub+zip"
  }

  it should "start the raw archive with the mimetype entry so readers can sniff it at a fixed offset" in {
    val header = String(bytes.slice(30, 30 + "mimetype".length + "application/epub+zip".length), StandardCharsets.UTF_8)

    header shouldBe "mimetypeapplication/epub+zip"
  }

  it should "write the remaining parts in a fixed order with fixed timestamps" in {
    val all = entries(bytes)

    all.map(_.name) shouldBe List(
      "mimetype",
      "META-INF/container.xml",
      "OEBPS/content.opf",
      "OEBPS/nav.xhtml",
      "OEBPS/css/style.css",
      "OEBPS/text/titlepage.xhtml",
      "OEBPS/text/dedication.xhtml",
      "OEBPS/text/chapter-001.xhtml",
      "OEBPS/text/chapter-002.xhtml"
    )
    all.map(_.time).distinct shouldBe List(LocalDateTime.of(1980, 1, 1, 0, 0, 2))
  }

  it should "be byte-for-byte deterministic" in {
    ManuscriptEpubWriter.write(ManuscriptEpubFixture.plain, ManuscriptEpubFixture.modified) shouldBe bytes
  }

  it should "match the golden package document, navigation document, container and stylesheet" in {
    canonical(part("META-INF/container.xml")) shouldBe canonical(golden("container.xml"))
    canonical(part("OEBPS/content.opf")) shouldBe canonical(golden("content.opf"))
    canonical(part("OEBPS/nav.xhtml")) shouldBe canonical(golden("nav.xhtml"))
    canonical(part("OEBPS/css/style.css")) shouldBe canonical(golden("style.css"))
  }

  it should "match the golden title page, dedication and chapters" in {
    canonical(part("OEBPS/text/titlepage.xhtml")) shouldBe canonical(golden("titlepage.xhtml"))
    canonical(part("OEBPS/text/dedication.xhtml")) shouldBe canonical(golden("dedication.xhtml"))
    canonical(part("OEBPS/text/chapter-001.xhtml")) shouldBe canonical(golden("chapter-001.xhtml"))
    canonical(part("OEBPS/text/chapter-002.xhtml")) shouldBe canonical(golden("chapter-002.xhtml"))
  }

  it should "list the spine in section order, with the front matter first" in {
    val opf      = xml(part("OEBPS/content.opf"))
    val manifest = elements(opf, OpfNs, "item").map(item => item.getAttribute("id") -> item.getAttribute("href")).toMap
    val spine    = elements(opf, OpfNs, "itemref").map(_.getAttribute("idref")).map(manifest)

    spine shouldBe List(
      "text/titlepage.xhtml",
      "text/dedication.xhtml",
      "text/chapter-001.xhtml",
      "text/chapter-002.xhtml"
    )
  }

  it should "link the table of contents to the spine documents in the same order" in {
    val nav = xml(part("OEBPS/nav.xhtml"))
    val toc = elements(nav, XhtmlNs, "nav")
      .find(_.getAttributeNS(OpsNs, "type") == "toc")
      .getOrElse(fail("no toc navigation"))
    val links = toc.getElementsByTagNameNS(XhtmlNs, "a")
    val hrefs =
      (0 until links.getLength).toList.map(links.item).collect { case link: Element => link.getAttribute("href") }

    hrefs shouldBe List("text/chapter-001.xhtml", "text/chapter-002.xhtml")
  }

  it should "declare the navigation document in the manifest with the nav property" in {
    val items = elements(xml(part("OEBPS/content.opf")), OpfNs, "item")

    items.filter(_.getAttribute("properties") == "nav").map(_.getAttribute("href")) shouldBe List("nav.xhtml")
  }

  it should "write well-formed XML for every part" in {
    val xmlParts =
      entries(bytes).filter(e => e.name.endsWith(".xhtml") || e.name.endsWith(".opf") || e.name.endsWith(".xml"))

    xmlParts should not be empty
    xmlParts.foreach(entry => withClue(entry.name)(noException should be thrownBy xml(entry.content)))
  }

  it should "escape ampersands, angle brackets and quotes in titles and text, and keep a literal tag as text" in {
    val book  = ManuscriptEpubWriter.write(ManuscriptEpubFixture.withParts, ManuscriptEpubFixture.modified)
    val opf   = part("OEBPS/content.opf", book)
    val title = elements(xml(opf), "http://purl.org/dc/elements/1.1/", "title").map(_.getTextContent)
    val prose = xml(part("OEBPS/text/chapter-001.xhtml", book))

    title shouldBe List("Tom & \"Jerry\" <Ltd>")
    opf should include("<dc:title>Tom &amp; &quot;Jerry&quot; &lt;Ltd&gt;</dc:title>")
    elements(prose, XhtmlNs, "p").map(_.getTextContent) should contain(
      "Salt & \"pepper\" <b>not bold</b> and 'quoted'."
    )
    elements(prose, XhtmlNs, "b") shouldBe empty
    elements(prose, XhtmlNs, "h2").map(_.getTextContent) shouldBe List("One & Two")
  }

  it should "nest chapters under their part in the table of contents, and keep part pages in the spine" in {
    val book = ManuscriptEpubWriter.write(ManuscriptEpubFixture.withParts, ManuscriptEpubFixture.modified)

    entries(book).map(_.name).filter(_.startsWith("OEBPS/text/")) shouldBe List(
      "OEBPS/text/titlepage.xhtml",
      "OEBPS/text/part-001.xhtml",
      "OEBPS/text/chapter-001.xhtml",
      "OEBPS/text/chapter-002.xhtml",
      "OEBPS/text/part-002.xhtml",
      "OEBPS/text/chapter-003.xhtml"
    )
    canonical(part("OEBPS/nav.xhtml", book)) shouldBe canonical(golden("parts-nav.xhtml"))
    canonical(part("OEBPS/text/chapter-002.xhtml", book)) shouldBe canonical(golden("parts-chapter-002.xhtml"))
  }

  it should "keep preformatted lines intact, including a line that starts with a pipe" in {
    val book = ManuscriptEpubWriter.write(ManuscriptEpubFixture.withParts, ManuscriptEpubFixture.modified)

    elements(xml(part("OEBPS/text/chapter-002.xhtml", book)), XhtmlNs, "pre").map(_.getTextContent) shouldBe
      List("| piped line\n  indented <code>")
  }

  it should "use the configured identifier and language, and derive a stable UUID otherwise" in {
    val configured = part(
      "OEBPS/content.opf",
      ManuscriptEpubWriter.write(ManuscriptEpubFixture.withParts, ManuscriptEpubFixture.modified)
    )
    val derived =
      elements(xml(part("OEBPS/content.opf")), "http://purl.org/dc/elements/1.1/", "identifier").map(_.getTextContent)

    configured should include("<dc:identifier id=\"book-id\">urn:isbn:9780000000002</dc:identifier>")
    configured should include("<dc:language>en-GB</dc:language>")
    derived.map(_.stripPrefix("urn:uuid:")).map(UUID.fromString(_).version) shouldBe List(3)
    val renamed = ManuscriptEpubFixture.plain.copy(meta =
      ManuscriptEpubFixture.plain.meta.copy(author = AuthorName.fromLegal("Someone Else"))
    )
    part("OEBPS/content.opf", ManuscriptEpubWriter.write(renamed, ManuscriptEpubFixture.modified)) should not be part(
      "OEBPS/content.opf"
    )
  }

  it should "keep the identifier when only the text or the export time changes" in {
    def identifier(book: Manuscript, at: java.time.Instant): String =
      part("OEBPS/content.opf", ManuscriptEpubWriter.write(book, at)).linesIterator
        .find(_.contains("dc:identifier"))
        .getOrElse("")

    val edited = ManuscriptEpubFixture.plain.copy(endMarker = Some("FIN"))

    identifier(edited, ManuscriptEpubFixture.modified.plusSeconds(3600)) shouldBe identifier(
      ManuscriptEpubFixture.plain,
      ManuscriptEpubFixture.modified
    )
  }

  it should "drop characters XML cannot carry rather than write an unparseable chapter" in {
    val meta   = ManuscriptEpubFixture.plain.meta
    val broken = ManuscriptEpubFixture.plain.copy(meta = meta.copy(title = "Bell\u0007 ring"))

    val book = ManuscriptEpubWriter.write(broken, ManuscriptEpubFixture.modified)

    elements(xml(part("OEBPS/text/titlepage.xhtml", book)), XhtmlNs, "h1").map(_.getTextContent) shouldBe List(
      "Bell ring"
    )
  }
