package com.serenity.manuscript.epub

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import javax.xml.parsers.DocumentBuilderFactory

import com.serenity.manuscript.Manuscript
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/** The navigation document's headings follow the book's language, and `manuscript.conf` can override each of them. */
class ManuscriptEpubLabelsSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with EitherValues:

  private def nav(manuscript: Manuscript): String =
    ManuscriptEpubWriter
      .parts(manuscript, ManuscriptEpubFixture.modified)
      .collectFirst { case ("OEBPS/nav.xhtml", content) => content }
      .getOrElse(fail("missing nav.xhtml"))

  private def navIn(language: String): String =
    val plain = ManuscriptEpubFixture.plain
    nav(plain.copy(meta = plain.meta.copy(language = language)))

  private def headings(document: String): (String, String) =
    def tag(name: String): String = s"<$name>(.*)</$name>".r.findFirstMatchIn(document).fold("")(_.group(1))
    (tag("h1"), tag("h2"))

  private def golden(name: String): String =
    Files.readString(Path.of(getClass.getResource(s"/export/golden/epub-$name").toURI), StandardCharsets.UTF_8)

  private def canonical(content: String): String =
    content.replace("\r\n", "\n").linesIterator.map(_.stripTrailing).mkString("\n").trim

  private val labelTable = Table(
    ("language", "contents", "guide"),
    ("en", "Contents", "Guide"),
    ("en-GB", "Contents", "Guide"),
    ("fr", "Table des matières", "Guide"),
    ("de", "Inhalt", "Wegweiser"),
    ("es", "Contenido", "Guía"),
    ("it", "Indice", "Guida"),
    ("pt", "Sumário", "Guia"),
    ("nl", "Inhoud", "Gids")
  )

  "The navigation document" should "label its contents and guide in each supported language" in
    forAll(labelTable)((language, contents, guide) => headings(navIn(language)) shouldBe (contents, guide))

  it should "fall back to the base language for a regional tag, whatever its case or separator" in
    forAll(
      Table(
        ("language", "contents"),
        ("fr-CA", "Table des matières"),
        ("pt-BR", "Sumário"),
        ("de_AT", "Inhalt"),
        ("NL-be", "Inhoud"),
        ("ES", "Contenido")
      )
    )((language, contents) => headings(navIn(language))._1 shouldBe contents)

  it should "fall back to English for a language it has no table for, or none at all" in
    forAll(Table("language", "zh-Hans", "xx", "", "  ")) { language =>
      headings(navIn(language)) shouldBe ("Contents", "Guide")
    }

  it should "match the golden French navigation document" in {
    canonical(nav(ManuscriptEpubFixture.french)) shouldBe canonical(golden("fr-nav.xhtml"))
  }

  it should "let manuscript.conf override single labels on top of the language's own" in {
    canonical(nav(ManuscriptEpubFixture.labelled)) shouldBe canonical(golden("labelled-nav.xhtml"))
  }

  it should "escape an overriding label so the document stays well-formed" in {
    val book    = ManuscriptEpubFixture.fromConf("labels.contents = \"Q&A <fast> \\\"now\\\"\"")
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    val document = nav(book)

    document should include("<h1>Q&amp;A &lt;fast&gt; &quot;now&quot;</h1>")
    noException should be thrownBy factory
      .newDocumentBuilder()
      .parse(ByteArrayInputStream(document.getBytes(StandardCharsets.UTF_8)))
  }

  it should "ignore a blank overriding label" in {
    headings(nav(ManuscriptEpubFixture.fromConf("language = \"de\"\nlabels.contents = \"  \""))) shouldBe
      ("Inhalt", "Wegweiser")
  }

  private def part(manuscript: Manuscript, name: String): String =
    ManuscriptEpubWriter
      .parts(manuscript, ManuscriptEpubFixture.modified)
      .collectFirst { case (`name`, content) => content }
      .getOrElse(fail(s"missing $name"))

  private def titleOf(document: String): String =
    "<title>(.*)</title>".r.findFirstMatchIn(document).fold("")(_.group(1))

  private def documentTitles(manuscript: Manuscript): (String, String, String, String) =
    val toc = """<a href="text/chapter-001.xhtml">(.*)</a>""".r
      .findFirstMatchIn(part(manuscript, "OEBPS/nav.xhtml"))
      .fold("")(_.group(1))
    (
      titleOf(part(manuscript, "OEBPS/text/titlepage.xhtml")),
      titleOf(part(manuscript, "OEBPS/text/dedication.xhtml")),
      titleOf(part(manuscript, "OEBPS/text/chapter-001.xhtml")),
      toc
    )

  private def headingless(conf: String): Manuscript =
    ManuscriptEpubFixture.headingless(conf).value

  it should "title the title page, dedication and an untitled chapter in the book's language" in
    forAll(
      Table(
        ("language", "titles"),
        ("en", ("Title Page", "Dedication", "Chapter 1", "Chapter 1")),
        ("en-GB", ("Title Page", "Dedication", "Chapter 1", "Chapter 1")),
        ("fr", ("Page de titre", "Dédicace", "Chapitre 1", "Chapitre 1")),
        ("de", ("Titelseite", "Widmung", "Kapitel 1", "Kapitel 1")),
        ("es", ("Portada", "Dedicatoria", "Capítulo 1", "Capítulo 1")),
        ("it", ("Frontespizio", "Dedica", "Capitolo 1", "Capitolo 1")),
        ("pt-BR", ("Folha de rosto", "Dedicatória", "Capítulo 1", "Capítulo 1")),
        ("nl", ("Titelpagina", "Opdracht", "Hoofdstuk 1", "Hoofdstuk 1")),
        ("xx", ("Title Page", "Dedication", "Chapter 1", "Chapter 1"))
      )
    )((language, titles) => documentTitles(headingless(s"""language = "$language"""")) shouldBe titles)

  it should "match the golden French title page and dedication" in {
    val book = headingless("language = \"fr\"")

    canonical(part(book, "OEBPS/text/titlepage.xhtml")) shouldBe canonical(golden("fr-titlepage.xhtml"))
    canonical(part(book, "OEBPS/text/dedication.xhtml")) shouldBe canonical(golden("fr-dedication.xhtml"))
  }

  it should "let manuscript.conf override the document titles, with {n} standing for the chapter number" in {
    val conf =
      """language = "de"
        |labels.title-page = "Titel & Autor"
        |labels.dedication = "Für M."
        |labels.chapter = "Kapitel {n} ohne Titel"
        |""".stripMargin

    documentTitles(headingless(conf)) shouldBe
      ("Titel &amp; Autor", "Für M.", "Kapitel 1 ohne Titel", "Kapitel 1 ohne Titel")
  }

  it should "keep the chapter label as written when it has no {n}" in {
    documentTitles(headingless("labels.chapter = \"Unnamed\""))._3 shouldBe "Unnamed"
  }
