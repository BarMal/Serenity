package com.serenity.manuscript.epub

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import javax.xml.parsers.DocumentBuilderFactory

import com.serenity.manuscript.Manuscript
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/** The navigation document's headings follow the book's language, and `manuscript.conf` can override each of them. */
class ManuscriptEpubLabelsSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

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
