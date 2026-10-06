package com.serenity.manuscript.epub

import java.time.Instant

import com.serenity.manuscript.docx.ManuscriptDocxFixture
import com.serenity.manuscript.{
  AuthorName,
  CompileError,
  CompileSpec,
  HeadingTemplate,
  Manuscript,
  ManuscriptCompiler,
  ManuscriptConf,
  SectionRules,
  SourceDocument
}

object ManuscriptEpubFixture:

  val modified: Instant = Instant.parse("2026-10-05T12:30:00Z")

  /** Chapters only: a title page, a dedication, a scene break, a block quote and an end marker. */
  def plain: Manuscript = ManuscriptDocxFixture.manuscript()

  /** The plain book, in French: the navigation document takes its headings from the language. */
  def french: Manuscript =
    plain.copy(meta = plain.meta.copy(language = "fr"))

  val labelConf: String =
    """language = "de"
      |labels.contents = "Inhaltsverzeichnis"
      |labels.start-of-content = "Zum Textbeginn"
      |""".stripMargin

  /** The plain book, in German, with two of the navigation headings overridden by its `manuscript.conf`. */
  def labelled: Manuscript = fromConf(labelConf)

  def fromConf(conf: String): Manuscript =
    ManuscriptConf
      .decode(conf, ManuscriptDocxFixture.spec)
      .flatMap(ManuscriptCompiler.compile(_, List(SourceDocument.Markdown(ManuscriptDocxFixture.markdown))))
      .fold(error => sys.error(error.message), identity)

  /** A dedicated book whose only chapter has no heading, so its titles come from the label table, then `conf`. */
  def headingless(conf: String): Either[CompileError, Manuscript] =
    ManuscriptConf
      .decode(conf, CompileSpec.forTitle("Untitled").copy(dedication = Some("For M.")))
      .flatMap(ManuscriptCompiler.compile(_, List(SourceDocument.Markdown("Just prose."))))

  /** Raw HTML in the source: marks, breaks, a block, and tags that must never reach the output as markup. */
  def withHtml: Manuscript =
    val markdown =
      """# Markup
        |
        |A <b>bold</b>, <i>italic</i> and <u>underlined</u> word,<br>then H<sub>2</sub>O and x<sup>2</sup>.
        |
        |<div class="note" onclick="steal()">Block <em>text</em> &amp; more &lt;3</div>
        |
        |<script>alert("x")</script>
        |
        |An <img src="x.png" onerror="steal()"> unclosed <b>tag and a stray < sign & ampersand.""".stripMargin
    ManuscriptCompiler
      .compile(
        CompileSpec.forTitle("Markup").copy(transforms = Nil),
        List(SourceDocument.Markdown(markdown))
      )
      .fold(error => sys.error(error.message), identity)

  /** Two parts, a part-less chapter's worth of awkward characters, preformatted text and an underlined run. */
  def withParts: Manuscript =
    val spec = CompileSpec
      .forTitle("Tom & \"Jerry\" <Ltd>")
      .copy(
        author = AuthorName.fromLegal("Ann O'Neil"),
        rules = SectionRules(Some(1), 2, SectionRules.DefaultSceneBreakPatterns),
        partHeading = HeadingTemplate("Part <$R>: <$t>"),
        chapterHeading = HeadingTemplate("<$t>"),
        transforms = Nil,
        language = "en-GB",
        identifier = Some("urn:isbn:9780000000002")
      )
    val markdown =
      """# Beginnings
        |
        |## One & Two
        |
        |Salt & "pepper" \<b>not bold\</b> and 'quoted'.
        |
        |## Three
        |
        |```
        || piped line
        |  indented <code>
        |```
        |
        |# Endings
        |
        |## Four
        |
        |Last words.""".stripMargin
    ManuscriptCompiler
      .compile(spec, List(SourceDocument.Markdown(markdown)))
      .fold(error => sys.error(error.message), identity)
