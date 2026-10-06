package com.serenity.manuscript.epub

import java.time.Instant

import com.serenity.manuscript.docx.ManuscriptDocxFixture
import com.serenity.manuscript.{
  AuthorName,
  CompileSpec,
  HeadingTemplate,
  Manuscript,
  ManuscriptCompiler,
  SectionRules,
  SourceDocument
}

object ManuscriptEpubFixture:

  val modified: Instant = Instant.parse("2026-10-05T12:30:00Z")

  /** Chapters only: a title page, a dedication, a scene break, a block quote and an end marker. */
  def plain: Manuscript = ManuscriptDocxFixture.manuscript()

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
