package com.serenity.manuscript.docx

import com.serenity.manuscript.{
  AuthorName,
  CompileSpec,
  HeadingTemplate,
  Manuscript,
  ManuscriptCompiler,
  ManuscriptFormat,
  SourceDocument
}

/** One small book that exercises every part of the manuscript layout the golden files pin down. */
object ManuscriptDocxFixture:

  val spec: CompileSpec = CompileSpec
    .forTitle("The Long Night")
    .copy(
      shortTitle = Some("NIGHT"),
      author = AuthorName.fromLegal("Jane Q. Writer"),
      contact = List("1 High Street", "Springfield", "jane@example.com"),
      chapterHeading = HeadingTemplate("Chapter <$n>\n<$t>"),
      dedication = Some("For M.")
    )

  val markdown: String =
    """# Arrival
      |
      |The train was late, and "nobody" said a word.
      |
      |She waited -- *quietly* -- by the door.
      |
      |#
      |
      |Morning came.
      |
      |# The Storm
      |
      |> A quoted letter & a \<tag>.
      |
      |It rained.""".stripMargin

  def manuscript(format: ManuscriptFormat = ManuscriptFormat.Modern): Manuscript =
    ManuscriptCompiler
      .compile(
        spec.copy(format = format, transforms = format.defaultTransforms),
        List(SourceDocument.Markdown(markdown))
      )
      .fold(error => sys.error(error.message), identity)
