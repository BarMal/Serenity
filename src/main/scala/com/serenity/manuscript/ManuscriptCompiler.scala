package com.serenity.manuscript

import java.util.regex.{Matcher, Pattern}

import scala.annotation.tailrec
import scala.util.Try

import cats.syntax.all.*
import com.serenity.richtext.{InlineMark, RichTextParagraph, RichTextRun, RichTextStyle}
import com.serenity.text.SmartPunctuation

/** Sources plus a [[CompileSpec]] to a [[Manuscript]]. Pure: reading files and writing the result belong to the shell.
  */
object ManuscriptCompiler:

  def compile(spec: CompileSpec, sources: List[SourceDocument]): Either[CompileError, Manuscript] =
    for
      replacements <- spec.replacements.traverse(compiled)
      sections = sources.toVector.flatMap(sectionsOf(spec.rules, _))
      _ <- Either.cond(ManuscriptText.chapters(sections).nonEmpty, (), CompileError.NothingToCompile)
      body = numbered(spec, sections.map(transformed(spec.transforms.toSet, replacements, _)))
    yield Manuscript(meta(spec, body), frontMatter(spec), body, spec.endMarker.map(_.trim).filter(_.nonEmpty))

  private def sectionsOf(rules: SectionRules, source: SourceDocument): Vector[Section] =
    source match
      case SourceDocument.Markdown(text) => MarkdownManuscript.sections(text, rules)
      case SourceDocument.Rich(document) => RichTextManuscript.sections(document, rules)

  private def meta(spec: CompileSpec, body: Vector[Section]): ManuscriptMeta =
    ManuscriptMeta(
      title = spec.title,
      shortTitle = spec.shortTitle.getOrElse(spec.title.toUpperCase),
      author = spec.author,
      byline = spec.byline.getOrElse(spec.author.legal),
      contact = spec.contact,
      wordCount = ManuscriptText.wordCount(body),
      wordCountRounding = spec.wordCountRounding
    )

  private def frontMatter(spec: CompileSpec): List[FrontMatter] =
    Option.when(spec.titlePage)(FrontMatter.TitlePage).toList ++
      spec.dedication.map(_.trim).filter(_.nonEmpty).map(FrontMatter.Dedication(_)).toList

  final private case class CompiledReplacement(pattern: Pattern, replacement: String)

  private def compiled(replacement: Replacement): Either[CompileError, CompiledReplacement] =
    if replacement.regex then
      Try(Pattern.compile(replacement.find)).toEither
        .leftMap(error => CompileError.InvalidReplacement(replacement.find, error.getMessage))
        .map(CompiledReplacement(_, replacement.replace))
    else
      Right(
        CompiledReplacement(
          Pattern.compile(Pattern.quote(replacement.find)),
          Matcher.quoteReplacement(replacement.replace)
        )
      )

  /** Chapters are numbered straight through the book, across parts; parts have their own count. */
  private def numbered(spec: CompileSpec, sections: Vector[Section]): Vector[Section] =
    sections
      .foldLeft((Vector.empty[Section], 0, 0)) {
        case ((done, parts, chapters), Section.Part(heading, nested)) =>
          val (numberedNested, chaptersAfter) = numberedChapters(spec, nested, chapters)
          val partHeading                     = spec.partHeading.render(parts + 1, heading.fold("")(_.title))
          (done :+ Section.Part(partHeading, numberedNested), parts + 1, chaptersAfter)
        case ((done, parts, chapters), chapter: Section.Chapter) =>
          (done :+ numberedChapter(spec, chapter, chapters + 1), parts, chapters + 1)
      }
      ._1

  private def numberedChapters(spec: CompileSpec, sections: Vector[Section], first: Int): (Vector[Section], Int) =
    sections.foldLeft((Vector.empty[Section], first)) {
      case ((done, count), chapter: Section.Chapter) => (done :+ numberedChapter(spec, chapter, count + 1), count + 1)
      case ((done, count), part: Section.Part)       => (done :+ part, count)
    }

  private def numberedChapter(spec: CompileSpec, chapter: Section.Chapter, number: Int): Section =
    Section.Chapter(spec.chapterHeading.render(number, chapter.heading.fold("")(_.title)), chapter.blocks)

  private def transformed(
    transforms: Set[TextTransform],
    replacements: List[CompiledReplacement],
    section: Section
  ): Section =
    section match
      case Section.Part(heading, nested) =>
        Section.Part(
          heading.map(transformedHeading(transforms, _)),
          nested.map(transformed(transforms, replacements, _))
        )
      case Section.Chapter(heading, blocks) =>
        Section.Chapter(
          heading.map(transformedHeading(transforms, _)),
          blocks.map(transformedBlock(transforms, replacements, _))
        )

  private def transformedHeading(transforms: Set[TextTransform], heading: SectionHeading): SectionHeading =
    SectionHeading(heading.lines.map(line => ManuscriptText.plainText(punctuated(transforms, List(RichTextRun(line))))))

  private def transformedBlock(
    transforms: Set[TextTransform],
    replacements: List[CompiledReplacement],
    block: Block
  ): Block =
    block match
      case Block.Paragraph(runs, kind) =>
        val afterReplacements =
          runs.map(run => if isCode(run) then run else run.copy(text = replaced(replacements, run.text)))
        val styled =
          if transforms.contains(TextTransform.ItalicsAsUnderline) then afterReplacements.map(italicsAsUnderline)
          else afterReplacements
        Block.Paragraph(RichTextParagraph(punctuated(transforms, styled)).normalized.runs, kind)
      case other => other

  private def replaced(replacements: List[CompiledReplacement], text: String): String =
    replacements.foldLeft(text)((current, replacement) =>
      replacement.pattern.matcher(current).replaceAll(replacement.replacement)
    )

  /** Punctuation is changed across run boundaries -- a quote closing an italic word opens or closes by what precedes it
    * in the previous run -- so the runs are flattened to styled characters and regrouped afterwards.
    */
  private def punctuated(transforms: Set[TextTransform], runs: List[RichTextRun]): List[RichTextRun] =
    val chars = runs.toVector.flatMap(run => run.text.toVector.map(_ -> run.style))
    val educated =
      if transforms.contains(TextTransform.SmartPunctuation) then SmartPunctuation.educateTagged(chars, isCodeStyle)
      else chars
    val dashed =
      if transforms.contains(TextTransform.EmDashAsDoubleHyphen) then
        educated.flatMap {
          case ('—', style) if !isCodeStyle(style) => Vector('-' -> style, '-' -> style)
          case other                               => Vector(other)
        }
      else educated
    val straight =
      if transforms.contains(TextTransform.StraightPunctuation) then
        SmartPunctuation.straightenTagged(dashed, isCodeStyle)
      else dashed
    regrouped(straight, Nil)

  @tailrec
  private def regrouped(chars: Vector[(Char, RichTextStyle)], runs: List[RichTextRun]): List[RichTextRun] =
    chars.headOption match
      case None => runs.reverse
      case Some((_, style)) =>
        val (same, rest) = chars.span(_._2 == style)
        regrouped(rest, RichTextRun(same.map(_._1).mkString, style) :: runs)

  private def italicsAsUnderline(run: RichTextRun): RichTextRun =
    if run.style.marks.contains(InlineMark.Italic) then
      run.copy(style = run.style.withoutMark(InlineMark.Italic).withMark(InlineMark.Underline))
    else run

  private def isCode(run: RichTextRun): Boolean = isCodeStyle(run.style)

  private def isCodeStyle(style: RichTextStyle): Boolean =
    style.fontFamily.contains(MarkdownManuscript.CodeFontFamily)
