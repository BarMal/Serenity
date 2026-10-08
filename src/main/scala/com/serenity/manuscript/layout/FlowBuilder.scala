package com.serenity.manuscript.layout

import cats.syntax.all.*
import com.serenity.manuscript.typography.{LineMetrics, LineSpacing, PageTypography}
import com.serenity.manuscript.{Block, FrontMatter, Manuscript, ManuscriptMeta, ParagraphKind, Section, SectionHeading}
import com.serenity.richtext.RichTextRun

/** The lines still waiting for text to attach to: a section's heading, or scene breaks. `gapAfter` is the space the
  * next line takes in place of the paragraph gap, as after a heading.
  */
final private case class Pending(opening: Opening, lines: Vector[SetLine], gapAfter: Option[Float])

private object Pending:
  val none: Pending = Pending(Opening.Continue, Vector.empty, None)

final private case class Flow(groups: Vector[Group], pending: Pending, sections: Int):

  def flush: Flow =
    if pending.lines.isEmpty then copy(pending = Pending.none)
    else copy(groups = groups :+ Group(pending.opening, pending.lines, pending.lines.size), pending = Pending.none)

  def open(opening: Opening, lines: Vector[SetLine], gapAfter: Option[Float]): Flow =
    flush.copy(pending = Pending(opening, lines, gapAfter))

  def hold(lines: Vector[SetLine]): Flow =
    copy(pending = pending.copy(lines = pending.lines ++ lines, gapAfter = None))

  def attach(lines: Vector[SetLine]): Flow =
    copy(groups = groups :+ Group(pending.opening, pending.lines ++ lines, pending.lines.size), pending = Pending.none)

private object Flow:
  val empty: Flow = Flow(Vector.empty, Pending.none, 0)

/** Turns a [[Manuscript]] into groups of set lines, ready for the [[PageFiller]]. It knows the manuscript conventions:
  * sections start pages a third of the way down, headings and scene breaks hold with the text that follows, and the
  * title page and dedication stand alone.
  */
final private[layout] class FlowBuilder(t: PageTypography, setter: ParagraphSetter, metrics: LineMetrics):

  private val pitch: Float = t.lineSpacing match
    case LineSpacing.Multiple(factor) => (metrics.height * factor).toFloat
    case LineSpacing.Exact(points)    => points

  private val singlePitch: Float = metrics.height

  private val drop: Float = (t.textHeight * t.chapterDrop).toFloat

  def groups(manuscript: Manuscript): Either[PaginationError, Vector[Group]] =
    for
      front <- manuscript.front.toVector.flatTraverse(frontGroups(manuscript.meta, _))
      body  <- manuscript.body.foldM(Flow.empty)(section)
      end   <- manuscript.endMarker.traverse(text => lines(text, Placement.centre, pitch, firstSpace = pitch))
    yield front ++ end.fold(body.flush)(body.attach).flush.groups

  private def frontGroups(meta: ManuscriptMeta, front: FrontMatter): Either[PaginationError, Vector[Group]] =
    val group = front match
      case FrontMatter.TitlePage => titlePage(meta)
      case FrontMatter.Dedication(text) =>
        lines(text, Placement.centre, pitch).map(standalone(PageKind.Dedication, drop, _))
    group.map(Vector(_))

  private def titlePage(meta: ManuscriptMeta): Either[PaginationError, Group] =
    for
      first   <- identityLine(meta)
      contact <- meta.contact.toVector.flatTraverse(lines(_, Placement.flush, singlePitch))
      title   <- lines(meta.title, Placement.centre, pitch, firstSpace = drop)
      byline <- Option
        .when(meta.byline.trim.nonEmpty)(s"by ${meta.byline}")
        .toVector
        .flatTraverse(lines(_, Placement.centre, pitch))
    yield standalone(PageKind.Title, 0f, first +: (contact ++ title ++ byline))

  /** The legal name at the left and the word count at the right, on one single-spaced line. */
  private def identityLine(meta: ManuscriptMeta): Either[PaginationError, SetLine] =
    setter.width(meta.wordCountLine).map { count =>
      val name  = Option.when(meta.author.legal.nonEmpty)(setter.run(meta.author.legal, t.margins.left))
      val words = setter.run(meta.wordCountLine, (t.margins.left + t.textWidth - count).toFloat)
      SetLine(name.toVector :+ words, 0f, singlePitch)
    }

  private def standalone(kind: PageKind, pageDrop: Float, content: Vector[SetLine]): Group =
    Group(Opening.NewPage(kind, pageDrop), content, content.size)

  private def section(flow: Flow, section: Section): Either[PaginationError, Flow] =
    section match
      case Section.Part(heading, nested) =>
        opening(flow, heading).flatMap(nested.foldM(_)(this.section))
      case Section.Chapter(heading, blocks) =>
        opening(flow, heading).flatMap(blocks.foldM(_)(block))

  private def opening(flow: Flow, heading: Option[SectionHeading]): Either[PaginationError, Flow] =
    val page = Opening.NewPage(PageKind.SectionStart, drop, Some(SectionRef(flow.sections)))
    heading.fold(Vector.empty[String])(_.lines).flatTraverse(lines(_, Placement.centre, pitch)).map { headingLines =>
      flow.open(page, headingLines, Option.when(headingLines.nonEmpty)(pitch)).copy(sections = flow.sections + 1)
    }

  private def block(flow: Flow, block: Block): Either[PaginationError, Flow] =
    val gap = flow.pending.gapAfter.getOrElse(t.paragraphGap)
    block match
      case Block.Paragraph(runs, kind) =>
        setter.set(runs, placementOf(kind)).map(spaced(_, pitch, gap)).map(attached(flow, _))
      case Block.SceneBreak =>
        lines(t.sceneBreak, Placement.centre, pitch, firstSpace = gap).map(flow.hold)
      case Block.Preformatted(code) =>
        code
          .flatTraverse(line =>
            setter.set(List(RichTextRun(ProseText.expandTabs(line, CodeTabWidth))), codePlacement).map(blankIfEmpty)
          )
          .map(spaced(_, pitch, gap))
          .map(attached(flow, _))

  private def attached(flow: Flow, content: Vector[SetLine]): Flow =
    if content.isEmpty then flow else flow.attach(content)

  private def placementOf(kind: ParagraphKind): Placement =
    kind match
      case ParagraphKind.Body       => Placement(t.firstLineIndent, 0f, centred = false)
      case ParagraphKind.BlockQuote => Placement(0f, t.firstLineIndent, centred = false)
      case ParagraphKind.Centered   => Placement.centre

  private val CodeTabWidth = 4

  private val codePlacement: Placement = Placement(0f, 0f, centred = false, anywhere = true)

  private def blankIfEmpty(set: Vector[Vector[PlacedRun]]): Vector[Vector[PlacedRun]] =
    if set.isEmpty then Vector(Vector.empty) else set

  private def lines(
    text: String,
    placement: Placement,
    height: Float,
    firstSpace: Float = 0f
  ): Either[PaginationError, Vector[SetLine]] =
    setter.set(List(RichTextRun(text)), placement).map(spaced(_, height, firstSpace))

  private def spaced(set: Vector[Vector[PlacedRun]], height: Float, firstSpace: Float): Vector[SetLine] =
    set.zipWithIndex.map((runs, index) => SetLine(runs, if index == 0 then firstSpace else 0f, height))
