package com.serenity.richtext

/** Inline text formatting that can be applied to rich text runs. */
enum InlineMark:
  case Bold
  case Italic
  case Underline

/** Paragraph-level horizontal alignment. */
enum ParagraphAlignment:
  case Left
  case Center
  case Right
  case Justify

/** Paragraph-level structural role used for document navigation and rich document round-tripping. */
enum ParagraphRole:
  case Body
  case Heading(level: Int)

/** Inline style for a contiguous text run. */
case class RichTextStyle(
    marks: Set[InlineMark] = Set.empty,
    fontFamily: Option[String] = None,
    fontSize: Option[Float] = None,
    color: Option[String] = None
):
  def withMark(mark: InlineMark): RichTextStyle =
    copy(marks = marks + mark)

  /** Return this style without the given inline mark. */
  def withoutMark(mark: InlineMark): RichTextStyle =
    copy(marks = marks - mark)

object RichTextStyle:
  val empty: RichTextStyle = RichTextStyle()

/** A contiguous span of text sharing the same inline style. */
case class RichTextRun(text: String, style: RichTextStyle = RichTextStyle.empty):
  def isEmpty: Boolean =
    text.isEmpty

/** Position inside a rich text document, measured as a UTF-16 offset within one paragraph. */
case class RichTextPosition(paragraphIndex: Int, offset: Int)

/** Half-open range inside a rich text document. */
case class RichTextRange(start: RichTextPosition, end: RichTextPosition):
  def normalized: RichTextRange =
    if startsBeforeOrAt(start, end) then this else RichTextRange(end, start)

  private def startsBeforeOrAt(left: RichTextPosition, right: RichTextPosition): Boolean =
    left.paragraphIndex < right.paragraphIndex ||
      (left.paragraphIndex == right.paragraphIndex && left.offset <= right.offset)

/** One paragraph of rich text with inline runs and paragraph formatting. */
case class RichTextParagraph(
    runs: List[RichTextRun],
    alignment: ParagraphAlignment = ParagraphAlignment.Left,
    role: ParagraphRole = ParagraphRole.Body
):
  def plainText: String =
    runs.map(_.text).mkString

  def normalized: RichTextParagraph =
    copy(runs = mergeRuns(runs.filterNot(_.isEmpty)))

  def applyMark(startOffset: Int, endOffset: Int, mark: InlineMark): RichTextParagraph =
    val start = startOffset.max(0).min(plainText.length)
    val end   = endOffset.max(start).min(plainText.length)
    if start == end then this
    else copy(runs = mergeRuns(splitAndTransform(start, end, _.withMark(mark))))

  /** Toggle a mark across a paragraph range, removing it only when every covered run already has it. */
  def toggleMark(startOffset: Int, endOffset: Int, mark: InlineMark): RichTextParagraph =
    val start = startOffset.max(0).min(plainText.length)
    val end   = endOffset.max(start).min(plainText.length)
    if start == end then this
    else setMark(start, end, mark, enabled = !hasMarkThroughout(start, end, mark))

  /** Set or clear a mark across a paragraph range. */
  def setMark(startOffset: Int, endOffset: Int, mark: InlineMark, enabled: Boolean): RichTextParagraph =
    val start = startOffset.max(0).min(plainText.length)
    val end   = endOffset.max(start).min(plainText.length)
    if start == end then this
    else
      val transform =
        if enabled then (style: RichTextStyle) => style.withMark(mark)
        else (style: RichTextStyle) => style.withoutMark(mark)
      copy(runs = mergeRuns(splitAndTransform(start, end, transform)))

  /** True when the non-empty paragraph range is fully covered by the given mark. */
  def hasMarkThroughout(startOffset: Int, endOffset: Int, mark: InlineMark): Boolean =
    val start = startOffset.max(0).min(plainText.length)
    val end   = endOffset.max(start).min(plainText.length)
    start < end && stylesInRange(start, end).forall(_.marks.contains(mark))

  private def splitAndTransform(
    startOffset: Int,
    endOffset: Int,
    transform: RichTextStyle => RichTextStyle
  ): List[RichTextRun] =
    runs
      .foldLeft((0, List.empty[RichTextRun])) {
        case ((currentOffset, acc), run) =>
          val runStart   = currentOffset
          val runEnd     = currentOffset + run.text.length
          val nextOffset = runEnd

          if runEnd <= startOffset || runStart >= endOffset then (nextOffset, acc :+ run)
          else
            val localStart = (startOffset - runStart).max(0).min(run.text.length)
            val localEnd   = (endOffset - runStart).max(localStart).min(run.text.length)
            val before     = run.text.take(localStart)
            val middle     = run.text.slice(localStart, localEnd)
            val after      = run.text.drop(localEnd)
            val splitRuns = List(
              Option.when(before.nonEmpty)(RichTextRun(before, run.style)),
              Option.when(middle.nonEmpty)(RichTextRun(middle, transform(run.style))),
              Option.when(after.nonEmpty)(RichTextRun(after, run.style))
            ).flatten
            (nextOffset, acc ++ splitRuns)
      }
      ._2

  private def stylesInRange(startOffset: Int, endOffset: Int): List[RichTextStyle] =
    runs
      .foldLeft((0, List.empty[RichTextStyle])) {
        case ((currentOffset, acc), run) =>
          val runStart   = currentOffset
          val runEnd     = currentOffset + run.text.length
          val nextOffset = runEnd
          if runEnd <= startOffset || runStart >= endOffset then (nextOffset, acc)
          else (nextOffset, run.style :: acc)
      }
      ._2

  private def mergeRuns(input: List[RichTextRun]): List[RichTextRun] =
    input.foldRight(List.empty[RichTextRun]) {
      case (run, next :: tail) if run.style == next.style =>
        run.copy(text = run.text + next.text) :: tail
      case (run, acc) =>
        run :: acc
    }

object RichTextParagraph:

  def plain(
    text: String,
    alignment: ParagraphAlignment = ParagraphAlignment.Left,
    role: ParagraphRole = ParagraphRole.Body
  ): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(text)), alignment, role)

/** Rich text document model for document-format adapters and future rich editing surfaces. */
case class RichTextDocument(paragraphs: List[RichTextParagraph]):
  def plainText: String =
    paragraphs.map(_.plainText).mkString("\n")

  def normalized: RichTextDocument =
    copy(paragraphs = paragraphs.map(_.normalized))

  def applyMark(range: RichTextRange, mark: InlineMark): RichTextDocument =
    val normalizedRange = range.normalized
    copy(paragraphs = paragraphs.zipWithIndex.map {
      case (paragraph, index)
          if index < normalizedRange.start.paragraphIndex || index > normalizedRange.end.paragraphIndex =>
        paragraph
      case (paragraph, index) =>
        val startOffset =
          if index == normalizedRange.start.paragraphIndex then normalizedRange.start.offset else 0
        val endOffset =
          if index == normalizedRange.end.paragraphIndex then normalizedRange.end.offset else paragraph.plainText.length
        paragraph.applyMark(startOffset, endOffset, mark)
    })

  /** Toggle a mark across a document range, using one add/remove decision for the whole range. */
  def toggleMark(range: RichTextRange, mark: InlineMark): RichTextDocument =
    val normalizedRange = range.normalized
    val shouldRemove = paragraphs.zipWithIndex
      .filter {
        case (_, index) =>
          index >= normalizedRange.start.paragraphIndex && index <= normalizedRange.end.paragraphIndex
      }
      .forall {
        case (paragraph, index) =>
          val startOffset =
            if index == normalizedRange.start.paragraphIndex then normalizedRange.start.offset else 0
          val endOffset =
            if index == normalizedRange.end.paragraphIndex then normalizedRange.end.offset
            else paragraph.plainText.length
          paragraph.hasMarkThroughout(startOffset, endOffset, mark)
      }
    copy(paragraphs = paragraphs.zipWithIndex.map {
      case (paragraph, index)
          if index < normalizedRange.start.paragraphIndex || index > normalizedRange.end.paragraphIndex =>
        paragraph
      case (paragraph, index) =>
        val startOffset =
          if index == normalizedRange.start.paragraphIndex then normalizedRange.start.offset else 0
        val endOffset =
          if index == normalizedRange.end.paragraphIndex then normalizedRange.end.offset else paragraph.plainText.length
        paragraph.setMark(startOffset, endOffset, mark, enabled = !shouldRemove)
    })

  /** True when the rich document still represents the provided plain text exactly. */
  def matchesPlainText(text: String): Boolean =
    plainText == text

object RichTextDocument:
  def oneParagraph(text: String): RichTextDocument =
    RichTextDocument(List(RichTextParagraph.plain(text)))

  /** Build a plain rich text document from newline-separated text. */
  def fromPlainText(text: String): RichTextDocument =
    RichTextDocument(text.split("\n", -1).toList.map(line => RichTextParagraph.plain(line)))
