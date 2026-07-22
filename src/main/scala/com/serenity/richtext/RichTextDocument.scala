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

  /** Return this style with the given font family metadata. */
  def withFontFamily(family: String): RichTextStyle =
    copy(fontFamily = Some(family.trim).filter(_.nonEmpty))

  /** Return this style with the given font size metadata. */
  def withFontSize(size: Float): RichTextStyle =
    copy(fontSize = Some(size.max(1.0f)))

  /** Return this style with the given text colour metadata. */
  def withColor(color: String): RichTextStyle =
    copy(color = Some(color.trim).filter(_.nonEmpty))

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

  /** Replace text inside this paragraph, preserving surrounding inline styles. */
  def replaceRange(startOffset: Int, endOffset: Int, insertedText: String): RichTextParagraph =
    val start = startOffset.max(0).min(plainText.length)
    val end   = endOffset.max(start).min(plainText.length)
    val insertedRun = Option
      .when(insertedText.nonEmpty)(RichTextRun(insertedText, styleAtInsertion(start, end)))
    copy(runs = mergeRuns(runsInRange(0, start) ++ insertedRun.toList ++ runsInRange(end, plainText.length)))

  /** Transform inline style across a paragraph range. */
  def updateStyle(startOffset: Int, endOffset: Int)(transform: RichTextStyle => RichTextStyle): RichTextParagraph =
    val start = startOffset.max(0).min(plainText.length)
    val end   = endOffset.max(start).min(plainText.length)
    if start == end then this
    else copy(runs = mergeRuns(splitAndTransform(start, end, transform)))

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

  private[richtext] def runsInRange(startOffset: Int, endOffset: Int): List[RichTextRun] =
    runs
      .foldLeft((0, List.empty[RichTextRun])) {
        case ((currentOffset, acc), run) =>
          val runStart   = currentOffset
          val runEnd     = currentOffset + run.text.length
          val nextOffset = runEnd
          if runEnd <= startOffset || runStart >= endOffset then (nextOffset, acc)
          else
            val localStart = (startOffset - runStart).max(0).min(run.text.length)
            val localEnd   = (endOffset - runStart).max(localStart).min(run.text.length)
            val text       = run.text.slice(localStart, localEnd)
            (nextOffset, acc ++ Option.when(text.nonEmpty)(RichTextRun(text, run.style)).toList)
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

  private[richtext] def styleAtInsertion(startOffset: Int, endOffset: Int): RichTextStyle =
    if startOffset < endOffset then
      stylesInRange(startOffset, endOffset).reverse.headOption.getOrElse(RichTextStyle.empty)
    else
      runs
        .foldLeft((0, Option.empty[RichTextStyle])) {
          case ((currentOffset, found), run) =>
            val runStart   = currentOffset
            val runEnd     = currentOffset + run.text.length
            val nextOffset = runEnd
            val containsOffset =
              (runStart < startOffset && startOffset <= runEnd) ||
                (startOffset == 0 && runStart == 0)
            (nextOffset, found.orElse(Option.when(containsOffset)(run.style)))
        }
        ._2
        .getOrElse(RichTextStyle.empty)

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
  private lazy val indexedParagraphs: Vector[RichTextParagraph] =
    paragraphs.toVector

  /** Returns a paragraph by line index without repeatedly traversing the document list. */
  def paragraphAt(index: Int): Option[RichTextParagraph] =
    indexedParagraphs.lift(index)

  def plainText: String =
    paragraphs.map(_.plainText).mkString("\n")

  def plainTextLength: Int =
    if paragraphs.isEmpty then 0
    else paragraphs.map(_.plainText.length).sum + paragraphs.length - 1

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

  /** Set the font family for every inline run touched by the range. */
  def setFontFamily(range: RichTextRange, family: String): RichTextDocument =
    updateInlineStyle(range)(_.withFontFamily(family))

  /** Set the font size for every inline run touched by the range. */
  def setFontSize(range: RichTextRange, size: Float): RichTextDocument =
    updateInlineStyle(range)(_.withFontSize(size))

  /** Set the text colour for every inline run touched by the range. */
  def setColor(range: RichTextRange, color: String): RichTextDocument =
    updateInlineStyle(range)(_.withColor(color))

  /** Set the structural role for every paragraph touched by the range. */
  def setParagraphRole(range: RichTextRange, role: ParagraphRole): RichTextDocument =
    updateParagraphs(range)(_.copy(role = role))

  /** Set the alignment for every paragraph touched by the range. */
  def setParagraphAlignment(range: RichTextRange, alignment: ParagraphAlignment): RichTextDocument =
    updateParagraphs(range)(_.copy(alignment = alignment))

  /** Replace a document range while preserving inline styles for simple same-paragraph edits. */
  def replaceRange(range: RichTextRange, insertedText: String): RichTextDocument =
    val normalizedRange = range.normalized
    if normalizedRange.start.paragraphIndex == normalizedRange.end.paragraphIndex && !insertedText.contains('\n') then
      copy(paragraphs = paragraphs.zipWithIndex.map {
        case (paragraph, index) if index == normalizedRange.start.paragraphIndex =>
          paragraph.replaceRange(normalizedRange.start.offset, normalizedRange.end.offset, insertedText)
        case (paragraph, _) => paragraph
      }).normalized
    else replaceAcrossParagraphs(normalizedRange, insertedText)

  /** True when the rich document still represents the provided plain text exactly. */
  def matchesPlainText(text: String): Boolean =
    plainText == text

  /** Fast shape check for hot render paths. Exact content validation stays at load/save/edit boundaries. */
  def matchesPlainTextShape(lineCount: Int, textLength: Int): Boolean =
    paragraphs.length == lineCount && plainTextLength == textLength

  private def updateParagraphs(range: RichTextRange)(update: RichTextParagraph => RichTextParagraph): RichTextDocument =
    if paragraphs.isEmpty then this
    else
      val normalizedRange = range.normalized
      val lastIndex       = paragraphs.length - 1
      val startIndex      = normalizedRange.start.paragraphIndex.max(0).min(lastIndex)
      val endIndex        = normalizedRange.end.paragraphIndex.max(startIndex).min(lastIndex)
      copy(paragraphs = paragraphs.zipWithIndex.map {
        case (paragraph, index) if index >= startIndex && index <= endIndex => update(paragraph)
        case (paragraph, _)                                                 => paragraph
      })

  private[serenity] def updateInlineStyle(range: RichTextRange)(
    transform: RichTextStyle => RichTextStyle
  ): RichTextDocument =
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
        paragraph.updateStyle(startOffset, endOffset)(transform)
    })

  private def replaceAcrossParagraphs(range: RichTextRange, insertedText: String): RichTextDocument =
    if paragraphs.isEmpty then this
    else
      val lastIndex   = paragraphs.length - 1
      val startIndex  = range.start.paragraphIndex.max(0).min(lastIndex)
      val endIndex    = range.end.paragraphIndex.max(startIndex).min(lastIndex)
      val start       = paragraphs(startIndex)
      val end         = paragraphs(endIndex)
      val startOffset = range.start.offset.max(0).min(start.plainText.length)
      val endOffset   = range.end.offset.max(0).min(end.plainText.length)
      val style       = start.styleAtInsertion(startOffset, startOffset)
      val parts       = insertedText.split("\n", -1).toList
      val prefix      = start.runsInRange(0, startOffset)
      val suffix      = end.runsInRange(endOffset, end.plainText.length)
      val replacement = parts match
        case text :: Nil =>
          List(RichTextParagraph(prefix ++ styledRun(text, style) ++ suffix, start.alignment, start.role).normalized)
        case first :: rest =>
          val middle =
            rest.dropRight(1).map(text => RichTextParagraph(styledRun(text, style), start.alignment, start.role))
          val last = rest.lastOption.toList.map(text =>
            RichTextParagraph(styledRun(text, style) ++ suffix, end.alignment, end.role).normalized
          )
          RichTextParagraph(prefix ++ styledRun(first, style), start.alignment, start.role).normalized :: middle ++ last
        case Nil => Nil
      copy(paragraphs = paragraphs.take(startIndex) ++ replacement ++ paragraphs.drop(endIndex + 1))

  private def styledRun(text: String, style: RichTextStyle): List[RichTextRun] =
    Option.when(text.nonEmpty)(RichTextRun(text, style)).toList

object RichTextDocument:
  def oneParagraph(text: String): RichTextDocument =
    RichTextDocument(List(RichTextParagraph.plain(text)))

  /** Build a plain rich text document from newline-separated text. */
  def fromPlainText(text: String): RichTextDocument =
    RichTextDocument(text.split("\n", -1).toList.map(line => RichTextParagraph.plain(line)))
