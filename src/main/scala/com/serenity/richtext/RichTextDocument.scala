package com.serenity.richtext

/** Inline text formatting that can be applied to rich text runs. */
enum InlineMark:
  case Bold
  case Italic
  case Underline

/** Content that occupies one character of paragraph text without being text. The character in the rope is
  * [[RichTextRun.AtomCharacter]]; the payload lives on the run, so undo and copy carry it with the text.
  */
enum InlineAtom:
  /** A line break inside a paragraph (`w:br`, `text:line-break`, RTF `\line`). It must not be a rope `'\n'`: rope lines
    * are paragraphs, and every rich-text layer reads paragraph i as rope line i.
    */
  case SoftBreak

enum ParagraphAlignment:
  case Left
  case Center
  case Right
  case Justify

/** Paragraph-level structural role used for document navigation and rich document round-tripping. */
enum ParagraphRole:
  case Body
  case Heading(level: Int)

  /** A printed-book-style drop cap: the paragraph's first character renders as a large glyph spanning `lines` visual
    * lines, with body text wrapping in beside it. `lines` is clamped to at least 1 by [[ParagraphRole.dropCap]].
    */
  case DropCap(lines: Int)

object ParagraphRole:
  /** The conventional drop cap span used by the `paragraph-drop-cap` command and by codecs that only track a boolean
    * "has a drop cap" flag (DOCX/ODT round-trip the exact line count; this is the default when creating one fresh).
    */
  val DefaultDropCapLines: Int = 3

  /** Smart constructor clamping the span to at least one line -- a zero/negative span would collapse the multi-line
    * layout reservation to nothing while still tagging the paragraph as a drop cap.
    */
  def dropCap(lines: Int = DefaultDropCapLines): ParagraphRole =
    DropCap(lines.max(1))

/** Inline style for a contiguous text run. */
final case class RichTextStyle(
    marks: Set[InlineMark] = Set.empty,
    fontFamily: Option[String] = None,
    fontSize: Option[Float] = None,
    color: Option[String] = None,
    link: Option[String] = None
):
  def withMark(mark: InlineMark): RichTextStyle =
    copy(marks = marks + mark)

  def withoutMark(mark: InlineMark): RichTextStyle =
    copy(marks = marks - mark)

  def withFontFamily(family: String): RichTextStyle =
    copy(fontFamily = Some(family.trim).filter(_.nonEmpty))

  def withFontSize(size: Float): RichTextStyle =
    copy(fontSize = Some(size.max(1.0f)))

  def withColor(color: String): RichTextStyle =
    copy(color = Some(color.trim).filter(_.nonEmpty))

  /** `target` is an external URL, or `#name` for an anchor inside the document. */
  def withLink(target: String): RichTextStyle =
    copy(link = Some(target.trim).filter(_.nonEmpty))

  /** The style with its link removed: what a format that stores links outside character formatting styles by. */
  def withoutLink: RichTextStyle =
    copy(link = None)

object RichTextStyle:
  val empty: RichTextStyle = RichTextStyle()

/** A contiguous span of text sharing the same inline style. A run carrying an [[InlineAtom]] is exactly one
  * [[RichTextRun.AtomCharacter]] and never merges with its neighbours.
  */
final case class RichTextRun(
    text: String,
    style: RichTextStyle = RichTextStyle.empty,
    atom: Option[InlineAtom] = None
):
  def isEmpty: Boolean =
    text.isEmpty

  def mergesWith(next: RichTextRun): Boolean =
    atom.isEmpty && next.atom.isEmpty && style == next.style

  /** The text a plain-text or Markdown export shows for this run. */
  def exportText: String =
    atom.fold(text) { case InlineAtom.SoftBreak => "\n" }

object RichTextRun:
  /** U+FFFC OBJECT REPLACEMENT CHARACTER: the single rope character standing for an [[InlineAtom]]. */
  val AtomCharacter: Char = '\uFFFC'

  def softBreak(style: RichTextStyle = RichTextStyle.empty): RichTextRun =
    RichTextRun(AtomCharacter.toString, style, Some(InlineAtom.SoftBreak))

/** Position inside a rich text document, measured as a UTF-16 offset within one paragraph. */
final case class RichTextPosition(paragraphIndex: Int, offset: Int)

/** Half-open range inside a rich text document. */
final case class RichTextRange(start: RichTextPosition, end: RichTextPosition):
  def normalized: RichTextRange =
    if startsBeforeOrAt(start, end) then this else RichTextRange(end, start)

  private def startsBeforeOrAt(left: RichTextPosition, right: RichTextPosition): Boolean =
    left.paragraphIndex < right.paragraphIndex ||
      (left.paragraphIndex == right.paragraphIndex && left.offset <= right.offset)

/** One paragraph of rich text with inline runs and paragraph formatting. */
final case class RichTextParagraph(
    runs: List[RichTextRun],
    alignment: ParagraphAlignment = ParagraphAlignment.Left,
    role: ParagraphRole = ParagraphRole.Body
):
  lazy val plainText: String =
    runs.map(_.text).mkString

  lazy val plainTextLength: Int =
    runs.foldLeft(0)(_ + _.text.length)

  def exportText: String =
    runs.map(_.exportText).mkString

  /** Consecutive runs grouped by link target, in order, for formats that wrap linked text in a container element. */
  def linkSpans: List[(Option[String], List[RichTextRun])] =
    runs.foldRight(List.empty[(Option[String], List[RichTextRun])]) {
      case (run, (target, spanRuns) :: tail) if target == run.style.link => (target, run :: spanRuns) :: tail
      case (run, acc)                                                    => (run.style.link, List(run)) :: acc
    }

  /** True when no run is empty and no two adjacent runs share a style -- the fixed point of [[normalized]]. */
  def isNormalized: Boolean =
    runs.forall(!_.isEmpty) && runs.zip(runs.drop(1)).forall((left, right) => !left.mergesWith(right))

  def normalized: RichTextParagraph =
    if isNormalized then this else copy(runs = mergeRuns(runs.filterNot(_.isEmpty)))

  def applyMark(startOffset: Int, endOffset: Int, mark: InlineMark): RichTextParagraph =
    val start = startOffset.max(0).min(plainTextLength)
    val end   = endOffset.max(start).min(plainTextLength)
    if start == end then this
    else copy(runs = mergeRuns(splitAndTransform(start, end, _.withMark(mark))))

  /** Toggle a mark across a paragraph range, removing it only when every covered run already has it. */
  def toggleMark(startOffset: Int, endOffset: Int, mark: InlineMark): RichTextParagraph =
    val start = startOffset.max(0).min(plainTextLength)
    val end   = endOffset.max(start).min(plainTextLength)
    if start == end then this
    else setMark(start, end, mark, enabled = !hasMarkThroughout(start, end, mark))

  /** Set or clear a mark across a paragraph range. */
  def setMark(startOffset: Int, endOffset: Int, mark: InlineMark, enabled: Boolean): RichTextParagraph =
    val start = startOffset.max(0).min(plainTextLength)
    val end   = endOffset.max(start).min(plainTextLength)
    if start == end then this
    else
      val transform =
        if enabled then (style: RichTextStyle) => style.withMark(mark)
        else (style: RichTextStyle) => style.withoutMark(mark)
      copy(runs = mergeRuns(splitAndTransform(start, end, transform)))

  /** Replace text inside this paragraph, preserving surrounding inline styles. */
  def replaceRange(startOffset: Int, endOffset: Int, insertedText: String): RichTextParagraph =
    val start = startOffset.max(0).min(plainTextLength)
    val end   = endOffset.max(start).min(plainTextLength)
    val insertedRun = Option
      .when(insertedText.nonEmpty)(RichTextRun(insertedText, styleAtInsertion(start, end)))
    copy(runs = mergeRuns(runsInRange(0, start) ++ insertedRun.toList ++ runsInRange(end, plainTextLength)))

  /** Transform inline style across a paragraph range. */
  def updateStyle(startOffset: Int, endOffset: Int)(transform: RichTextStyle => RichTextStyle): RichTextParagraph =
    val start = startOffset.max(0).min(plainTextLength)
    val end   = endOffset.max(start).min(plainTextLength)
    if start == end then this
    else copy(runs = mergeRuns(splitAndTransform(start, end, transform)))

  /** True when the non-empty paragraph range is fully covered by the given mark. */
  def hasMarkThroughout(startOffset: Int, endOffset: Int, mark: InlineMark): Boolean =
    val start = startOffset.max(0).min(plainTextLength)
    val end   = endOffset.max(start).min(plainTextLength)
    start < end && stylesInRange(start, end).forall(_.marks.contains(mark))

  private def splitAndTransform(
    startOffset: Int,
    endOffset: Int,
    transform: RichTextStyle => RichTextStyle
  ): List[RichTextRun] =
    runsWithStartOffsets.flatMap {
      case (run, runStart) =>
        val runEnd = runStart + run.text.length
        if runEnd <= startOffset || runStart >= endOffset then List(run)
        else
          val localStart = (startOffset - runStart).max(0).min(run.text.length)
          val localEnd   = (endOffset - runStart).max(localStart).min(run.text.length)
          val before     = run.text.take(localStart)
          val middle     = run.text.slice(localStart, localEnd)
          val after      = run.text.drop(localEnd)
          List(
            Option.when(before.nonEmpty)(run.copy(text = before)),
            Option.when(middle.nonEmpty)(run.copy(text = middle, style = transform(run.style))),
            Option.when(after.nonEmpty)(run.copy(text = after))
          ).flatten
    }

  private[richtext] def runsInRange(startOffset: Int, endOffset: Int): List[RichTextRun] =
    runsWithStartOffsets.flatMap {
      case (run, runStart) =>
        val runEnd = runStart + run.text.length
        if runEnd <= startOffset || runStart >= endOffset then Nil
        else
          val localStart = (startOffset - runStart).max(0).min(run.text.length)
          val localEnd   = (endOffset - runStart).max(localStart).min(run.text.length)
          val text       = run.text.slice(localStart, localEnd)
          Option.when(text.nonEmpty)(run.copy(text = text)).toList
    }

  private def runsWithStartOffsets: List[(RichTextRun, Int)] =
    runs.zip(runs.scanLeft(0)((offset, run) => offset + run.text.length))

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
    else withLinkOnlyBetweenLinkedText(startOffset, styleBeforeCaret(startOffset))

  /** Typing continues a link only from inside it, not from its edges, matching word processors. */
  private def withLinkOnlyBetweenLinkedText(offset: Int, style: RichTextStyle): RichTextStyle =
    val linkContinues =
      offset > 0 &&
        styleOfCharacter(offset - 1).exists(_.link == style.link) &&
        styleOfCharacter(offset).exists(_.link == style.link)
    if style.link.isEmpty || linkContinues then style else style.withoutLink

  private def styleOfCharacter(index: Int): Option[RichTextStyle] =
    runsWithStartOffsets.collectFirst {
      case (run, runStart) if runStart <= index && index < runStart + run.text.length => run.style
    }

  private def styleBeforeCaret(startOffset: Int): RichTextStyle =
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
      case (run, next :: tail) if run.mergesWith(next) =>
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

/** Rich text document model for document-format adapters and future rich editing surfaces.
  *
  * Backed by a [[ParagraphTree]] rather than storing `paragraphs` directly (#1663): paragraph lookup and range edits go
  * through the tree's `O(log n)` navigation, and only the paragraphs a given edit actually touches are reallocated --
  * the rest of the tree is shared by reference with the previous document. [[paragraphs]] itself stays available as a
  * `List[RichTextParagraph]` for callers (codecs, tests, rendering) that want the flat sequence; flattening it is
  * `O(n)` and happens at most once per document, memoized in the `lazy val`.
  *
  * Equality and hashing are defined over [[paragraphs]] (the flattened content), not the tree's internal shape -- two
  * documents holding the same paragraphs in the same order are equal regardless of which tree shape their respective
  * edit histories produced.
  *
  * `everyParagraphNormalized` records whether [[normalized]] would be a no-op (#1870). Edits re-check only the
  * paragraphs they produce, so an edit to an already-normalised document keeps that fact without a whole-document pass,
  * and [[normalized]] -- which the edit path calls after every keystroke -- returns `this` instead of rebuilding every
  * paragraph and tree node.
  */
final class RichTextDocument private (private val tree: ParagraphTree, everyParagraphNormalized: Boolean):
  lazy val paragraphs: List[RichTextParagraph] = tree.toParagraphs

  /** Whether this document carries any formatting a plain-text or Markdown save would discard: a non-body paragraph
    * role/alignment, or any run styled beyond [[RichTextStyle.empty]]. Used to decide whether saving at the
    * currently-typed extension should warn about lost formatting (issue #1253).
    */
  def hasFormatting: Boolean =
    tree.existsAny { paragraph =>
      paragraph.alignment != ParagraphAlignment.Left ||
      paragraph.role != ParagraphRole.Body ||
      paragraph.runs.exists(run => run.style != RichTextStyle.empty || run.atom.nonEmpty)
    }

  /** Returns a paragraph by line index via `O(log n)` tree descent instead of a full-document index build. */
  def paragraphAt(index: Int): Option[RichTextParagraph] =
    tree.paragraphAt(index)

  def plainText: String =
    paragraphs.map(_.plainText).mkString("\n")

  /** Plain text for formats with no inline atoms: each atom becomes the text it stands for, so a soft break is `'\n'`.
    */
  def exportText: String =
    paragraphs.map(_.exportText).mkString("\n")

  /** `O(1)`: the tree already tracks total character count and paragraph count at the root. */
  def plainTextLength: Int =
    if tree.paragraphCount == 0 then 0
    else tree.charCount + tree.paragraphCount - 1

  def normalized: RichTextDocument =
    if everyParagraphNormalized then this
    else new RichTextDocument(tree.mapAll(_.normalized), everyParagraphNormalized = true)

  def applyMark(range: RichTextRange, mark: InlineMark): RichTextDocument =
    updateRange(range)((paragraph, start, end) => paragraph.applyMark(start, end, mark))

  /** Toggle a mark across a document range, using one add/remove decision for the whole range. */
  def toggleMark(range: RichTextRange, mark: InlineMark): RichTextDocument =
    val normalizedRange = range.normalized
    val shouldRemove = tree.forallInRange(normalizedRange.start.paragraphIndex, normalizedRange.end.paragraphIndex) {
      (paragraph, index) =>
        val startOffset =
          if index == normalizedRange.start.paragraphIndex then normalizedRange.start.offset else 0
        val endOffset =
          if index == normalizedRange.end.paragraphIndex then normalizedRange.end.offset
          else paragraph.plainTextLength
        paragraph.hasMarkThroughout(startOffset, endOffset, mark)
    }
    updateRange(range)((paragraph, start, end) => paragraph.setMark(start, end, mark, enabled = !shouldRemove))

  /** Set the font family for every inline run touched by the range. */
  def setFontFamily(range: RichTextRange, family: String): RichTextDocument =
    updateInlineStyle(range)(_.withFontFamily(family))

  /** Set the font size for every inline run touched by the range. */
  def setFontSize(range: RichTextRange, size: Float): RichTextDocument =
    updateInlineStyle(range)(_.withFontSize(size))

  /** Nudge the font size of every inline run touched by the range by `deltaPt`, treating an un-sized (body) run as
    * `defaultSize`. Used by the increase/decrease authored-size commands.
    */
  def adjustFontSize(range: RichTextRange, deltaPt: Float, defaultSize: Float): RichTextDocument =
    updateInlineStyle(range)(style => style.withFontSize(style.fontSize.getOrElse(defaultSize) + deltaPt))

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
      val index = normalizedRange.start.paragraphIndex
      withUpdatedParagraphs(index, index) { (paragraph, _) =>
        paragraph.replaceRange(normalizedRange.start.offset, normalizedRange.end.offset, insertedText)
      }.normalized
    else replaceAcrossParagraphs(normalizedRange, insertedText)

  /** True when the rich document still represents the provided plain text exactly. */
  def matchesPlainText(text: String): Boolean =
    plainText == text

  /** Fast shape check for hot render paths: both sides are `O(1)` root-level tree aggregates. Exact content validation
    * stays at load/save/edit boundaries via [[matchesPlainText]].
    */
  def matchesPlainTextShape(lineCount: Int, textLength: Int): Boolean =
    tree.paragraphCount == lineCount && plainTextLength == textLength

  /** Applies `transform` to every paragraph in `[range.start.paragraphIndex, range.end.paragraphIndex]` (unclamped --
    * an out-of-bounds end paragraph index leaves every later paragraph's own length as its end offset), giving
    * `transform` each paragraph's local start/end offset for its slice of the range.
    */
  private def updateRange(
    range: RichTextRange
  )(transform: (RichTextParagraph, Int, Int) => RichTextParagraph): RichTextDocument =
    val normalizedRange = range.normalized
    withUpdatedParagraphs(normalizedRange.start.paragraphIndex, normalizedRange.end.paragraphIndex) {
      (paragraph, index) =>
        val startOffset =
          if index == normalizedRange.start.paragraphIndex then normalizedRange.start.offset else 0
        val endOffset =
          if index == normalizedRange.end.paragraphIndex then normalizedRange.end.offset else paragraph.plainTextLength
        transform(paragraph, startOffset, endOffset)
    }

  private def withUpdatedParagraphs(startIndex: Int, endIndex: Int)(
    update: (RichTextParagraph, Int) => RichTextParagraph
  ): RichTextDocument =
    val newTree = tree.updatedRange(startIndex, endIndex)(update)
    new RichTextDocument(
      newTree,
      everyParagraphNormalized && newTree.forallInRange(startIndex, endIndex)((paragraph, _) => paragraph.isNormalized)
    )

  private def updateParagraphs(range: RichTextRange)(update: RichTextParagraph => RichTextParagraph): RichTextDocument =
    if tree.paragraphCount == 0 then this
    else
      val normalizedRange = range.normalized
      val lastIndex       = tree.paragraphCount - 1
      val startIndex      = normalizedRange.start.paragraphIndex.max(0).min(lastIndex)
      val endIndex        = normalizedRange.end.paragraphIndex.max(startIndex).min(lastIndex)
      withUpdatedParagraphs(startIndex, endIndex)((paragraph, _) => update(paragraph))

  private[serenity] def updateInlineStyle(range: RichTextRange)(
    transform: RichTextStyle => RichTextStyle
  ): RichTextDocument =
    updateRange(range)((paragraph, start, end) => paragraph.updateStyle(start, end)(transform))

  private def replaceAcrossParagraphs(range: RichTextRange, insertedText: String): RichTextDocument =
    if tree.paragraphCount == 0 then this
    else
      val lastIndex  = tree.paragraphCount - 1
      val startIndex = range.start.paragraphIndex.max(0).min(lastIndex)
      val endIndex   = range.end.paragraphIndex.max(startIndex).min(lastIndex)
      (tree.paragraphAt(startIndex), tree.paragraphAt(endIndex)) match
        case (Some(start), Some(end)) =>
          val startOffset = range.start.offset.max(0).min(start.plainTextLength)
          val endOffset   = range.end.offset.max(0).min(end.plainTextLength)
          val style       = start.styleAtInsertion(startOffset, startOffset)
          val parts       = insertedText.split("\n", -1).toList
          val prefix      = start.runsInRange(0, startOffset)
          val suffix      = end.runsInRange(endOffset, end.plainTextLength)
          val replacement = parts match
            case text :: Nil =>
              List(
                RichTextParagraph(prefix ++ styledRun(text, style) ++ suffix, start.alignment, start.role).normalized
              )
            case first :: rest =>
              val middle =
                rest.dropRight(1).map(text => RichTextParagraph(styledRun(text, style), start.alignment, start.role))
              val last = rest.lastOption.toList.map(text =>
                RichTextParagraph(styledRun(text, style) ++ suffix, end.alignment, end.role).normalized
              )
              RichTextParagraph(prefix ++ styledRun(first, style), start.alignment, start.role).normalized ::
                middle ++ last
            case Nil => Nil
          new RichTextDocument(
            tree.replaceSlice(startIndex, endIndex, replacement),
            everyParagraphNormalized && replacement.forall(_.isNormalized)
          )
        case _ => this

  private def styledRun(text: String, style: RichTextStyle): List[RichTextRun] =
    Option.when(text.nonEmpty)(RichTextRun(text, style)).toList

  override def equals(other: Any): Boolean =
    other match
      case that: RichTextDocument => this.paragraphs == that.paragraphs
      case _                      => false

  override def hashCode(): Int =
    paragraphs.hashCode()

  override def toString: String =
    s"RichTextDocument($paragraphs)"

object RichTextDocument:
  def apply(paragraphs: List[RichTextParagraph]): RichTextDocument =
    new RichTextDocument(ParagraphTree.fromParagraphs(paragraphs), paragraphs.forall(_.isNormalized))

  def unapply(document: RichTextDocument): Some[List[RichTextParagraph]] =
    Some(document.paragraphs)

  def oneParagraph(text: String): RichTextDocument =
    RichTextDocument(List(RichTextParagraph.plain(text)))

  /** Build a plain rich text document from newline-separated text. */
  def fromPlainText(text: String): RichTextDocument =
    RichTextDocument(text.split("\n", -1).toList.map(line => RichTextParagraph.plain(line)))
