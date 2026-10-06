package com.serenity.text.bidi

import com.ibm.icu.lang.UCharacter
import com.ibm.icu.lang.UCharacterEnums.ECharacterDirection
import com.ibm.icu.text.Bidi

enum TextDirection:
  /** The first strong character decides (UAX#9 P2/P3); left to right when there is none. */
  case Auto
  case LeftToRight
  case RightToLeft

/** UTF-16 range `[start, end)` of a paragraph at one embedding level; odd levels run right to left. */
final case class DirectionalRun(start: Int, end: Int, level: Byte):
  def isRightToLeft: Boolean = (level & 1) == 1

/** Embedding levels resolved once for a whole paragraph (one logical line), so a row cut out of it by wrapping keeps
  * the levels the paragraph gave it; resolving the row on its own would let neutrals at its edges settle differently.
  * Levels come from ICU so bidi classes share one Unicode version with the grapheme and line-break iterators.
  */
final class ParagraphBidi private (paragraph: String, levels: IArray[Byte], val paragraphLevel: Byte):

  /** The row's runs in visual order, left to right: UAX#9 L1 resets the row's trailing whitespace (and whitespace
    * before a tab) to the paragraph level, then L2 reverses runs from the highest level down to the lowest odd one.
    * Rule L1 is applied here rather than read from an ICU line object because ICU caches runs inside its mutable
    * `Bidi`, and this value is shared between rows and threads.
    */
  def visualRuns(rowStart: Int, rowEnd: Int): Vector[DirectionalRun] =
    val start = math.max(0, rowStart)
    val end   = math.min(rowEnd, levels.length)
    if start >= end then Vector.empty
    else reorder(levelRuns(start, rowLevels(start, end)))

  private def rowLevels(start: Int, end: Int): List[Byte] =
    @annotation.tailrec
    def loop(index: Int, resetting: Boolean, acc: List[Byte]): List[Byte] =
      if index < start then acc
      else
        val direction = UCharacter.getDirection(codePointCovering(index))
        if ParagraphBidi.isSeparator(direction) then loop(index - 1, true, paragraphLevel :: acc)
        else if resetting && ParagraphBidi.isWhitespaceLike(direction) then loop(index - 1, true, paragraphLevel :: acc)
        else loop(index - 1, false, levels(index) :: acc)
    loop(end - 1, true, Nil)

  private def codePointCovering(index: Int): Int =
    if index > 0 && Character.isLowSurrogate(paragraph.charAt(index)) &&
        Character.isHighSurrogate(paragraph.charAt(index - 1))
    then paragraph.codePointAt(index - 1)
    else paragraph.codePointAt(index)

  private def levelRuns(start: Int, rowLevels: List[Byte]): Vector[DirectionalRun] =
    @annotation.tailrec
    def loop(
      index: Int,
      remaining: List[Byte],
      current: Option[DirectionalRun],
      runs: Vector[DirectionalRun]
    ): Vector[DirectionalRun] =
      remaining match
        case Nil => runs ++ current
        case level :: rest =>
          current match
            case Some(run) if run.level == level => loop(index + 1, rest, Some(run.copy(end = index + 1)), runs)
            case _ => loop(index + 1, rest, Some(DirectionalRun(index, index + 1, level)), runs ++ current)
    loop(start, rowLevels, None, Vector.empty)

  private def reorder(runs: Vector[DirectionalRun]): Vector[DirectionalRun] =
    val levelsPresent = runs.map(_.level.toInt)
    val highest       = levelsPresent.maxOption.getOrElse(0)
    val lowest        = levelsPresent.minOption.getOrElse(0)
    val lowestOdd     = if lowest % 2 == 1 then lowest else lowest + 1
    (highest to lowestOdd by -1).foldLeft(runs)(reverseSequencesAtOrAbove)

  private def reverseSequencesAtOrAbove(runs: Vector[DirectionalRun], level: Int): Vector[DirectionalRun] =
    val (done, pending) = runs.foldLeft((Vector.empty[DirectionalRun], Vector.empty[DirectionalRun])) {
      case ((settled, sequence), run) =>
        if run.level >= level then (settled, sequence :+ run)
        else (settled ++ sequence.reverse :+ run, Vector.empty)
    }
    done ++ pending.reverse

object ParagraphBidi:

  private val FirstStrongIsolate    = ECharacterDirection.FIRST_STRONG_ISOLATE.toInt
  private val LeftToRightIsolate    = ECharacterDirection.LEFT_TO_RIGHT_ISOLATE.toInt
  private val RightToLeftIsolate    = ECharacterDirection.RIGHT_TO_LEFT_ISOLATE.toInt
  private val PopDirectionalIsolate = ECharacterDirection.POP_DIRECTIONAL_ISOLATE.toInt
  private val RightToLeft           = ECharacterDirection.RIGHT_TO_LEFT
  private val RightToLeftArabic     = ECharacterDirection.RIGHT_TO_LEFT_ARABIC
  private val ArabicNumber          = ECharacterDirection.ARABIC_NUMBER
  private val LeftToRightEmbedding  = ECharacterDirection.LEFT_TO_RIGHT_EMBEDDING
  private val LeftToRightOverride   = ECharacterDirection.LEFT_TO_RIGHT_OVERRIDE
  private val RightToLeftEmbedding  = ECharacterDirection.RIGHT_TO_LEFT_EMBEDDING
  private val RightToLeftOverride   = ECharacterDirection.RIGHT_TO_LEFT_OVERRIDE
  private val PopDirectionalFormat  = ECharacterDirection.POP_DIRECTIONAL_FORMAT
  private val WhiteSpaceNeutral     = ECharacterDirection.WHITE_SPACE_NEUTRAL
  private val BoundaryNeutral       = ECharacterDirection.BOUNDARY_NEUTRAL
  private val SegmentSeparator      = ECharacterDirection.SEGMENT_SEPARATOR
  private val BlockSeparator        = ECharacterDirection.BLOCK_SEPARATOR

  /** `None` when the paragraph is left to right and nothing in it can produce another level, which is the case for
    * Latin prose and code: that check walks codepoints without allocating, so those lines pay nothing for bidi.
    */
  def analyse(paragraph: String, direction: TextDirection): Option[ParagraphBidi] =
    if direction != TextDirection.RightToLeft && !needsResolution(paragraph, 0) then None
    else if paragraph.isEmpty then Some(new ParagraphBidi(paragraph, IArray.empty[Byte], 1))
    else
      val bidi = new Bidi(paragraph, icuDirection(direction))
      Some(new ParagraphBidi(paragraph, IArray.unsafeFromArray(bidi.getLevels), bidi.getParaLevel))

  private def icuDirection(direction: TextDirection): Int = direction match
    case TextDirection.Auto        => Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT
    case TextDirection.LeftToRight => Bidi.DIRECTION_LEFT_TO_RIGHT
    case TextDirection.RightToLeft => Bidi.DIRECTION_RIGHT_TO_LEFT

  @annotation.tailrec
  private def needsResolution(paragraph: String, index: Int): Boolean =
    if index >= paragraph.length then false
    else
      val codePoint = paragraph.codePointAt(index)
      introducesLevels(UCharacter.getDirection(codePoint)) ||
      needsResolution(paragraph, index + Character.charCount(codePoint))

  private def introducesLevels(direction: Int): Boolean = direction match
    case RightToLeft | RightToLeftArabic | ArabicNumber | LeftToRightEmbedding | LeftToRightOverride |
        RightToLeftEmbedding | RightToLeftOverride | PopDirectionalFormat | FirstStrongIsolate | LeftToRightIsolate |
        RightToLeftIsolate | PopDirectionalIsolate =>
      true
    case _ => false

  private def isSeparator(direction: Int): Boolean =
    direction == SegmentSeparator || direction == BlockSeparator

  /** Characters UAX#9 L1 resets along with trailing whitespace: isolates, and the explicit formatting and boundary
    * neutrals that rule X9 removed from resolution.
    */
  private def isWhitespaceLike(direction: Int): Boolean = direction match
    case WhiteSpaceNeutral | BoundaryNeutral | FirstStrongIsolate | LeftToRightIsolate | RightToLeftIsolate |
        PopDirectionalIsolate | LeftToRightEmbedding | LeftToRightOverride | RightToLeftEmbedding |
        RightToLeftOverride | PopDirectionalFormat =>
      true
    case _ => false
