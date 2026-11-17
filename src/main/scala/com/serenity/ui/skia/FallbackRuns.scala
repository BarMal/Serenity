package com.serenity.ui.skia

import org.jetbrains.skia.Font

/** Font runs for text with symbols or CJK characters the base font may lack (box drawing, arrows, general punctuation,
  * the CJK symbols, kana, ideographs and fullwidth forms), which Skiko's `TextLine` would shape for 4 to 8 ms a call
  * whatever the text. Shaping the runs ourselves, as the Latin path does, takes a fraction of a millisecond. The runs
  * are `TextLine`'s own: a space or a punctuation mark stays in the font of the run before it while that font has the
  * glyph, any other character prefers the base font, then the last fallback font, then the font manager's choice.
  * `FallbackRunsSpec` compares the two for the code points of [[checkedCodePoints]]. Which fallback font the font
  * manager picks for a CJK character depends on the fonts a machine has, as it does for `TextLine`.
  */
private[skia] object FallbackRuns:

  private val FirstCharNeedingScriptRuns = 0x0300

  /** The CJK symbols and punctuation with kana (3000-30FF), the ideographs (4E00-9FFF) and the fullwidth forms. */
  private val Ranges: Vector[Range] =
    Vector(0x2500 to 0x257f, 0x2190 to 0x21ff, 0x2000 to 0x206f, 0x3000 to 0x30ff, 0x4e00 to 0x9fff, 0xff00 to 0xffef)

  private val Ideographs: Range.Inclusive = 0x4e00 to 0x9fff

  /** Every this-many-th ideograph is compared with `TextLine`, from the first: a prime, so the sample does not line up
    * with how the block is laid out. Comparing all 20,992 would take minutes for each font, for a rule that is the same
    * for every ideograph: the character is looked up in the base font, then in the last fallback, then asked of the
    * font manager.
    */
  val IdeographStride: Int = 61

  /** A hundred ideographs in everyday use, which the stride may skip. */
  private val CommonIdeographs: Vector[Int] =
    "的一是不了人我在有他这中大来上国个到说们为子和你地出道也时年得就那要下以生会自着去之过家学对可她里后小么心多天而能好都然没日于起还发成事只作当想看文知".codePoints().toArray.toVector

  /** Controls that reorder, join or break text, which runs of one direction and one script do not model: bidi marks and
    * embeddings, joiners, and the line and paragraph separators (the last two differed from `TextLine` on macOS).
    */
  private val Controls: Set[Int] =
    Set(0x200c, 0x200d, 0x200e, 0x200f, 0x2028, 0x2029) ++ (0x202a to 0x202e) ++ (0x2066 to 0x2069)

  /** Marks that combine with the character before them (the ideographic tone marks, the kana voiced sound marks, and
    * their halfwidth forms), which `TextLine` shapes with that character, so runs of one character each do not model
    * them: they differed from it on every font.
    */
  private def combines(codePoint: Int): Boolean =
    Set(Character.NON_SPACING_MARK, Character.ENCLOSING_MARK, Character.COMBINING_SPACING_MARK)
      .contains(Character.getType(codePoint).toByte) || codePoint == 0xff9e || codePoint == 0xff9f

  def isAllowlisted(codePoint: Int): Boolean =
    Ranges.exists(_.contains(codePoint)) && !Controls(codePoint) && !combines(codePoint) &&
      Character.isDefined(codePoint)

  /** Every allowlisted code point, in order. */
  def codePoints: Vector[Int] = Ranges.flatten.filter(isAllowlisted)

  /** The code points compared with `TextLine`: every one of the blocks but the ideographs, and of those the first and
    * last, every [[IdeographStride]]-th and the common ones.
    */
  def checkedCodePoints: Vector[Int] =
    val sampled = (Ideographs.start +: Ideographs.end +: Ideographs.by(IdeographStride).toVector) ++ CommonIdeographs
    codePoints.filter(cp => !Ideographs.contains(cp)) ++ sampled.distinct.filter(isAllowlisted).sorted

  /** Whether every character is either plain (below the combining marks) or allowlisted, and at least one is the
    * latter.
    */
  def eligible(text: String): Boolean =
    text.exists(c => isAllowlisted(c.toInt)) &&
      text.forall(c => c.toInt < FirstCharNeedingScriptRuns || isAllowlisted(c.toInt))

  private val Punctuation: Set[Int] = Set(
    Character.CONNECTOR_PUNCTUATION,
    Character.DASH_PUNCTUATION,
    Character.START_PUNCTUATION,
    Character.END_PUNCTUATION,
    Character.INITIAL_QUOTE_PUNCTUATION,
    Character.FINAL_QUOTE_PUNCTUATION,
    Character.OTHER_PUNCTUATION
  ).map(_.toInt)

  /** Spaces and punctuation go with the run before them when its font has them; anything else prefers the base font. */
  private def staysInRun(c: Char): Boolean =
    val kind = Character.getType(c)
    kind == Character.SPACE_SEPARATOR || Punctuation(kind)

  /** The run lengths and their fonts, or `None` when some character has no font at all. A space or punctuation mark
    * goes with the run before it when `spacesJoinRun` (as `TextLine` sets it), and otherwise takes the base font when
    * that has it (as AWT's logical fonts set it).
    */
  def runsFor(
    text: String,
    base: Font,
    fallbacks: FallbackFonts,
    spacesJoinRun: Boolean = true
  ): Option[Vector[(Int, Font)]] =
    def has(font: Font, c: Char): Boolean = font.getTypeface.getUTF32Glyph(c.toInt) != 0
    val assigned = text.foldLeft(Option((Vector.empty[Font], base, Option.empty[Font]))) {
      case (None, _) => None
      case (Some((fonts, current, last)), c) =>
        val next: Option[(Font, Option[Font])] =
          if spacesJoinRun && staysInRun(c) && has(current, c) then Some((current, last))
          else if has(base, c) then Some((base, last))
          else
            last.filter(has(_, c)) match
              case Some(font) => Some((font, last))
              case None       => fallbacks.fontFor(base, c.toInt).map(font => (font, Some(font)))
        next.map((font, remembered) => (fonts :+ font, font, remembered))
    }
    assigned.map { (fonts, _, _) =>
      fonts.foldLeft(Vector.empty[(Int, Font)]) { (runs, font) =>
        runs.lastOption match
          case Some((length, previous)) if previous eq font => runs.dropRight(1) :+ ((length + 1, font))
          case _                                            => runs :+ ((1, font))
      }
    }
