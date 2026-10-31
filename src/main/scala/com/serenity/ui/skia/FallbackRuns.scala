package com.serenity.ui.skia

import org.jetbrains.skia.Font

/** Font runs for text with a few simple symbols the base font may lack (box drawing, arrows, general punctuation),
  * which Skiko's `TextLine` would shape for 4 to 8 ms a call whatever the text. Shaping the runs ourselves, as the
  * Latin path does, takes a fraction of a millisecond. The runs are `TextLine`'s own: a space or a punctuation mark
  * stays in the font of the run before it while that font has the glyph, any other character prefers the base font,
  * then the last fallback font, then the font manager's choice. `FallbackRunsSpec` compares the two for every code
  * point listed here.
  */
private[skia] object FallbackRuns:

  private val FirstCharNeedingScriptRuns = 0x0300

  private val Ranges: Vector[Range] = Vector(0x2500 to 0x257f, 0x2190 to 0x21ff, 0x2000 to 0x206f)

  /** Marks and embeddings that reorder or join text, which runs of one direction and one script do not model. */
  private val BidiAndJoinerControls: Set[Int] =
    Set(0x200c, 0x200d, 0x200e, 0x200f) ++ (0x202a to 0x202e) ++ (0x2066 to 0x2069)

  def isAllowlisted(codePoint: Int): Boolean =
    Ranges.exists(_.contains(codePoint)) && !BidiAndJoinerControls(codePoint) && Character.isDefined(codePoint)

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

  /** The run lengths and their fonts, or `None` when some character has no font at all. */
  def runsFor(text: String, base: Font, fallbacks: FallbackFonts): Option[Vector[(Int, Font)]] =
    def has(font: Font, c: Char): Boolean = font.getTypeface.getUTF32Glyph(c.toInt) != 0
    val assigned = text.foldLeft(Option((Vector.empty[Font], base, Option.empty[Font]))) {
      case (None, _) => None
      case (Some((fonts, current, last)), c) =>
        val next: Option[(Font, Option[Font])] =
          if staysInRun(c) && has(current, c) then Some((current, last))
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
