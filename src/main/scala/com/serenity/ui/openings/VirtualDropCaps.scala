package com.serenity.ui.openings

import com.serenity.richtext.{ParagraphRole, RichTextDocument}

/** The openings of one rich-text document whose first letter is set as an initial without the document saying so: a
  * drop cap spanning `lines`, or a letter of `shape` standing on the baseline of the `lines`-th line.
  */
final case class VirtualDropCaps(
    openings: OpeningLines,
    handDropCap: HandDropCapRule,
    lines: Int,
    shape: LetterShape = LetterShape.Drop
):

  /** `stored`, the role the document keeps for the paragraph at `line`, or the initial an opening there is given. A
    * hand-made drop cap on an opening is kept unless the rules let the opening's win.
    */
  def roleAt(line: Int, stored: ParagraphRole): EffectiveRole =
    if !opensWithDropCap(line) then EffectiveRole.Real(stored)
    else
      stored match
        case ParagraphRole.Body => EffectiveRole.Virtual(ParagraphRole.dropCap(lines), shape)
        case ParagraphRole.DropCap(_) if handDropCap == HandDropCapRule.OpeningWins =>
          EffectiveRole.Virtual(ParagraphRole.dropCap(lines), shape)
        case other => EffectiveRole.Real(other)

  /** What the openings are set as, apart from which paragraphs they turned out to be. */
  def signature: VirtualDropCaps.Signature = (openings.rules, lines, shape)

  private def opensWithDropCap(line: Int): Boolean =
    openings.at(line).exists(_.kind != OpeningKind.SceneBreakSmallCaps)

object VirtualDropCaps:

  type Signature = (OpeningRules, Int, LetterShape)

  /** How many lines a chapter opening's drop cap spans: two, which a letter fitted to its lines carries well; a
    * hand-made cap keeps whatever its document gives it.
    */
  val OpeningLines: Int = 2

  /** Whether `settings` set the first letter of openings apart. */
  def drawn(settings: OpeningSettings): Boolean = settings.enabled

  /** Whether `settings` make an opening's first row as tall as the letter it holds: such a row changes height with the
    * letter, and with whether its paragraph opens a chapter at all.
    */
  def growsRows(settings: OpeningSettings): Boolean =
    drawn(settings) && settings.letterMode != LetterMode.Drop && settings.raisedRow == RaisedRowRule.Grow

  /** The lines the letter's baseline is counted down to, and the shape it is set in. A raised letter stands on the
    * first line, a hybrid one on the line it sinks to.
    */
  private def placement(settings: OpeningSettings): (Int, LetterShape) =
    val rising = LetterShape.Rising(growsRow = settings.raisedRow == RaisedRowRule.Grow)
    settings.letterMode match
      case LetterMode.Drop   => (OpeningLines, LetterShape.Drop)
      case LetterMode.Raised => (1, rising)
      case LetterMode.Hybrid => (settings.normalized.hybridSinkLines, rising)

  /** Where the openings of a document are found: a cache can answer for a document it has scanned before. */
  type Scan = (RichTextDocument, OpeningRules) => OpeningLines

  val scanning: Scan = (document, rules) =>
    ChapterOpenings.openingLines(document, rules, ChapterOpenings.noExplicitEpigraphs)

  /** The virtual initials `settings` give `document`, or none while openings are off. */
  def of(settings: OpeningSettings, document: RichTextDocument, scan: Scan = scanning): Option[VirtualDropCaps] =
    Option.when(drawn(settings)) {
      val (lines, shape) = placement(settings)
      VirtualDropCaps(scan(document, settings.rules), settings.rules.handDropCap, lines, shape)
    }
