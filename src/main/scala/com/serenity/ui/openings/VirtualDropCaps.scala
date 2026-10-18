package com.serenity.ui.openings

import com.serenity.richtext.{ParagraphRole, RichTextDocument}

/** The openings of one rich-text document that are set as drop caps without the document saying so. */
final case class VirtualDropCaps(openings: OpeningLines, handDropCap: HandDropCapRule, lines: Int):

  /** `stored`, the role the document keeps for the paragraph at `line`, or the drop cap an opening there is given. A
    * hand-made drop cap on an opening is kept unless the rules let the opening's win.
    */
  def roleAt(line: Int, stored: ParagraphRole): EffectiveRole =
    if !opensWithDropCap(line) then EffectiveRole.Real(stored)
    else
      stored match
        case ParagraphRole.Body => EffectiveRole.Virtual(ParagraphRole.dropCap(lines))
        case ParagraphRole.DropCap(_) if handDropCap == HandDropCapRule.OpeningWins =>
          EffectiveRole.Virtual(ParagraphRole.dropCap(lines))
        case other => EffectiveRole.Real(other)

  /** What the openings are set as, apart from which paragraphs they turned out to be. */
  def signature: (OpeningRules, Int) = (openings.rules, lines)

  private def opensWithDropCap(line: Int): Boolean =
    openings.at(line).exists(_.kind != OpeningKind.SceneBreakSmallCaps)

object VirtualDropCaps:

  /** How many lines a chapter opening's cap spans: two, which a letter fitted to its lines carries well; a hand-made
    * cap keeps whatever its document gives it.
    */
  val OpeningLines: Int = 2

  /** Whether `settings` set openings as drop caps. */
  def drawn(settings: OpeningSettings): Boolean =
    settings.enabled && settings.letterMode == LetterMode.Drop

  /** Where the openings of a document are found: a cache can answer for a document it has scanned before. */
  type Scan = (RichTextDocument, OpeningRules) => OpeningLines

  val scanning: Scan = (document, rules) =>
    ChapterOpenings.openingLines(document, rules, ChapterOpenings.noExplicitEpigraphs)

  /** The virtual drop caps `settings` give `document`, or none while they are off or set the letter another way. */
  def of(settings: OpeningSettings, document: RichTextDocument, scan: Scan = scanning): Option[VirtualDropCaps] =
    Option.when(drawn(settings)) {
      VirtualDropCaps(scan(document, settings.rules), settings.rules.handDropCap, OpeningLines)
    }
