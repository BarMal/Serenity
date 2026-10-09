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

  /** Source XML the model cannot edit but must put back where it was: an image, a bookmark, a comment anchor, a field
    * code. `visible` objects are drawn as a placeholder glyph; markers are invisible.
    */
  case Opaque(raw: String, visible: Boolean)

  /** A body-level construct the model does not hold (a table, a content control, an index), as the one atom of a
    * paragraph of its own so that the rope line still stands for the paragraph. The line is read-only: an edit that
    * would put text beside the atom is refused, and deleting the atom removes the block. `raw` is the block as the
    * source wrote it, empty when it could not be captured.
    */
  case Block(raw: String, feature: DocumentFeature)

  /** The one rope character standing for this atom. Each kind uses a different one so that plain text exports can turn
    * a soft break into a newline and drop an opaque atom or a block.
    */
  def character: Char =
    this match
      case SoftBreak    => InlineAtom.SoftBreakCharacter
      case Opaque(_, _) => InlineAtom.OpaqueCharacter
      case Block(_, _)  => InlineAtom.BlockCharacter

object InlineAtom:
  /** U+FFFC OBJECT REPLACEMENT CHARACTER. */
  val SoftBreakCharacter: Char = '\uFFFC'

  /** U+2060 WORD JOINER: zero width, so an invisible marker takes no room even where it is drawn as-is. */
  val OpaqueCharacter: Char = '\u2060'

  /** U+2064 INVISIBLE PLUS: the placeholder of a block. It is not U+FFFC because a rope-only export turns that into a
    * newline, which would put a blank line inside the text around a block.
    */
  val BlockCharacter: Char = '\u2064'

  /** `text` of a rich document's rope as plain text: a soft break becomes a newline and an opaque atom or a block
    * disappears.
    */
  def asPlainText(text: String): String =
    text.replace(SoftBreakCharacter, '\n').filterNot(isExportedAsNothing)

  /** The placeholders a plain text export leaves out rather than turning into text. */
  def isExportedAsNothing(char: Char): Boolean =
    char == OpaqueCharacter || char == BlockCharacter
