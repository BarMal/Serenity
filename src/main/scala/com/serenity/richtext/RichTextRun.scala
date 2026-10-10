package com.serenity.richtext

/** Inline style for a contiguous text run. */
final case class RichTextStyle(
    marks: Set[InlineMark] = Set.empty,
    fontFamily: Option[String] = None,
    fontSize: Option[Float] = None,
    color: Option[String] = None,
    link: Option[String] = None,
    extras: List[RawProperty] = Nil
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
    atom.fold(text) {
      case InlineAtom.SoftBreak    => "\n"
      case InlineAtom.Opaque(_, _) => ""
      case InlineAtom.Block(_, _)  => ""
    }

object RichTextRun:
  /** U+FFFC OBJECT REPLACEMENT CHARACTER: the rope character standing for a soft break. */
  val AtomCharacter: Char = InlineAtom.SoftBreakCharacter

  def softBreak(style: RichTextStyle = RichTextStyle.empty): RichTextRun =
    atom(InlineAtom.SoftBreak, style)

  def opaque(raw: String, visible: Boolean, style: RichTextStyle = RichTextStyle.empty): RichTextRun =
    atom(InlineAtom.Opaque(raw, visible), style)

  def block(raw: String, feature: DocumentFeature): RichTextRun =
    atom(InlineAtom.Block(raw, feature), RichTextStyle.empty)

  private def atom(atom: InlineAtom, style: RichTextStyle): RichTextRun =
    RichTextRun(atom.character.toString, style, Some(atom))
