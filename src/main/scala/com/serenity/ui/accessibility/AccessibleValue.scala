package com.serenity.ui.accessibility

import com.serenity.richtext.InlineAtom
import com.serenity.rope.Rope

/** What a node reports as its value. A document's text is held as its `Rope` and only turned into a `String` when an
  * assistive technology (or a description of the node) actually asks for it, so projecting a document costs nothing
  * proportional to its size and two projections of unchanged text compare equal without reading either.
  */
sealed trait AccessibleValue:
  def text: String

object AccessibleValue:

  final case class Plain(text: String) extends AccessibleValue

  /** Equal exactly when it wraps the same `Rope` object. `Rope` is persistent, so an unedited document keeps its
    * identity from frame to frame and any edit yields a new root. Identity is used rather than
    * `Document.contentVersion` because undo restores an older document along with its older version number, after which
    * a fresh edit can reach a version that already named different text. `inlineAtomsAsText` marks a rich document,
    * whose rope holds placeholder characters that must read as the text they stand for (a soft break as a newline).
    */
  final class DocumentText(val content: Rope, val inlineAtomsAsText: Boolean = false) extends AccessibleValue:

    def text: String =
      val collected = content.collect()
      if inlineAtomsAsText then InlineAtom.asPlainText(collected) else collected

    override def equals(other: Any): Boolean =
      other match
        case that: DocumentText =>
          ((content: AnyRef) eq (that.content: AnyRef)) && inlineAtomsAsText == that.inlineAtomsAsText
        case _ => false

    override def hashCode: Int = System.identityHashCode(content)

    override def toString: String = s"DocumentText(${content.weight} chars)"

  def plain(text: Option[String]): Option[AccessibleValue] = text.map(Plain.apply)
