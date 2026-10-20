package com.serenity.input

import java.awt.font.TextHitInfo
import java.awt.im.InputMethodRequests
import java.awt.{Component, IllegalComponentStateException, Point, Rectangle}
import java.text.{AttributedCharacterIterator, AttributedString}

/** What the editor tells the platform's input method (#2008): where its candidate window goes, and that there is no
  * committed text for it to read back (no reconversion) or take away (`cancelLatestCommittedText` is `null`, the
  * contract's answer for nothing to cancel). `null` is likewise the answer for "no offset".
  *
  * `observe` is told each request as it comes in, which the opt-in diagnostic logs.
  */
object ImeRequests:

  /** A caret whose position is not yet known is a thin rectangle at the component's origin. */
  private val FallbackSize = new java.awt.Dimension(1, 16)

  def forComponent(
    component: Component,
    caretBounds: () => Option[Rectangle],
    observe: String => Unit
  ): InputMethodRequests =
    create(caretBounds, () => screenOriginOf(component), observe)

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def create(
    caretBounds: () => Option[Rectangle],
    screenOrigin: () => Point,
    observe: String => Unit
  ): InputMethodRequests =
    new InputMethodRequests:
      override def getTextLocation(offset: TextHitInfo): Rectangle =
        observe(s"REQ getTextLocation offset=${ImeDiagnostics.hit(offset)}")
        val origin = screenOrigin()
        caretBounds().fold(new Rectangle(origin.x, origin.y, FallbackSize.width, FallbackSize.height)) { caret =>
          new Rectangle(origin.x + caret.x, origin.y + caret.y, caret.width, caret.height)
        }

      override def getLocationOffset(x: Int, y: Int): TextHitInfo =
        observe(s"REQ getLocationOffset x=$x y=$y")
        null

      override def getInsertPositionOffset: Int =
        observe("REQ getInsertPositionOffset")
        0

      override def getCommittedText(
        beginIndex: Int,
        endIndex: Int,
        attributes: Array[AttributedCharacterIterator.Attribute]
      ): AttributedCharacterIterator =
        observe(s"REQ getCommittedText begin=$beginIndex end=$endIndex")
        empty

      override def getCommittedTextLength: Int =
        observe("REQ getCommittedTextLength")
        0

      override def cancelLatestCommittedText(
        attributes: Array[AttributedCharacterIterator.Attribute]
      ): AttributedCharacterIterator =
        observe("REQ cancelLatestCommittedText")
        null

      override def getSelectedText(
        attributes: Array[AttributedCharacterIterator.Attribute]
      ): AttributedCharacterIterator =
        observe("REQ getSelectedText")
        empty

  private def empty: AttributedCharacterIterator = new AttributedString("").getIterator

  private def screenOriginOf(component: Component): Point =
    try component.getLocationOnScreen
    catch case _: IllegalComponentStateException => new Point(0, 0)
