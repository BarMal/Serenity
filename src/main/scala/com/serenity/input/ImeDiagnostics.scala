package com.serenity.input

import java.awt.event.{InputMethodEvent, InputMethodListener, KeyEvent}
import java.awt.font.{TextAttribute, TextHitInfo}
import java.awt.im.{InputMethodHighlight, InputMethodRequests}
import java.awt.{Component, IllegalComponentStateException, Point, Rectangle}
import java.text.{Annotation, AttributedCharacterIterator, AttributedString}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/** Opt-in evidence log for input-method behaviour (issue #2008), enabled by `SERENITY_IME_DIAGNOSTICS=1`. It records
  * what each platform's input method delivers, so the composition design can be checked against real IBus, Fcitx, macOS
  * and Windows behaviour (see `docs/ime-platform-matrix.md`). Text is logged as code points, never as characters.
  */
object ImeDiagnostics:

  type Sink = String => Unit

  val EnvVar: String = "SERENITY_IME_DIAGNOSTICS"

  def enabled(env: Map[String, String]): Boolean =
    env.get(EnvVar).exists(value => value == "1" || value.equalsIgnoreCase("true"))

  def fromEnvironment(env: Map[String, String]): Option[Sink] =
    Option.when(enabled(env)) {
      val log = org.slf4j.LoggerFactory.getLogger("ImeDiagnostics")
      line => log.info(line)
    }

  def listener(sink: Sink, now: () => Long = () => System.currentTimeMillis()): InputMethodListener =
    new InputMethodListener:
      override def inputMethodTextChanged(event: InputMethodEvent): Unit = sink(describe(event, now()))
      override def caretPositionChanged(event: InputMethodEvent): Unit   = sink(describe(event, now()))

  def describe(event: InputMethodEvent, nowMillis: Long): String =
    val text   = Option(event.getText)
    val chars  = text.fold("")(characters)
    val points = chars.codePoints.toArray.map(_.toHexString.toUpperCase).mkString(",")
    val runs   = text.fold(List.empty[String])(runsOf).mkString("; ")
    s"t=$nowMillis IME id=${idName(event.getID)} committed=${event.getCommittedCharacterCount} " +
      s"length=${chars.length} caret=${hit(event.getCaret)} visible=${hit(event.getVisiblePosition)} " +
      s"codePoints=[$points] runs=[$runs]"

  def describe(event: KeyEvent, nowMillis: Long): String =
    val name = if event.getID == KeyEvent.KEY_TYPED then "KEY_TYPED" else s"KEY_${event.getID}"
    s"t=$nowMillis $name char=${event.getKeyChar.toInt.toHexString.toUpperCase} " +
      s"keyCode=${event.getKeyCode} modifiers=${event.getModifiersEx}"

  /** Enough to make the input method treat the component as a text client and place its candidate window at the
    * component's origin; the document is reported empty because nothing here reads it back yet. `null` is the AWT
    * contract's own answer for "no offset" and "nothing to cancel".
    */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def requests(component: Component, sink: Sink): InputMethodRequests =
    new InputMethodRequests:
      override def getTextLocation(offset: TextHitInfo): Rectangle =
        sink(s"REQ getTextLocation offset=${hit(offset)}")
        val origin =
          try component.getLocationOnScreen
          catch case _: IllegalComponentStateException => new Point(0, 0)
        new Rectangle(origin.x, origin.y, 1, 16)

      override def getLocationOffset(x: Int, y: Int): TextHitInfo =
        sink(s"REQ getLocationOffset x=$x y=$y")
        null

      override def getInsertPositionOffset: Int =
        sink("REQ getInsertPositionOffset")
        0

      override def getCommittedText(
        beginIndex: Int,
        endIndex: Int,
        attributes: Array[AttributedCharacterIterator.Attribute]
      ): AttributedCharacterIterator =
        sink(s"REQ getCommittedText begin=$beginIndex end=$endIndex")
        empty

      override def getCommittedTextLength: Int =
        sink("REQ getCommittedTextLength")
        0

      override def cancelLatestCommittedText(
        attributes: Array[AttributedCharacterIterator.Attribute]
      ): AttributedCharacterIterator =
        sink("REQ cancelLatestCommittedText")
        null

      override def getSelectedText(
        attributes: Array[AttributedCharacterIterator.Attribute]
      ): AttributedCharacterIterator =
        sink("REQ getSelectedText")
        empty

  private def empty: AttributedCharacterIterator = new AttributedString("").getIterator

  private def idName(id: Int): String = id match
    case InputMethodEvent.INPUT_METHOD_TEXT_CHANGED => "INPUT_METHOD_TEXT_CHANGED"
    case InputMethodEvent.CARET_POSITION_CHANGED    => "CARET_POSITION_CHANGED"
    case other                                      => other.toString

  private def hit(info: TextHitInfo): String =
    Option(info).fold("none")(h => s"${h.getCharIndex} ${if h.isLeadingEdge then "leading" else "trailing"}")

  private def characters(text: AttributedCharacterIterator): String =
    (text.getBeginIndex until text.getEndIndex).map { index =>
      text.setIndex(index)
      text.current
    }.mkString

  private def runsOf(text: AttributedCharacterIterator): List[String] =
    @tailrec def loop(index: Int, acc: List[String]): List[String] =
      if index >= text.getEndIndex then acc.reverse
      else
        text.setIndex(index)
        val limit = text.getRunLimit.max(index + 1)
        loop(limit, describeRun(index, limit, text.getAttributes.asScala.toMap) :: acc)
    loop(text.getBeginIndex, Nil)

  private def describeRun(
    start: Int,
    end: Int,
    attributes: Map[AttributedCharacterIterator.Attribute, AnyRef]
  ): String =
    val highlight = attributes.get(TextAttribute.INPUT_METHOD_HIGHLIGHT).flatMap(highlightOf)
    val others    = attributes.keys.filterNot(_ == TextAttribute.INPUT_METHOD_HIGHLIGHT).map(_.toString).toList.sorted
    val state = highlight.fold("plain") { h =>
      val kind = if h.getState == InputMethodHighlight.CONVERTED_TEXT then "CONVERTED" else "RAW"
      if h.isSelected then s"$kind selected" else kind
    }
    val extra = if others.isEmpty then "" else s" +${others.mkString(",")}"
    s"[$start,$end) $state$extra"

  private[input] def highlightOf(value: AnyRef): Option[InputMethodHighlight] = value match
    case highlight: InputMethodHighlight => Some(highlight)
    case annotation: Annotation          => Option(annotation.getValue).flatMap(highlightOf)
    case _                               => None
