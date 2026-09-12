package com.serenity

import scala.collection.mutable.ListBuffer

import com.serenity.ui.accessibility.{
  AccessibilityAnnouncement,
  AccessibilityRole,
  AccessibilitySnapshot,
  AccessibleNode,
  TuiAccessibilityBridge
}
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A terminal has no accessibility-tree API to publish into the way Swing does (see `SwingWindowAccessibilitySpec`) --
  * these cover the OSC-based baseline `TuiAccessibilityBridge` speaks instead: a title update a screen reader's
  * window-title announcement fires on, and a desktop notification per `AccessibilitySnapshot` announcement.
  */
class TuiAccessibilityBridgeSpec extends AnyFlatSpec with Matchers:

  private val esc = 0x1b.toChar.toString
  private val bel = 0x07.toChar.toString

  private def writer(): (ListBuffer[String], String => Unit) =
    val written = ListBuffer.empty[String]
    (written, text => written += text)

  private def node(
    role: AccessibilityRole,
    name: String,
    value: Option[String] = None,
    selected: Boolean = false,
    focused: Boolean = true
  ): AccessibleNode =
    AccessibleNode(s"node:$name", role, name, value, selected, focused, LayoutRect(0, 0, 10, 1))

  "TuiAccessibilityBridge" should "retitle the terminal with the focused node's role and name" in {
    val (written, write) = writer()
    val bridge           = new TuiAccessibilityBridge(write)

    bridge.publish(AccessibilitySnapshot(List(node(AccessibilityRole.Button, "Save")), Nil))

    written should contain(s"$esc]0;button Save$bel")
  }

  it should "include the node's value and selection state in the title" in {
    val (written, write) = writer()
    val bridge           = new TuiAccessibilityBridge(write)

    bridge.publish(
      AccessibilitySnapshot(
        List(node(AccessibilityRole.TextField, "Find", value = Some("needle"), selected = true)),
        Nil
      )
    )

    written should contain(s"$esc]0;textfield Find: needle, selected$bel")
  }

  it should "fall back to a generic title when nothing is focused" in {
    val (written, write) = writer()
    val bridge           = new TuiAccessibilityBridge(write)

    bridge.publish(AccessibilitySnapshot(Nil, Nil))

    written should contain(s"$esc]0;Serenity editor$bel")
  }

  it should "not rewrite the title when the focused description has not changed" in {
    val (written, write) = writer()
    val bridge           = new TuiAccessibilityBridge(write)
    val snapshot         = AccessibilitySnapshot(List(node(AccessibilityRole.Button, "Save")), Nil)

    bridge.publish(snapshot)
    written.clear()
    bridge.publish(snapshot)

    written shouldBe empty
  }

  it should "send one OSC 9 desktop notification per announcement, verbatim" in {
    val (written, write) = writer()
    val bridge           = new TuiAccessibilityBridge(write)

    // A fresh bridge's `previousTitle` starts unset, so its very first `publish` always (re)writes the title even
    // when nothing is focused -- establish that baseline first so this test measures only the notifications.
    bridge.publish(AccessibilitySnapshot(Nil, Nil))
    written.clear()

    bridge.publish(
      AccessibilitySnapshot(Nil, List(AccessibilityAnnouncement("Saved"), AccessibilityAnnouncement("2 matches")))
    )

    written.toList shouldBe List(s"$esc]9;Saved$bel", s"$esc]9;2 matches$bel")
  }

  it should "strip escape and BEL bytes from titles and messages so they cannot forge a second sequence" in {
    val (written, write) = writer()
    val bridge           = new TuiAccessibilityBridge(write)

    bridge.publish(
      AccessibilitySnapshot(
        List(node(AccessibilityRole.Button, s"evil${esc}]0;hijack$bel")),
        List(AccessibilityAnnouncement(s"msg${esc}]9;hijack$bel"))
      )
    )

    written.toList shouldBe List(s"$esc]0;button evil]0;hijack$bel", s"$esc]9;msg]9;hijack$bel")
  }
