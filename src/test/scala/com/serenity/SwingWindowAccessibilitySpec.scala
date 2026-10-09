package com.serenity

import java.awt.Rectangle
import java.beans.PropertyChangeListener
import javax.accessibility.{AccessibleRole, AccessibleState}
import javax.swing.JPanel

import scala.collection.mutable.ListBuffer

import com.serenity.ui.accessibility.{
  AccessibilityAnnouncement,
  AccessibilityPublishGate,
  AccessibilityRole,
  AccessibilitySnapshot,
  AccessibleNode,
  AccessibleValue,
  SwingAccessibilityBridge
}
import com.serenity.ui.layout.{CellMetrics, LayoutRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingWindowAccessibilitySpec extends AnyFlatSpec with Matchers:

  "SwingAccessibilityBridge" should "materialize focused canvas controls as native accessibility children" in {
    val canvas = new JPanel
    val bridge = new SwingAccessibilityBridge(canvas)
    val node = AccessibleNode(
      "surface:runner/item:open-settings",
      AccessibilityRole.Button,
      "Open Settings",
      None,
      selected = true,
      focused = true,
      LayoutRect(2, 3, 20, 2)
    )

    bridge.publish(AccessibilitySnapshot(List(node), Nil))

    canvas.getAccessibleContext.getAccessibleName shouldBe "Open Settings"
    canvas.getAccessibleContext.getAccessibleDescription should include("button Open Settings")
    canvas.getAccessibleContext.getAccessibleChildrenCount shouldBe 1

    val child = canvas.getAccessibleContext.getAccessibleChild(0)
    child.getAccessibleContext.getAccessibleName shouldBe "Open Settings"
    child.getAccessibleContext.getAccessibleRole shouldBe AccessibleRole.PUSH_BUTTON
    child.getAccessibleContext.getAccessibleStateSet.contains(AccessibleState.CHECKED) shouldBe false
    child.asInstanceOf[java.awt.Component].getName shouldBe "surface:runner/item:open-settings"
    child.asInstanceOf[java.awt.Component].getBounds shouldBe new Rectangle(2, 3, 20, 2)
    child.getAccessibleContext.getAccessibleDescription should include("selected=true")
    child.getAccessibleContext.getAccessibleStateSet.contains(AccessibleState.FOCUSED) shouldBe true
  }

  it should "preserve native accessibility children across cursor-only publications" in {
    val canvas = new JPanel
    val bridge = new SwingAccessibilityBridge(canvas)
    val snapshot = AccessibilitySnapshot(
      List(
        AccessibleNode(
          "pane:0",
          AccessibilityRole.Document,
          "Untitled document",
          Some(AccessibleValue.Plain("content")),
          selected = false,
          focused = true,
          LayoutRect(0, 0, 80, 24)
        )
      ),
      Nil
    )

    bridge.publish(snapshot)
    val childBeforeCursorRender = canvas.getAccessibleContext.getAccessibleChild(0)

    bridge.publish(snapshot)

    canvas.getAccessibleContext.getAccessibleChild(0) should be theSameInstanceAs childBeforeCursorRender
  }

  it should "emit one description event for each validation status change" in {
    val canvas = new JPanel
    val bridge = new SwingAccessibilityBridge(canvas)
    val events = ListBuffer.empty[String]
    canvas.getAccessibleContext.addPropertyChangeListener(
      new PropertyChangeListener:
        override def propertyChange(event: java.beans.PropertyChangeEvent): Unit =
          if event.getPropertyName == javax.accessibility.AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY then
            events += Option(event.getNewValue).fold("")(_.toString)
    )
    // The bridge relays `snapshot.announcements` verbatim; the diff against prior state is computed once, by
    // `AccessibilityModel` (see `AccessibilityModelSpec`), so each fixture supplies the announcement its message
    // would carry off that diff rather than the bridge re-deriving it from the node list.
    val status = (message: String) =>
      AccessibilitySnapshot(
        List(
          AccessibleNode(
            "surface:runner/status",
            AccessibilityRole.Status,
            "Status",
            Some(AccessibleValue.Plain(message)),
            false,
            false,
            LayoutRect(0, 0, 20, 1)
          )
        ),
        List(AccessibilityAnnouncement(message))
      )

    bridge.publish(status("Invalid command"))
    events.clear()
    bridge.publish(status("Unknown command"))

    events.toList shouldBe List("Unknown command")
  }

  it should "update an existing document proxy in place when only its text changed" in {
    val canvas = new JPanel
    val bridge = new SwingAccessibilityBridge(canvas)
    def document(text: String) = AccessibilitySnapshot(
      List(
        AccessibleNode(
          "pane:0",
          AccessibilityRole.Document,
          "Untitled document",
          Some(AccessibleValue.Plain(text)),
          selected = false,
          focused = true,
          LayoutRect(0, 0, 80, 24)
        )
      ),
      Nil
    )

    bridge.publish(document("before"))
    val proxy = canvas.getAccessibleContext.getAccessibleChild(0)

    bridge.publish(document("after"))

    canvas.getAccessibleContext.getAccessibleChildrenCount shouldBe 1
    canvas.getAccessibleContext.getAccessibleChild(0) should be theSameInstanceAs proxy
    proxy.asInstanceOf[javax.swing.JTextArea].getText shouldBe "after"
    proxy.getAccessibleContext.getAccessibleDescription should include("value=after")
  }

  it should "rebuild the proxies when a node moves" in {
    val canvas = new JPanel
    val bridge = new SwingAccessibilityBridge(canvas)
    def button(bounds: LayoutRect) = AccessibilitySnapshot(
      List(AccessibleNode("button", AccessibilityRole.Button, "Go", None, selected = false, focused = false, bounds)),
      Nil
    )

    bridge.publish(button(LayoutRect(0, 0, 4, 1)))
    val before = canvas.getAccessibleContext.getAccessibleChild(0)

    bridge.publish(button(LayoutRect(2, 3, 4, 1)))

    canvas.getAccessibleContext.getAccessibleChild(0) should not be theSameInstanceAs(before)
    canvas.getAccessibleContext.getAccessibleChild(0).asInstanceOf[java.awt.Component].getBounds shouldBe
      new Rectangle(2, 3, 4, 1)
  }

  "AccessibilityPublishGate" should "admit a snapshot once, then again only when it or the metrics change" in {
    val gate    = new AccessibilityPublishGate
    val metrics = CellMetrics(10, 20, 15)
    def snapshot(name: String) = AccessibilitySnapshot(
      List(
        AccessibleNode(
          "button",
          AccessibilityRole.Button,
          name,
          None,
          selected = false,
          focused = true,
          LayoutRect(0, 0, 4, 1)
        )
      ),
      Nil
    )

    gate.admit(snapshot("Go"), metrics) shouldBe true
    gate.admit(snapshot("Go"), metrics) shouldBe false
    gate.admit(snapshot("Stop"), metrics) shouldBe true
    gate.admit(snapshot("Stop"), CellMetrics(12, 24, 18)) shouldBe true
    gate.admit(snapshot("Stop"), CellMetrics(12, 24, 18)) shouldBe false
  }
