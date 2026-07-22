package com.serenity

import java.awt.Rectangle
import java.beans.PropertyChangeListener
import javax.accessibility.{AccessibleRole, AccessibleState}
import javax.swing.JPanel

import scala.collection.mutable.ListBuffer

import com.serenity.ui.accessibility.{
  AccessibilityRole,
  AccessibilitySnapshot,
  AccessibleNode,
  SwingAccessibilityBridge
}
import com.serenity.ui.layout.LayoutRect
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
          Some("content"),
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
    val status = (message: String) =>
      AccessibilitySnapshot(
        List(
          AccessibleNode(
            "surface:runner/status",
            AccessibilityRole.Status,
            "Status",
            Some(message),
            false,
            false,
            LayoutRect(0, 0, 20, 1)
          )
        ),
        Nil
      )

    bridge.publish(status("Invalid command"))
    events.clear()
    bridge.publish(status("Unknown command"))

    events.toList shouldBe List("Unknown command")
  }
