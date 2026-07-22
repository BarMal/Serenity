package com.serenity

import java.awt.Rectangle
import javax.accessibility.AccessibleRole
import javax.swing.JPanel

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
    child.getAccessibleContext.getAccessibleRole shouldBe AccessibleRole.TOGGLE_BUTTON
    child.asInstanceOf[java.awt.Component].getName shouldBe "surface:runner/item:open-settings"
    child.asInstanceOf[java.awt.Component].getBounds shouldBe new Rectangle(2, 3, 20, 2)
    child.getAccessibleContext.getAccessibleDescription should include("selected=true")
  }
