package com.serenity

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

  "SwingAccessibilityBridge" should "publish the focused canvas control through Swing accessibility" in {
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
  }
