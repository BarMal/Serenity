package com.serenity

import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RichTextToolbarLayoutSpec extends AnyFlatSpec with Matchers:

  "RichTextToolbar" should "expose the first rich text formatting commands as toolbar actions" in
    RichTextToolbar.defaultItems
      .map(_.commandName)
      .shouldBe(
        List(
          "bold",
          "italic",
          "underline",
          "paragraph-body",
          "heading-1",
          "heading-2",
          "heading-3",
          "align-left",
          "align-center",
          "align-right",
          "align-justify"
        )
      )

  it should "show, move focus, and dismiss without mutating the previous state" in {
    val hidden = RichTextToolbarState.Hidden
    val shown  = hidden.showAt(ScreenPosition(40, 10))
    val moved  = shown.focusNext(RichTextToolbar.defaultItems)

    hidden.isVisible.shouldBe(false)
    shown.isVisible.shouldBe(true)
    shown.focusedItem(RichTextToolbar.defaultItems).map(_.commandName).shouldBe(Some("bold"))
    moved.focusedItem(RichTextToolbar.defaultItems).map(_.commandName).shouldBe(Some("italic"))
    moved.dismiss.shouldBe(RichTextToolbarState.Hidden)
  }

  it should "move focus from the clamped item when toolbar actions change" in {
    val items         = RichTextToolbar.defaultItems.take(3)
    val staleHigh     = RichTextToolbarState.visible(ScreenPosition(40, 10)).copy(focusedIndex = 10)
    val staleNegative = RichTextToolbarState.visible(ScreenPosition(40, 10)).copy(focusedIndex = -4)

    staleHigh.focusedItem(items).map(_.commandName).shouldBe(Some("underline"))
    staleHigh.focusNext(items).focusedItem(items).map(_.commandName).shouldBe(Some("bold"))

    staleNegative.focusedItem(items).map(_.commandName).shouldBe(Some("bold"))
    staleNegative.focusPrevious(items).focusedItem(items).map(_.commandName).shouldBe(Some("underline"))
  }

  it should "place a visible toolbar above the cursor when there is room" in {
    val contentRect = LayoutRect(0, 1, 80, 20)
    val state       = RichTextToolbarState.Hidden.showAt(ScreenPosition(40, 10))

    val layout = RichTextToolbar.layout(state, contentRect).getOrElse(fail("Expected toolbar layout"))

    layout.shouldBe(
      RichTextToolbarLayout(
        rect = LayoutRect(18, 6, 44, 3),
        placement = RichTextToolbarPlacement.AboveCursor
      )
    )
  }

  it should "fall back below the cursor when there is not enough room above" in {
    val contentRect = LayoutRect(10, 5, 50, 12)
    val state       = RichTextToolbarState.Hidden.showAt(ScreenPosition(16, 6))

    val layout = RichTextToolbar.layout(state, contentRect).getOrElse(fail("Expected toolbar layout"))

    layout.placement.shouldBe(RichTextToolbarPlacement.BelowCursor)
    layout.rect.y.shouldBe(8)
    contentRect.containsRect(layout.rect).shouldBe(true)
  }

  it should "clamp horizontal placement inside the active content rect" in {
    val contentRect = LayoutRect(5, 1, 60, 20)
    val state       = RichTextToolbarState.Hidden.showAt(ScreenPosition(64, 12))

    val layout = RichTextToolbar.layout(state, contentRect).getOrElse(fail("Expected toolbar layout"))

    layout.rect.right.shouldBe(contentRect.right)
    contentRect.containsRect(layout.rect).shouldBe(true)
  }

  it should "return no layout when hidden" in
    RichTextToolbar.layout(RichTextToolbarState.Hidden, LayoutRect(0, 0, 80, 20)).shouldBe(None)
end RichTextToolbarLayoutSpec
