package com.serenity.command

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `ToggleItem` -- a generic, independent boolean checkbox row, distinct from `OptionItem`'s single choice among N
  * options. Covers the pure `toggled` state transition and its `searchText`/`hint` conventions.
  */
class CommandSurfaceItemToggleItemSpec extends AnyFlatSpec with Matchers:

  private def toggle(checked: Boolean, hint: Option[String] = None): CommandSurfaceItem.ToggleItem =
    CommandSurfaceItem.ToggleItem(
      id = "test-toggle",
      label = "Test Toggle",
      checked = checked,
      category = CommandCategory.Settings,
      hint = hint
    )

  "toggled" should "flip checked from false to true" in {
    toggle(checked = false).toggled.checked shouldBe true
  }

  it should "flip checked from true to false" in {
    toggle(checked = true).toggled.checked shouldBe false
  }

  it should "flip back and forth in a cycle -- checked, unchecked, checked" in {
    val checked   = toggle(checked = true)
    val unchecked = checked.toggled
    val recheck   = unchecked.toggled

    checked.checked shouldBe true
    unchecked.checked shouldBe false
    recheck.checked shouldBe true
  }

  it should "leave every other field untouched" in {
    val item    = toggle(checked = false, hint = Some("A hint"))
    val flipped = item.toggled

    flipped.id shouldBe item.id
    flipped.label shouldBe item.label
    flipped.category shouldBe item.category
    flipped.hint shouldBe item.hint
  }

  "searchText" should "combine label and hint" in {
    toggle(checked = false, hint = Some("Enable the thing")).searchText shouldBe "Test Toggle Enable the thing"
  }

  it should "fall back to just the label when there is no hint" in {
    toggle(checked = false).searchText shouldBe "Test Toggle"
  }
