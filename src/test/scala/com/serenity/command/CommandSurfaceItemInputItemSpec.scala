package com.serenity.command

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** issue #1056: type-appropriate value editors -- numeric settings step without typing (within the same bounds `parse`
  * already enforces) and every setting with a `defaultValue` can be reset in one action (typing the literal "default",
  * mirroring the existing keybinding-reset convention).
  */
class CommandSurfaceItemInputItemSpec extends AnyFlatSpec with Matchers:

  private def decimalItem(current: String, default: Option[String] = Some("1.00")): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = "test-decimal",
      label = "Test Decimal",
      hint = "Scale (0.0-4.0)",
      currentValue = current,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toDoubleOption
          .filter(v => v >= 0.0 && v <= 4.0)
          .map(v => CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(v)))),
      category = CommandCategory.Settings,
      defaultValue = default
    )

  private def integerItem(current: String, default: Option[String] = Some("50")): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = "test-integer",
      label = "Test Integer",
      hint = "Steps (0-100)",
      currentValue = current,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = false),
      parse = text =>
        text.toIntOption
          .filter(v => v >= 0 && v <= 100)
          .map(v => CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationSteps(v)))),
      category = CommandCategory.Settings,
      defaultValue = default
    )

  "steppedIntent" should "step a decimal value up and down by the decimal step" in {
    decimalItem("1.00").steppedIntent(1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(1.1)))
    )
    decimalItem("1.00").steppedIntent(-1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(0.9)))
    )
  }

  it should "step an integer value up and down by the integer step" in {
    integerItem("50").steppedIntent(1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationSteps(51)))
    )
    integerItem("50").steppedIntent(-1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationSteps(49)))
    )
  }

  it should "not step past the enforced upper or lower bound" in {
    decimalItem("4.00").steppedIntent(1) shouldBe None
    decimalItem("0.00").steppedIntent(-1) shouldBe None
    integerItem("100").steppedIntent(1) shouldBe None
    integerItem("0").steppedIntent(-1) shouldBe None
  }

  it should "return None for a non-numeric current value" in {
    decimalItem("not-a-number").steppedIntent(1) shouldBe None
  }

  private def columnCountItem(current: String): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = "test-column-count",
      label = "Test Column Count",
      hint = "Columns, or auto",
      currentValue = current,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = false),
      parse = text =>
        text.trim.toLowerCase match
          case "auto" | "0" =>
            Some(CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(None))))
          case trimmed =>
            trimmed.toIntOption
              .filter(_ >= 1)
              .map(count =>
                CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(Some(count))))
              ),
      category = CommandCategory.Settings,
      defaultValue = Some("auto"),
      stepFromAuto = Some(1)
    )

  "steppedIntent" should "step up from the Auto sentinel to the value stepFromAuto names" in {
    columnCountItem("auto").steppedIntent(1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(Some(1))))
    )
  }

  it should "stay at Auto when stepping down from it" in {
    columnCountItem("auto").steppedIntent(-1) shouldBe None
  }

  it should "step down from 1 back to Auto" in {
    columnCountItem("1").steppedIntent(-1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(None)))
    )
  }

  it should "step up from a numeric column count normally" in {
    columnCountItem("2").steppedIntent(1) shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(Some(3))))
    )
  }

  it should "not step up from the Auto sentinel when the item has no stepFromAuto" in {
    decimalItem("not-a-number").copy(stepFromAuto = None).steppedIntent(1) shouldBe None
  }

  it should "return None for free-text and binding input kinds" in {
    val freeText = CommandSurfaceItem.InputItem(
      id = "test-free-text",
      label = "Test",
      hint = "Hint",
      currentValue = "hello",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = _ => None,
      category = CommandCategory.Settings
    )
    freeText.steppedIntent(1) shouldBe None
  }

  "parseOrDefault" should "reset to the default value when the text is the literal word \"default\"" in {
    decimalItem("2.50").parseOrDefault("default") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(1.0)))
    )
    decimalItem("2.50").parseOrDefault("DEFAULT") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(1.0)))
    )
  }

  it should "parse the text normally when it is not the reset sentinel" in {
    decimalItem("2.50").parseOrDefault("3.00") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(3.0)))
    )
  }

  it should "fall back to ordinary parse when there is no default value" in {
    decimalItem("2.50", default = None).parseOrDefault("default") shouldBe None
  }

  it should "still honor an item's own literal-\"default\" handling in parse when there is no defaultValue" in {
    // Mirrors CommandRunnerSettingsKeymapItems.parseBindingText: a keybinding row already treats the literal
    // "default" (or "reset") as its own reset convention inside `parse`, entirely independent of `defaultValue` --
    // parseOrDefault must not shadow that with a hard None just because defaultValue is unset.
    val bindingItem = CommandSurfaceItem.InputItem(
      id = "test-binding",
      label = "Test Binding",
      hint = "Binding or default",
      currentValue = "ctrl+k",
      kind = CommandSurfaceItem.InputKind.Binding,
      parse = text =>
        text.trim.toLowerCase match
          case "default" | "reset" =>
            Some(CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationSteps(0))))
          case _ => None,
      category = CommandCategory.Settings
    )
    bindingItem.parseOrDefault("default") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationSteps(0)))
    )
  }

  "isOutOfBounds" should "not flag the reset sentinel or its in-progress prefixes as an error" in {
    val item = decimalItem("2.50")
    item.isOutOfBounds("d") shouldBe false
    item.isOutOfBounds("def") shouldBe false
    item.isOutOfBounds("default") shouldBe false
    item.isOutOfBounds("DEFAULT") shouldBe false
  }

  it should "still flag genuinely invalid text as an error" in {
    decimalItem("2.50").isOutOfBounds("dxyz") shouldBe true
    decimalItem("2.50").isOutOfBounds("9.9") shouldBe true
  }

  "accepts" should "allow letters that keep the typed text a prefix of the reset sentinel" in {
    val item = decimalItem("2.50")
    item.accepts("", 'd') shouldBe true
    item.accepts("d", 'e') shouldBe true
    item.accepts("defaul", 't') shouldBe true
  }

  it should "reject letters that are not a prefix of the reset sentinel" in {
    val item = decimalItem("2.50")
    item.accepts("", 'x') shouldBe false
    item.accepts("d", 'x') shouldBe false
  }

  it should "still accept ordinary numeric characters" in {
    val item = decimalItem("2.50")
    item.accepts("2", '5') shouldBe true
    item.accepts("2", '.') shouldBe true
  }

  it should "reject the reset-sentinel prefix for a field with no default value (e.g. rich-text-font-size)" in {
    val item = decimalItem("18.00", default = None)
    item.accepts("", 'd') shouldBe false
    item.accepts("d", 'e') shouldBe false
  }
