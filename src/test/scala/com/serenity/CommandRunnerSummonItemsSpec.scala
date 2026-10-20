package com.serenity.command

import com.serenity.config.{AppConfig, ChromeElement, HotkeyTrigger, SummonConfig}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The summon chord is configurable from the settings list, base chord included. */
class CommandRunnerSummonItemsSpec extends AnyFlatSpec with Matchers:

  private val ids = List(
    "summon-modifiers",
    "summon-lock",
    "summon-line-numbers",
    "summon-status-bar",
    "summon-tab-strip",
    "summon-scrollbar",
    "summon-pane-headers"
  )

  private def items(config: AppConfig = AppConfig.default) =
    CommandRunnerSettingsInputItems.build(config).filter(_.id.startsWith("summon-"))

  private def item(id: String, config: AppConfig = AppConfig.default) =
    items(config).find(_.id == id).getOrElse(fail(s"no $id row"))

  private def chrome(intent: InterfaceChromeIntent) =
    Some(CommandIntent.Settings(SettingsIntent.InterfaceChrome(intent)))

  "The summon rows" should "be one for the base chord, one for the lock key and one per element" in {
    items().map(_.id) shouldBe ids
  }

  it should "show the chord that is configured" in {
    items().map(_.currentValue) shouldBe List("alt+shift", "space", "l", "s", "t", "b", "h")

    val custom = AppConfig.default.withSummon(
      SummonConfig(
        modifiers = Set(Modifier.Ctrl, Modifier.Alt),
        lock = SummonConfig.parseLock("enter").getOrElse(fail("enter parses"))
      )
        .withLetter(ChromeElement.Scrollbar, None)
    )
    items(custom).map(_.currentValue) shouldBe List("ctrl+alt", "enter", "l", "s", "t", "off", "h")
  }

  it should "offer the shipped chord as each row's default" in {
    items().map(_.defaultValue) shouldBe List(
      Some("alt+shift"),
      Some("space"),
      Some("l"),
      Some("s"),
      Some("t"),
      Some("b"),
      Some("h")
    )
  }

  "The base chord row" should "parse a modifier chord and refuse anything else" in {
    val row = item("summon-modifiers")

    row.parse("ctrl+alt") shouldBe chrome(InterfaceChromeIntent.SetSummonModifiers(Set(Modifier.Ctrl, Modifier.Alt)))
    row.parse("alt+shift") shouldBe chrome(InterfaceChromeIntent.SetSummonModifiers(Set(Modifier.Alt, Modifier.Shift)))
    row.parse("alt+l") shouldBe None
    row.parse("") shouldBe None
  }

  "The lock key row" should "parse a lock key and refuse a chord" in {
    val row = item("summon-lock")

    row.parse("enter") shouldBe chrome(
      InterfaceChromeIntent.SetSummonLock(HotkeyTrigger(InputKey.Enter, None, Set.empty))
    )
    row.parse("space") shouldBe chrome(InterfaceChromeIntent.SetSummonLock(SummonConfig.DefaultLock))
    row.parse("ctrl+space") shouldBe None
  }

  "An element's row" should "parse a letter, or off for no chord" in {
    val row = item("summon-status-bar")

    row.parse("x") shouldBe chrome(InterfaceChromeIntent.SetSummonLetter(ChromeElement.StatusBar, Some('x')))
    row.parse("OFF") shouldBe chrome(InterfaceChromeIntent.SetSummonLetter(ChromeElement.StatusBar, None))
    row.parse("xy") shouldBe None
    row.parse("") shouldBe None
  }

  "The settings tree" should "list the summon rows in a group inside Interface Layout" in {
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = Map.empty,
      inputItems = CommandRunnerSettingsInputItems.build(AppConfig.default),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )
    val layout = groups
      .flatMap(_.children)
      .flatMap(child =>
        child :: (child match
          case group: CommandSurfaceItem.GroupItem => group.children
          case _                                   => Nil)
      )
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-interface-layout" => group }
      .getOrElse(fail("no Interface Layout group"))
    val summon = layout.children
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-summon" => group }
      .getOrElse(fail("no summon group in Interface Layout"))

    summon.children.map(_.id) shouldBe ids
  }

  it should "list them on the terminal too, where the chord toggles" in {
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = Map.empty,
      inputItems = CommandRunnerSettingsInputItems.build(AppConfig.default, FrontendCapabilities.tui()),
      uiPresetPreviews = Nil,
      editingPresetName = None,
      capabilities = FrontendCapabilities.tui()
    )
    val all = groups.flatMap(group =>
      group :: group.children.flatMap(child =>
        child :: (child match
          case nested: CommandSurfaceItem.GroupItem => nested.children
          case _                                    => Nil)
      )
    )

    all.map(_.id) should contain("settings-summon")
  }
