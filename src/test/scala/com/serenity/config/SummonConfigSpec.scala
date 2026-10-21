package com.serenity.config

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SummonConfigSpec extends AnyFlatSpec with Matchers:

  private val default = SummonConfig()

  "The default summon chord" should "be Alt+Shift with L, S, T, B and H and Space to lock" in {
    default.modifiers shouldBe Set(Modifier.Alt, Modifier.Shift)
    default.lock shouldBe HotkeyTrigger(InputKey.Character, Some(' '), Set.empty)
    ChromeElement.values.map(element => element -> default.letterOf(element)).toList shouldBe List(
      ChromeElement.LineNumbers -> Some('l'),
      ChromeElement.StatusBar   -> Some('s'),
      ChromeElement.TabStrip    -> Some('t'),
      ChromeElement.Scrollbar   -> Some('b'),
      ChromeElement.PaneHeaders -> Some('h')
    )
  }

  it should "collide with no default hotkey, global, command or focused keymap, on any platform" in {
    for osName <- List("linux", "Mac OS X", "Windows 11") do
      val hotkeys = HotkeyConfig.forOs(osName)
      withClue(s"$osName: ")(default.settled(hotkeys, FocusedKeymapConfig())._2 shouldBe Nil)
  }

  it should "leave the config untouched when nothing collides" in {
    default.settled(HotkeyConfig.forOs("linux"), FocusedKeymapConfig())._1 shouldBe default
  }

  "Summon modifiers" should "parse as a plus-separated modifier set" in {
    SummonConfig.parseModifiers("alt+shift") shouldBe Some(Set(Modifier.Alt, Modifier.Shift))
    SummonConfig.parseModifiers(" Shift + ALT ") shouldBe Some(Set(Modifier.Alt, Modifier.Shift))
    SummonConfig.parseModifiers("ctrl+alt") shouldBe Some(Set(Modifier.Ctrl, Modifier.Alt))
    SummonConfig.parseModifiers("cmd+shift") shouldBe Some(Set(Modifier.Meta, Modifier.Shift))
  }

  it should "refuse an empty set, a key that is not a modifier and a single bare Shift" in {
    SummonConfig.parseModifiers("") shouldBe None
    SummonConfig.parseModifiers("alt+l") shouldBe None
    SummonConfig.parseModifiers("l") shouldBe None
    SummonConfig.parseModifiers("shift") shouldBe None
  }

  it should "render in the order hotkeys render modifiers, and read back what was rendered" in {
    SummonConfig.renderModifiers(Set(Modifier.Shift, Modifier.Alt)) shouldBe "alt+shift"
    SummonConfig.renderModifiers(Set(Modifier.Meta, Modifier.Ctrl)) shouldBe "ctrl+meta"
    for mods <- List(Set(Modifier.Alt), Set(Modifier.Ctrl, Modifier.Shift), Set(Modifier.Meta, Modifier.Alt)) do
      SummonConfig.parseModifiers(SummonConfig.renderModifiers(mods)) shouldBe Some(mods)
  }

  "A summon letter" should "be one letter or digit, or off" in {
    SummonConfig.parseLetter("l") shouldBe Some(Some('l'))
    SummonConfig.parseLetter(" L ") shouldBe Some(Some('l'))
    SummonConfig.parseLetter("7") shouldBe Some(Some('7'))
    SummonConfig.parseLetter("off") shouldBe Some(None)
    SummonConfig.parseLetter("ll") shouldBe None
    SummonConfig.parseLetter("") shouldBe None
    SummonConfig.parseLetter("space") shouldBe None
    SummonConfig.parseLetter("-") shouldBe None
  }

  it should "render as the letter, or off" in {
    SummonConfig.renderLetter(Some('l')) shouldBe "l"
    SummonConfig.renderLetter(None) shouldBe "off"
  }

  "The lock key" should "parse space, a letter, enter, tab and backspace, and nothing with a modifier" in {
    SummonConfig.parseLock("space") shouldBe Some(HotkeyTrigger(InputKey.Character, Some(' '), Set.empty))
    SummonConfig.parseLock("x") shouldBe Some(HotkeyTrigger(InputKey.Character, Some('x'), Set.empty))
    SummonConfig.parseLock("enter") shouldBe Some(HotkeyTrigger(InputKey.Enter, None, Set.empty))
    SummonConfig.parseLock("tab") shouldBe Some(HotkeyTrigger(InputKey.Tab, None, Set.empty))
    SummonConfig.parseLock("backspace") shouldBe Some(HotkeyTrigger(InputKey.Backspace, None, Set.empty))
    SummonConfig.parseLock("ctrl+space") shouldBe None
    SummonConfig.parseLock("alt") shouldBe None
    SummonConfig.parseLock("f5") shouldBe None
    SummonConfig.parseLock("") shouldBe None
  }

  it should "render space as space and read back what it rendered" in {
    SummonConfig.renderLock(default.lock) shouldBe "space"
    val locks = default.lock :: List("x", "enter").flatMap(SummonConfig.parseLock)
    locks.foreach(lock => SummonConfig.parseLock(SummonConfig.renderLock(lock)) shouldBe Some(lock))
  }

  "A hotkey trigger" should "parse and render space like any other key" in {
    HotkeyTrigger.parse("ctrl+space") shouldBe Some(HotkeyTrigger(InputKey.Character, Some(' '), Set(Modifier.Ctrl)))
    HotkeyTrigger(InputKey.Character, Some(' '), Set(Modifier.Ctrl)).render shouldBe "ctrl+space"
  }

  "Matching a keystroke to a chord" should "find the element for the base modifiers and its letter" in {
    def stroke(char: Char, mods: Modifier*) = KeyStrokeInfo(InputKey.Character, Some(char), mods.toSet)
    default.elementFor(stroke('l', Modifier.Alt, Modifier.Shift)) shouldBe Some(ChromeElement.LineNumbers)
    default.elementFor(stroke('h', Modifier.Shift, Modifier.Alt)) shouldBe Some(ChromeElement.PaneHeaders)
  }

  it should "read a legacy terminal's capital letter under Alt as Alt+Shift+letter" in {
    default.elementFor(KeyStrokeInfo(InputKey.Character, Some('S'), Set(Modifier.Alt))) shouldBe
      Some(ChromeElement.StatusBar)
  }

  it should "ignore other modifiers, other letters and a letter bound to nothing" in {
    def stroke(char: Char, mods: Modifier*) = KeyStrokeInfo(InputKey.Character, Some(char), mods.toSet)
    default.elementFor(stroke('l', Modifier.Alt)) shouldBe None
    default.elementFor(stroke('l', Modifier.Ctrl, Modifier.Alt, Modifier.Shift)) shouldBe None
    default.elementFor(stroke('a', Modifier.Alt, Modifier.Shift)) shouldBe None
    default.withLetter(ChromeElement.LineNumbers, None).elementFor(stroke('l', Modifier.Alt, Modifier.Shift)) shouldBe
      None
    default.elementFor(KeyStrokeInfo(InputKey.Enter, None, Set(Modifier.Alt, Modifier.Shift))) shouldBe None
  }

  "Settling a config against the hotkeys" should "unbind a letter whose chord a hotkey holds, and report it" in {
    val taken   = HotkeyTrigger(InputKey.Character, Some('l'), Set(Modifier.Alt, Modifier.Shift))
    val hotkeys = HotkeyConfig.forOs("linux").withCommandBinding("toggle-line-numbers", taken.render)
    hotkeys.commandBindingsFor("toggle-line-numbers") shouldBe List(taken)

    val (settled, conflicts) = default.settled(hotkeys, FocusedKeymapConfig())

    settled.letterOf(ChromeElement.LineNumbers) shouldBe None
    settled.letterOf(ChromeElement.StatusBar) shouldBe Some('s')
    conflicts shouldBe List(
      HotkeyConflict("alt+shift+l", "command.toggle-line-numbers", List("ui.summon.line_numbers"))
    )
  }

  it should "unbind a letter whose chord a global action holds" in {
    val hotkeys              = HotkeyConfig.forOs("linux").withBinding(HotkeyAction.Find, "alt+shift+s")
    val (settled, conflicts) = default.settled(hotkeys, FocusedKeymapConfig())

    settled.letterOf(ChromeElement.StatusBar) shouldBe None
    conflicts.map(_.droppedFrom) shouldBe List(List("ui.summon.status_bar"))
    conflicts.map(_.keptBy) shouldBe List("find")
  }

  it should "leave a letter alone whose chord only differs by modifiers" in {
    val hotkeys = HotkeyConfig.forOs("linux").withBinding(HotkeyAction.Find, "ctrl+alt+shift+s")
    default.settled(hotkeys, FocusedKeymapConfig())._2 shouldBe Nil
  }

  it should "give a letter two elements share to the first element only" in {
    val doubled              = default.withLetter(ChromeElement.Scrollbar, Some('l'))
    val (settled, conflicts) = doubled.settled(HotkeyConfig.forOs("linux"), FocusedKeymapConfig())

    settled.letterOf(ChromeElement.LineNumbers) shouldBe Some('l')
    settled.letterOf(ChromeElement.Scrollbar) shouldBe None
    conflicts shouldBe List(HotkeyConflict("alt+shift+l", "ui.summon.line_numbers", List("ui.summon.scrollbar")))
  }

  it should "put a lock key that is also a summon letter back to space" in {
    val clashing             = default.copy(lock = SummonConfig.parseLock("l").getOrElse(fail("l parses")))
    val (settled, conflicts) = clashing.settled(HotkeyConfig.forOs("linux"), FocusedKeymapConfig())

    settled.lock shouldBe SummonConfig.DefaultLock
    conflicts.map(_.droppedFrom) shouldBe List(List("ui.summon.lock"))
  }

  "Validating a change from the settings list" should "refuse a letter another element holds, a hotkey holds or the lock holds" in {
    val hotkeys = HotkeyConfig.forOs("linux").withBinding(HotkeyAction.Find, "alt+shift+q")
    val keymaps = FocusedKeymapConfig()
    default.withLetterChecked(ChromeElement.Scrollbar, Some('l'), hotkeys, keymaps) shouldBe None
    default.withLetterChecked(ChromeElement.Scrollbar, Some('q'), hotkeys, keymaps) shouldBe None
    default
      .withLetterChecked(ChromeElement.Scrollbar, Some('x'), hotkeys, keymaps)
      .map(_.letterOf(ChromeElement.Scrollbar)) shouldBe
      Some(Some('x'))
    default
      .withLetterChecked(ChromeElement.Scrollbar, None, hotkeys, keymaps)
      .map(_.letterOf(ChromeElement.Scrollbar)) shouldBe
      Some(None)
    default.withLockChecked(SummonConfig.parseLock("l").getOrElse(fail("l parses"))) shouldBe None
    default.withModifiersChecked(Set(Modifier.Ctrl, Modifier.Alt), hotkeys, keymaps) should not be None
  }
