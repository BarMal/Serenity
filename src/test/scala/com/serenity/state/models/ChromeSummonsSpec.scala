package com.serenity.state.models

import com.serenity.config.{AppConfig, ChromeElement, StatusLinePlacement, TabStripMode}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChromeSummonsSpec extends AnyFlatSpec with Matchers:

  import ChromeElement.*

  private val none = ChromeSummons.empty

  "Pressing the chord" should "show the element only while it is held" in {
    val held = none.pressed(LineNumbers)
    held.shows(LineNumbers) shouldBe true
    held.shows(StatusBar) shouldBe false
    held.released(LineNumbers).shows(LineNumbers) shouldBe false
  }

  it should "show several elements at once and release them one at a time" in {
    val both = none.pressed(LineNumbers).pressed(Scrollbar)
    both.released(LineNumbers).shows(Scrollbar) shouldBe true
    both.released(LineNumbers).shows(LineNumbers) shouldBe false
  }

  "Locking while the chord is held" should "keep every held element shown after release" in {
    val locked = none.pressed(LineNumbers).pressed(StatusBar).lockHeld.released(LineNumbers).released(StatusBar)

    locked.shows(LineNumbers) shouldBe true
    locked.shows(StatusBar) shouldBe true
    locked.locked shouldBe Set(LineNumbers, StatusBar)
    locked.held shouldBe Set.empty
  }

  it should "lock nothing when no chord is held" in {
    none.lockHeld shouldBe none
  }

  it should "leave an element not held unlocked" in {
    none.pressed(LineNumbers).lockHeld.pressed(Scrollbar).released(Scrollbar).locked shouldBe Set(LineNumbers)
  }

  "Pressing the chord on a locked element" should "unlock and hide it, and stay hidden while the chord is still held" in {
    val dismissed = none.pressed(LineNumbers).lockHeld.released(LineNumbers).pressed(LineNumbers)

    dismissed.shows(LineNumbers) shouldBe false
    dismissed.locked shouldBe Set.empty
    dismissed.released(LineNumbers).shows(LineNumbers) shouldBe false
  }

  it should "show the element again the next time the chord is pressed" in {
    val again = none.pressed(LineNumbers).lockHeld.released(LineNumbers).pressed(LineNumbers).released(LineNumbers)

    again.pressed(LineNumbers).shows(LineNumbers) shouldBe true
  }

  "A terminal's chord, which cannot report a release," should "toggle: first press shows and locks, second dismisses" in {
    val shown = none.toggled(StatusBar)
    shown.shows(StatusBar) shouldBe true
    shown.locked shouldBe Set(StatusBar)
    shown.toggled(StatusBar).shows(StatusBar) shouldBe false
  }

  it should "dismiss an element that is both held and locked" in {
    none.pressed(StatusBar).lockHeld.toggled(StatusBar).shows(StatusBar) shouldBe false
  }

  "Ending every hold" should "keep what is locked" in {
    val state = none.pressed(LineNumbers).lockHeld.pressed(Scrollbar).releasedAll

    state.held shouldBe Set.empty
    state.locked shouldBe Set(LineNumbers)
  }

  "The effective chrome" should "be the config value when nothing is summoned" in {
    val minimal = EffectiveChrome.of(AppConfig.default, none)
    minimal.lineNumbers shouldBe false
    minimal.paneHeaders shouldBe false
    minimal.scrollbar shouldBe false
    minimal.tabStrip shouldBe false
    minimal.statusLine.isShown shouldBe false

    val conventional = EffectiveChrome.of(AppConfig.conventional, none)
    conventional.lineNumbers shouldBe true
    conventional.paneHeaders shouldBe true
    conventional.scrollbar shouldBe true
    conventional.tabStrip shouldBe true
    conventional.statusLine.isPinned shouldBe true
  }

  it should "add a summoned element to a config that hides it" in {
    val config = AppConfig.default
    EffectiveChrome.of(config, none.pressed(LineNumbers)).lineNumbers shouldBe true
    EffectiveChrome.of(config, none.pressed(PaneHeaders)).paneHeaders shouldBe true
    EffectiveChrome.of(config, none.pressed(Scrollbar)).scrollbar shouldBe true
    EffectiveChrome.of(config, none.pressed(TabStrip)).tabStrip shouldBe true
    EffectiveChrome.of(config, none.pressed(StatusBar)).statusLine.isPinned shouldBe true
  }

  it should "summon only the element asked for" in {
    val chrome = EffectiveChrome.of(AppConfig.default, none.pressed(LineNumbers))
    chrome.paneHeaders shouldBe false
    chrome.scrollbar shouldBe false
    chrome.tabStrip shouldBe false
    chrome.statusLine.isShown shouldBe false
  }

  it should "add a locked element the same way" in {
    EffectiveChrome.of(AppConfig.default, none.toggled(LineNumbers)).lineNumbers shouldBe true
  }

  it should "never hide what the config shows" in {
    val chrome = EffectiveChrome.of(AppConfig.conventional, none.pressed(LineNumbers).lockHeld.pressed(LineNumbers))
    chrome.lineNumbers shouldBe true
    chrome.statusLine.isPinned shouldBe true
  }

  it should "keep a floating status line floating and use the configured segments when summoning an off one" in {
    val floating = AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Floating)
    EffectiveChrome.of(floating, none.pressed(StatusBar)).statusLine shouldBe floating.statusLine

    val off = AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Off)
    EffectiveChrome.of(off, none.pressed(StatusBar)).statusLine.segments shouldBe off.statusLine.segments
  }

  it should "show the status bar with the default segments when the configured list is empty" in {
    val config = AppConfig.default.withStatusLineSegments(Nil)
    EffectiveChrome.of(config, none.pressed(StatusBar)).statusLine.isPinned shouldBe true
    EffectiveChrome.of(config, none.pressed(StatusBar)).statusLine.segments should not be empty
  }

  it should "be the tab strip mode's own answer when the strip is off and nothing is summoned" in {
    EffectiveChrome.of(AppConfig.conventional.withTabStrip(TabStripMode.Off), none).tabStrip shouldBe false
    EffectiveChrome.of(AppConfig.conventional.withTabStrip(TabStripMode.Off), none.pressed(TabStrip)).tabStrip shouldBe
      true
  }
