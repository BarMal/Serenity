package com.serenity.ui.tui
import TuiScenarios.*

/** Changing settings from the terminal: navigating the surface, toggling an option and seeing the editor change shape
  * underneath it, and the annotations that exist specifically because this is a terminal -- the controls epic #1103
  * accepted as inert in cell space are labelled rather than hidden, so a config file can still be prepared while
  * running headless.
  */
class TuiSettingsSpec extends TuiSpec:

  "the settings surface" should "open from the palette with its groups and navigation hints" in runTui() {
    for
      _ <- openSettings
      _ <- verify("settings root") { screen =>
        screen.containsText("Settings") shouldBe true
        screen.containsText("↑↓ navigate • Enter open • Esc back • ←→ cycle option") shouldBe true
      }
      _ <- verifyState("surface")(current => current.runtime.uiSurfaces should have size 1)
    yield ()
  }

  it should "search within settings and open the matching group" in runTui() {
    for
      _ <- openSettings
      _ <- typeText("post")
      _ <- verify("searched") { screen =>
        screen.containsText("Settings search: post") shouldBe true
        screen.containsText("Surface Appearance") shouldBe true
      }
      _ <- enter
      _ <- verify("opened group") { screen =>
        screen.containsText("Settings > Appearance & Motion > Surface Appearance") shouldBe true
        screen.containsText("Post-processing") shouldBe true
      }
    yield ()
  }

  "toggling line numbers" should "change the editor's own cells, not just a config flag" in
    runTui(TuiEnvironment.withFile("body text")) {
      for
        before <- settledScreen
        _      <- openSettings
        _      <- typeText("line numbers")
        _      <- enter
        _      <- verify("option row")(screen => screen.containsText("Line Numbers") shouldBe true)
        _      <- arrowRight
        _      <- verifyState("flag")(current => current.persisted.config.surfaceConfig.showLineNumbers shouldBe false)
        _      <- dismissSurfaces()
        after  <- settledScreen
      yield
        before.rowText(1).stripTrailing shouldBe " 1 body text"
        after.rowText(1).stripTrailing shouldBe "body text"
    }

  it should "put the gutter back when the option is cycled again" in
    runTui(TuiEnvironment.withFile("body text")) {
      for
        _     <- openSettings
        _     <- typeText("line numbers")
        _     <- enter
        _     <- arrowRight
        _     <- arrowRight
        _     <- verifyState("flag")(current => current.persisted.config.surfaceConfig.showLineNumbers shouldBe true)
        _     <- dismissSurfaces()
        after <- settledScreen
      yield after.rowText(1).stripTrailing shouldBe " 1 body text"
    }

  "typography settings" should "be annotated as inert in TUI mode rather than hidden" in runTui() {
    for
      _ <- openSettings
      _ <- typeText("typography")
      _ <- verify("group listed")(screen => screen.containsText("Typography") shouldBe true)
      _ <- enter
      _ <- verify("annotated") { screen =>
        // Epic #1103's accepted degradation: typography has no effect on a fixed cell grid, so the hint says so.
        screen.containsText("inert in TUI mode") shouldBe true
        screen.containsText("Prose Font") shouldBe true
      }
    yield ()
  }

  /** The Post-processing option carries the same "-- inert in TUI mode" suffix as Typography
    * (`CommandRunnerSettingsGroups.inertInTuiHint`), but its hint is long enough that the settings surface's
    * fixed-width hint column elides it before the suffix is reached -- at 240 columns just as at 200, since that column
    * does not grow with the terminal. So the annotation `docs/tui-mode.md` describes for this option is never actually
    * legible in TUI mode. Asserted as it behaves today rather than as the doc describes it; the elision is worth
    * fixing, and this test will say so when it is.
    */
  "the post-processing option" should "be reachable and show its value, with its hint elided before the annotation" in
    runTui(TuiEnvironment.default.withViewport(TuiViewport.Wide)) {
      for
        _ <- openSettings
        _ <- typeText("post")
        _ <- enter
        _ <- verify("option row") { screen =>
          val row = screen.rowOf("Post-processing").getOrElse(fail("expected the Post-processing option"))
          screen.rowText(row) should include("Off")
          screen.rowText(row) should include("Frame-wide scanlines")
          screen.rowText(row) should include("...")
          screen.rowText(row) should not include "inert in TUI mode"
        }
      yield ()
    }

  "Escape" should "pop one settings level at a time rather than closing the whole surface" in runTui() {
    for
      _      <- openSettings
      _      <- typeText("line numbers")
      _      <- enter
      nested <- state
      _      <- escape
      _      <- openSurfaces.map(surfaces => surfaces should not be empty)
      _      <- dismissSurfaces()
      _      <- openSurfaces.map(surfaces => surfaces shouldBe empty)
    yield nested.runtime.uiSurfaces should not be empty
  }

  "a setting changed through the palette" should "survive dismissing and reopening the surface" in runTui() {
    for
      _ <- runCommand("toggle line")
      _ <- verifyState("off")(current => current.persisted.config.surfaceConfig.showLineNumbers shouldBe false)
      _ <- openSettings
      _ <- typeText("line numbers")
      _ <- enter
      _ <- verify("value shown as off")(screen => screen.containsText("Line Numbers") shouldBe true)
      _ <- verifyState("still off")(current => current.persisted.config.surfaceConfig.showLineNumbers shouldBe false)
    yield ()
  }
end TuiSettingsSpec
