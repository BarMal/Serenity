package com.serenity.ui.tui

import com.serenity.state.models.SurfaceContent

import TuiScenarios.*

/** Driving the command palette entirely from the keyboard and the mouse, through the terminal: it opens, it filters as
  * you type, the selection moves, a command runs and its effect shows up in the cells, and dismissing it puts back
  * exactly what was underneath.
  */
class TuiCommandPaletteSpec extends TuiSpec:

  private def selectedIndex(screenState: com.serenity.state.models.AppState): Option[Int] =
    screenState.commandRunnerSurface.map(_.content).collect {
      case SurfaceContent.CommandPalette(runner) =>
        runner.selectedIndex
    }

  private def selectedItemIsCommand(screenState: com.serenity.state.models.AppState): Option[Boolean] =
    screenState.commandRunnerSurface.map(_.content).collect {
      case SurfaceContent.CommandPalette(runner) =>
        runner.selectedItem.exists {
          case _: com.serenity.command.CommandSurfaceItem.CommandItem => true
          case _                                                      => false
        }
    }

  "Ctrl+P" should "open the palette over the document, with its search field and key hints" in runTui() {
    for
      _ <- openCommandPalette
      _ <- verify("palette") { screen =>
        screen.containsText("search:") shouldBe true
        screen.containsText("↑↓ navigate • Enter run • Esc dismiss") shouldBe true
        // The document underneath is still drawn: the palette is an overlay, not a screen replacement.
        screen.statusBar should include("scratch.md")
      }
      _ <- verifyState("surface")(current => current.commandRunnerSurface should not be empty)
    yield ()
  }

  "typing in the palette" should "narrow the visible commands as each character arrives" in runTui() {
    for
      _    <- openCommandPalette
      all  <- screen
      _    <- typeSlowly("line")
      some <- screen
      _ <- verify("filtered") { screen =>
        screen.containsText("search: line") shouldBe true
        // issue #1048: direct command matches ("Go to Line", "Toggle Line ...") now rank above settings-group
        // breadcrumb matches, so a line-related command is what actually shows for this query.
        screen.containsText("Toggle Line") shouldBe true
      }
    yield
      // Filtering must actually reduce what is offered, not merely reorder it.
      val listedBefore = all.rowsContaining("•").size + all.paintedRows.size
      some.paintedRows.size should be <= listedBefore
  }

  it should "leave the document underneath untouched while it filters" in
    runTui(TuiEnvironment.withFile("underneath")) {
      for
        before <- screen
        _      <- searchCommands("line")
        during <- screen
      yield
        during.rowText(1) shouldBe before.rowText(1)
        during.statusBar shouldBe before.statusBar
    }

  "the arrow keys" should "move the selection onto the next command, repainting the list" in runTui() {
    for
      _      <- searchCommands("line")
      before <- settledScreen
      first  <- state
      _      <- arrowDown
      after  <- settledScreen
      second <- state
    yield
      selectedIndex(first) shouldBe Some(0)
      selectedIndex(second) shouldBe Some(1)
      // issue #1048: direct command matches now rank above settings-group breadcrumbs, so both the first and
      // second result for "line" are commands (e.g. "Go to Line", "Toggle Line Numbers"), not settings groups.
      selectedItemIsCommand(first) shouldBe Some(true)
      selectedItemIsCommand(second) shouldBe Some(true)
      // Whatever it repaints, it must not disturb the document underneath.
      after.changedRows(before) should not contain 0
      after.statusBar shouldBe before.statusBar
  }

  "Enter" should "run the selected command, with the effect visible in the document's own cells" in
    runTui(TuiEnvironment.withFile("gutter check")) {
      for
        before <- screen
        _      <- runCommand("toggle line")
        after  <- screen
        _ <- verifyState("config")(current => current.persisted.config.surfaceConfig.showLineNumbers shouldBe false)
      yield
        before.rowText(1).stripTrailing shouldBe " 1 gutter check"
        // With line numbers off the gutter is gone and the text starts at the left edge.
        after.rowText(1).stripTrailing shouldBe "gutter check"
    }

  "Escape" should "dismiss the palette and restore the cells it was covering" in
    runTui(TuiEnvironment.withFile("covered content")) {
      for
        before <- settledScreen
        _      <- openCommandPalette
        opened <- screen
        _      <- escape
        after  <- settledScreen
        _      <- verifyState("closed")(current => current.commandRunnerSurface shouldBe empty)
      yield
        opened.containsText("search:") shouldBe true
        after.containsText("search:") shouldBe false
        after.changedCells(before) shouldBe empty
    }

  "a mouse click on a palette row" should "activate that row, the same as selecting it and pressing Enter" in
    runTui(TuiEnvironment.withFile("gutter check")) {
      for
        // issue #1048: "line" now ranks the "Toggle Line Numbers" command above settings-group breadcrumbs, so
        // clicking it (rather than the "Editor View" breadcrumb this used to fall back to) is what exercises
        // "a click activates the row the same as Enter would".
        _      <- searchCommands("line")
        listed <- screen
        row    = listed.rowOf("Toggle Line").getOrElse(fail("expected the Toggle Line Numbers command row"))
        column = listed.rowText(row).indexOf("Toggle Line")
        before <- screen
        _      <- click(column + 1, row)
        after  <- screen
        _ <- verifyState("config")(current => current.persisted.config.surfaceConfig.showLineNumbers shouldBe false)
      yield
        before.rowText(1).stripTrailing shouldBe " 1 gutter check"
        after.rowText(1).stripTrailing shouldBe "gutter check"
    }

  "reopening the palette" should "start from a clean search rather than the previous query" in runTui() {
    for
      _ <- searchCommands("line")
      _ <- escape
      _ <- openCommandPalette
      _ <- verify("clean search") { screen =>
        screen.containsText("search: line") shouldBe false
        screen.containsText("search:") shouldBe true
      }
    yield ()
  }

  "a query matching nothing" should "leave the palette open, showing the query and no commands" in runTui() {
    for
      _ <- searchCommands("zzzznotacommand")
      _ <- verify("no matches") { screen =>
        screen.containsText("search: zzzznotacommand") shouldBe true
        // No matches means no rows to navigate, so the list and its key hints are gone rather than shown empty.
        screen.containsText("Enter run") shouldBe false
        screen.statusBar should include("scratch.md")
      }
      _ <- verifyState("still open")(current => current.commandRunnerSurface should not be empty)
    yield ()
  }
end TuiCommandPaletteSpec
