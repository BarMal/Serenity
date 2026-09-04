package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.ui.layout.ViewportSize

import TuiScenarios.*

/** The same session at different terminal sizes, and across the resizes a window manager puts it through. A TUI has no
  * window of its own to negotiate with: whatever the terminal is, the layout has to work in it -- from a full-screen
  * high-DPI terminal down to something barely taller than its own chrome.
  */
class TuiViewportSpec extends TuiSpec:

  private val document = "alpha\nbeta\ngamma\ndelta"

  "a resize" should "re-lay out at the new size, keeping the document and caret" in
    runTui(TuiEnvironment.withFile(document)) {
      for
        _ <- lineEnd
        _ <- resize(TuiViewport.HalfScreen)
        _ <- verify("half screen") { screen =>
          screen.width shouldBe 100
          screen.height shouldBe 56
          screen.rowText(1).stripTrailing shouldBe " 1 alpha"
          screen.caret shouldBe (8, 1)
          screen.statusBar should include("Line 1, Col 6")
        }
        _ <- verifyState("viewport")(current => current.runtime.viewportSize shouldBe Some(TuiViewport.HalfScreen))
      yield ()
    }

  it should "survive a sequence of resizes, ending where the last one left it" in
    runTui(TuiEnvironment.withFile(document)) {
      for
        _ <- List(TuiViewport.Small, TuiViewport.Wide, TuiViewport.Tiny, TuiViewport.Default).traverse_(resize)
        _ <- verify("back to default") { screen =>
          screen.width shouldBe 200
          screen.height shouldBe 56
          screen.rowText(1).stripTrailing shouldBe " 1 alpha"
          screen.rowText(4).stripTrailing shouldBe " 4 delta"
        }
      yield ()
    }

  it should "keep editing working after the terminal has changed size" in
    runTui(TuiEnvironment.withFile("before")) {
      for
        _    <- resize(TuiViewport.Small)
        _    <- lineEnd
        _    <- typeText(" and after")
        text <- documentText
        _ <- verify("edited at the new size") { screen =>
          screen.width shouldBe 80
          screen.rowText(1).stripTrailing shouldBe " 1 before and after"
        }
      yield text shouldBe Some("before and after")
    }

  "a very small terminal" should "still draw chrome and as much of the document as fits" in
    runTui(TuiEnvironment.withLines(40).withViewport(TuiViewport.Tiny)) {
      verify("tiny terminal") { screen =>
        screen.width shouldBe 40
        screen.height shouldBe 10
        screen.titleBar should include("scratch.md")
        screen.statusBar should include("Line 1")
        screen.rowText(1) should include("line 0")
        // Only the rows that exist can be drawn; the rest of the document is simply not on screen.
        screen.containsText("line 39") shouldBe false
      }
    }

  it should "keep the status bar readable when the terminal is narrower than its full text" in
    runTui(TuiEnvironment.withFile(document, name = "a-rather-long-file-name.md").withViewport(TuiViewport.Tiny)) {
      verify("narrow status bar") { screen =>
        screen.statusBar.length shouldBe 40
        screen.statusBar.strip should not be empty
        // Whatever is elided, the bar must not spill into the row above it.
        screen.rowText(screen.height - 2).strip should not include "Line 1"
      }
    }

  "a wide terminal" should "use the extra columns for content rather than stretching the gutter" in
    runTui(TuiEnvironment.withFile("x" * 300).withViewport(TuiViewport.Wide)) {
      verify("wide terminal") { screen =>
        screen.width shouldBe 240
        screen.rowText(1).take(3) shouldBe " 1 "
        screen.rowText(1).count(_ == 'x') shouldBe 237
        screen.rowsContaining("x") should have size 2
      }
    }

  "the start page" should "stay centred across a resize" in runTuiStartPage {
    for
      _ <- verify("default")(screen => screen.rowOf("Welcome to Serenity") should not be empty)
      _ <- resize(TuiViewport.Small)
      _ <- verify("small") { screen =>
        val row    = screen.rowOf("Welcome to Serenity").getOrElse(fail("welcome text vanished after resize"))
        val indent = screen.rowText(row).indexOf("Welcome")
        // Centred means the left margin is roughly half the slack, not a fixed indent carried over from 200 columns.
        indent should be < 40
        indent should be > 10
      }
    yield ()
  }

  "an overlay" should "be re-laid out for the new terminal rather than clipped at its old size" in
    runTui(TuiEnvironment.withFile(document)) {
      for
        _ <- openCommandPalette
        _ <- resize(TuiViewport.Small)
        _ <- verify("palette at the new size") { screen =>
          screen.width shouldBe 80
          screen.containsText("search:") shouldBe true
          screen.paintedRows.foreach((_, line) => line.length should be <= 80)
        }
      yield ()
    }

  "a terminal that reports an odd size" should "be laid out without error" in
    runTui(TuiEnvironment.withFile(document).withViewport(ViewportSize(37, 11))) {
      verify("odd size") { screen =>
        screen.width shouldBe 37
        screen.height shouldBe 11
        screen.rowText(1) should include("alpha")
      }
    }
end TuiViewportSpec
