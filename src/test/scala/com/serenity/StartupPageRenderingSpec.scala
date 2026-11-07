package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.StartupPage
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupPageRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "StartupPage Rendering"

  it should "generate the correct render lines structure" in {
    val startupPage = StartupPage(
      title = "What would you like to do?",
      options = List(
        "1. Start a new session",
        "2. Restore an existing session",
        "3. Open an existing file or directory"
      ),
      selectedIndex = 1 // Second option selected
    )

    val renderedLines = startupPage.renderLines

    // Note: Selection highlighting is now handled in the renderer, not in renderLines
    renderedLines should contain allElementsOf List(
      "What would you like to do?",
      "",
      "1. Start a new session",
      "2. Restore an existing session",
      "3. Open an existing file or directory"
    )
  }

  private val longNotice =
    "Skia could not load, so Serenity is painting with Java2D. The dynamic loader cannot find libGL.so.1. " +
      "Put the directories that hold it on LD_LIBRARY_PATH before launching (see \"Skia on NixOS\" in " +
      "DEVELOPMENT.md). Set ui.render.backend = java2d to stop this notice."

  it should "break a status message that is wider than the window into rows that fit, losing no word" in {
    val page    = StartupPage(title = "Welcome", options = List("New document"), statusMessage = Some(longNotice))
    val lines   = page.renderLinesWithin(widthCells = 50)
    val message = lines.slice(4, lines.size - 2)

    message.size should be > 1
    all(message.map(_.length)) should be <= 50
    message.mkString(" ") shouldBe longNotice
  }

  it should "keep the actions where they were and the footer last when the status message wraps" in {
    val page    = StartupPage(title = "Welcome", options = List("New document"), statusMessage = Some(longNotice))
    val wrapped = page.renderLinesWithin(widthCells = 40)

    wrapped.take(3) shouldBe page.renderLines.take(3)
    wrapped.last shouldBe page.renderLines.last
    page.actionLineIndices shouldBe List(2)
    wrapped.size should be > page.renderLines.size
  }

  it should "not change the lines when the window is wide enough for the status message" in {
    val page = StartupPage(title = "Welcome", options = List("New document"), statusMessage = Some(longNotice))
    page.renderLinesWithin(widthCells = longNotice.length + 8) shouldBe page.renderLines
  }

  it should "maintain consistent line structure regardless of selection" in {
    val startupPage = StartupPage(
      title = "Test Title",
      options = List("Option A", "Option B", "Option C"),
      selectedIndex = 0 // Default
    )

    val renderedLines = startupPage.renderLines

    renderedLines should contain allElementsOf List(
      "Test Title",
      "",
      "Option A",
      "Option B",
      "Option C"
    )
  }

  it should "properly handle option wrapping navigation" in {
    val startupPage = StartupPage(
      title = "Test",
      options = List("A", "B", "C"),
      selectedIndex = 0
    )

    // Test moving down from first to second
    val afterDown = startupPage.moveSelectionDown
    afterDown.selectedIndex shouldBe 1

    // Test moving down from last to first (wrap around)
    val lastSelected  = startupPage.copy(selectedIndex = 2)
    val afterWrapDown = lastSelected.moveSelectionDown
    afterWrapDown.selectedIndex shouldBe 0

    // Test moving up from first to last (wrap around)
    val afterWrapUp = startupPage.moveSelectionUp
    afterWrapUp.selectedIndex shouldBe 2
  }
