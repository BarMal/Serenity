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
    val lastSelected = startupPage.copy(selectedIndex = 2)
    val afterWrapDown = lastSelected.moveSelectionDown
    afterWrapDown.selectedIndex shouldBe 0
    
    // Test moving up from first to last (wrap around)
    val afterWrapUp = startupPage.moveSelectionUp
    afterWrapUp.selectedIndex shouldBe 2
  }