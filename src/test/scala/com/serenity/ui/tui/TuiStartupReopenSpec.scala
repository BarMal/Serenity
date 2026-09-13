package com.serenity.ui.tui

import com.serenity.state.models.Modal

/** #-repro: from the startup splash, opening the file dialog, dismissing it with Escape, and opening it again left the
  * screen blank until the next keystroke -- the reopened dialog's state was correct, but the runtime render phase never
  * painted the frame. Drives the real `AppRuntime` input + render decision (`runtimeScreen`) and contrasts it with a
  * forced full paint (`screen`) to separate "state wrong" from "frame not painted".
  */
class TuiStartupReopenSpec extends TuiSpec:

  private def isFileDialog(state: com.serenity.state.models.AppState): Boolean =
    state.topModal.map(_.modal).exists {
      case Modal.FileWorkflow(_) => true
      case _                     => false
    }

  "the startup splash" should "repaint the open-file dialog when it is reopened after Escape" in runTuiStartPage {
    for
      _             <- arrowDown // "New document" -> "Open file or folder"
      _             <- enter     // open the file dialog
      afterFirst    <- state
      firstRuntime  <- runtimeScreen
      _             <- escape    // dismiss back to the splash
      afterEscape   <- state
      _             <- enter     // reopen (selection stays on "Open file or folder")
      afterSecond   <- state
      secondRuntime <- runtimeScreen
    yield
      // State is correct on every step (matches the live-session log): the bug was purely in the render scene cache.
      isFileDialog(afterFirst) shouldBe true
      afterEscape.topModal shouldBe None
      isFileDialog(afterSecond) shouldBe true
      // First open paints the dialog.
      firstRuntime.containsText("Path") shouldBe true
      // The reopened dialog must paint too -- before the fix the scene cache returned the first (dismissed) dialog's
      // scene, whose node referenced a surface id no longer on the modal stack, so the frame came out blank.
      secondRuntime.containsText("Path") shouldBe true
  }
