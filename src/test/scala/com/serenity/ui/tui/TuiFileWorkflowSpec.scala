package com.serenity.ui.tui
import TuiScenarios.*

/** Opening, saving and juggling files from a terminal -- including the save-as form, which exists specifically because
  * a TUI session has no native file dialog (`TuiRuntime` passes `fileDialog = None`, so `StateManager` falls back to
  * the in-app form, issue #1110).
  */
class TuiFileWorkflowSpec extends TuiSpec:

  "an unsaved edit" should "mark the buffer header, and clear it once Ctrl+S has written the file" in
    runTui(TuiEnvironment.withFile("original")) {
      for
        _        <- lineEnd
        _        <- typeText(" edited")
        modified <- screen
        _        <- save
        saved    <- screen
        onDisk   <- fileContent()
      yield
        modified.titleBar should include("scratch.md - unsaved")
        saved.titleBar should include("scratch.md")
        saved.titleBar should not include "unsaved"
        onDisk shouldBe "original edited"
    }

  it should "write exactly what the document holds, newlines and all" in runTui(TuiEnvironment.withFile("")) {
    for
      _      <- typeDocument("alpha", "beta", "gamma")
      _      <- save
      onDisk <- fileContent()
    yield onDisk shouldBe "alpha\nbeta\ngamma"
  }

  "Ctrl+Shift+S" should "open the in-app save-as form rather than a native dialog" in
    runTui(TuiEnvironment.withFile("save me")) {
      for
        _ <- saveAs
        _ <- verify("save-as form") { screen =>
          screen.containsText("save-as") shouldBe true
          screen.containsText("Filename") shouldBe true
          screen.containsText("Path") shouldBe true
          screen.containsText("Format") shouldBe true
          screen.containsText("Submit enter") shouldBe true
        }
      yield ()
    }

  it should "prefill the form with the current file and its own workspace" in
    runTui(TuiEnvironment.withFile("save me", name = "notes.md")) {
      for
        _         <- saveAs
        current   <- screen
        workspace <- workspacePath("")
      yield
        val filenameRow = current.rowOf("Filename").getOrElse(fail("no filename row"))
        current.rowText(filenameRow) should include("notes.md")
        current.containsText("tui-session") shouldBe true
    }

  it should "be dismissible without writing anything" in runTui(TuiEnvironment.withFile("untouched")) {
    for
      _      <- saveAs
      _      <- escape
      _      <- openSurfaces.map(surfaces => surfaces shouldBe empty)
      onDisk <- fileContent()
    yield onDisk shouldBe "untouched"
  }

  "a new tab" should "open an empty unsaved buffer, leaving the first one intact" in
    runTui(TuiEnvironment.withFile("first file")) {
      for
        _ <- newTab
        _ <- typeText("second buffer")
        _ <- verify("second buffer") { screen =>
          screen.titleBar should include("unsaved")
          screen.rowText(1).stripTrailing shouldBe " 1 second buffer"
          screen.statusBar should include("Not saved to file yet")
        }
        _ <- verifyState("two buffers")(current => current.persisted.bufferOrder should have size 2)
      yield ()
    }

  "Ctrl+Tab" should "switch back to the first buffer, restoring its content and status" in
    runTui(TuiEnvironment.withFile("first file")) {
      for
        _ <- newTab
        _ <- typeText("second buffer")
        _ <- nextTab
        _ <- verify("back on the first file") { screen =>
          screen.titleBar should include("scratch.md")
          screen.rowText(1).stripTrailing shouldBe " 1 first file"
          screen.statusBar should include("scratch.md")
        }
        first <- state
      yield first.focusedBufferId shouldBe first.persisted.bufferOrder.headOption
    }

  it should "cycle forward again to the second buffer" in runTui(TuiEnvironment.withFile("first file")) {
    for
      _ <- newTab
      _ <- typeText("second buffer")
      _ <- nextTab
      _ <- nextTab
      _ <- verify("second buffer again")(screen => screen.rowText(1).stripTrailing shouldBe " 1 second buffer")
    yield ()
  }

  "closing an untouched tab" should "close it outright, leaving the remaining buffer on screen" in
    runTui(TuiEnvironment.withFile("keep me")) {
      for
        _ <- newTab
        _ <- closeTab
        _ <- verifyState("one buffer left")(current => current.persisted.bufferOrder should have size 1)
        _ <- verify("original content")(screen => screen.rowText(1).stripTrailing shouldBe " 1 keep me")
      yield ()
    }

  "closing a tab with unsaved changes" should "ask first, and keep the buffer if the prompt is cancelled" in
    runTui(TuiEnvironment.withFile("keep me")) {
      for
        _ <- newTab
        _ <- typeText("throwaway")
        _ <- closeTab
        _ <- verify("prompt") { screen =>
          screen.containsText("unsaved changes") shouldBe true
          screen.containsText("Save") shouldBe true
          screen.containsText("Close Anyway") shouldBe true
          screen.containsText("Cancel") shouldBe true
        }
        prompt <- screen
        cancelRow = prompt.rowOf("Cancel").getOrElse(fail("expected a Cancel choice"))
        cancelCol = prompt.rowText(cancelRow).indexOf("Cancel")
        _ <- click(cancelCol + 1, cancelRow)
        _ <- verifyState("buffer kept")(current => current.persisted.bufferOrder should have size 2)
      yield ()
    }

  it should "close the buffer when the prompt is answered with Close Anyway" in
    runTui(TuiEnvironment.withFile("keep me")) {
      for
        _      <- newTab
        _      <- typeText("throwaway")
        _      <- closeTab
        prompt <- screen
        closeRow = prompt.rowOf("Close Anyway").getOrElse(fail("expected a Close Anyway choice"))
        closeCol = prompt.rowText(closeRow).indexOf("Close Anyway")
        _ <- click(closeCol + 1, closeRow)
        _ <- verifyState("buffer closed")(current => current.persisted.bufferOrder should have size 1)
        _ <- verify("original content")(screen => screen.rowText(1).stripTrailing shouldBe " 1 keep me")
      yield ()
    }

  "a file opened at startup" should "not be rewritten to disk until the user saves" in
    runTui(TuiEnvironment.withFile("as it was")) {
      for
        _      <- typeText("typed but unsaved")
        onDisk <- fileContent()
      yield onDisk shouldBe "as it was"
    }
end TuiFileWorkflowSpec
