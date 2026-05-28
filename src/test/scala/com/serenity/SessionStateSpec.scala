package com.serenity

import java.nio.file.Files

import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionStateSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SessionState" should "restore clean file-backed buffers from disk content" in {
    val tempFile = Files.createTempFile("session-state-clean", ".txt")
    Files.writeString(tempFile, "content from disk")

    val buffer = Buffer.fromFile(BufferId(7), tempFile, "content from disk")
    val appState = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(8),
      nextPaneId = PaneId(1)
    )

    val restoredState =
      SessionState.toAppState(SessionState.fromAppState(appState), Theme.default)

    restoredState.buffers(buffer.id).content.toString.shouldBe("content from disk")
    restoredState.buffers(buffer.id).filePath.shouldBe(Some(tempFile))
    restoredState.buffers(buffer.id).isDirty.shouldBe(false)
  }

  it should "restore dirty file-backed buffers from unsaved in-memory content" in {
    val tempFile = Files.createTempFile("session-state-dirty", ".txt")
    Files.writeString(tempFile, "saved on disk")

    val buffer = Buffer
      .fromFile(BufferId(9), tempFile, "unsaved in memory")
      .copy(isDirty = true)
    val appState = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(10),
      nextPaneId = PaneId(1)
    )

    val restoredState =
      SessionState.toAppState(SessionState.fromAppState(appState), Theme.default)

    restoredState.buffers(buffer.id).content.toString.shouldBe("unsaved in memory")
    restoredState.buffers(buffer.id).filePath.shouldBe(Some(tempFile))
    restoredState.buffers(buffer.id).isDirty.shouldBe(true)
  }

  it should "discard dirty buffer content when persistUnsavedBuffers is false" in {
    val tempFile = Files.createTempFile("session-state-no-persist", ".txt")
    Files.writeString(tempFile, "saved on disk")

    val buffer = Buffer
      .fromFile(BufferId(11), tempFile, "unsaved in memory")
      .copy(isDirty = true)
    val appState = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(12),
      nextPaneId = PaneId(1)
    )

    val sessionState = SessionState.fromAppState(appState, persistUnsaved = false)
    val sessionBuffer = sessionState.buffers.find(_.id == buffer.id.value).get

    sessionBuffer.unsavedContent shouldBe None
    sessionBuffer.isDirty shouldBe true
  }

  it should "preserve clean buffer content when persistUnsavedBuffers is false" in {
    val tempFile = Files.createTempFile("session-state-clean-persist", ".txt")
    Files.writeString(tempFile, "saved on disk")

    val buffer = Buffer.fromFile(BufferId(13), tempFile, "saved on disk")
    val appState = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(14),
      nextPaneId = PaneId(1)
    )

    val sessionState = SessionState.fromAppState(appState, persistUnsaved = false)
    val sessionBuffer = sessionState.buffers.find(_.id == buffer.id.value).get

    sessionBuffer.unsavedContent shouldBe Some("saved on disk")
    sessionBuffer.isDirty shouldBe false
  }
