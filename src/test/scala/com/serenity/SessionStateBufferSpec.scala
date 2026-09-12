package com.serenity

import java.nio.file.Files

import _root_.io.circe.syntax.*
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.session.given
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionStateBufferSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def singlePaneLayout(bufferId: BufferId): Layout =
    Layout(
      editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), bufferId)),
      activeEditorPaneId = Some(PaneId(0)),
      workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
    )

  "SessionState" should "restore clean file-backed buffers from disk content" in {
    val tempFile = Files.createTempFile("session-state-clean", ".txt")
    Files.writeString(tempFile, "content from disk")

    val buffer = Buffer.fromFile(BufferId(7), tempFile, "content from disk")
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(8),
        nextPaneId = PaneId(1)
      )
    )

    val restoredState =
      SessionState.toAppState(SessionState.fromAppState(appState), Theme.default)

    restoredState.persisted.buffers(buffer.id).document.content.toString.shouldBe("content from disk")
    restoredState.persisted.buffers(buffer.id).document.filePath.shouldBe(Some(tempFile))
    restoredState.persisted.buffers(buffer.id).document.isDirty.shouldBe(false)
  }

  it should "restore dirty file-backed buffers from unsaved in-memory content" in {
    val tempFile = Files.createTempFile("session-state-dirty", ".txt")
    Files.writeString(tempFile, "saved on disk")

    val baseBuffer = Buffer.fromFile(BufferId(9), tempFile, "unsaved in memory")
    val buffer     = baseBuffer.copy(document = baseBuffer.document.copy(isDirty = true))
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(10),
        nextPaneId = PaneId(1)
      )
    )

    val restoredState =
      SessionState.toAppState(SessionState.fromAppState(appState), Theme.default)

    restoredState.persisted.buffers(buffer.id).document.content.toString.shouldBe("unsaved in memory")
    restoredState.persisted.buffers(buffer.id).document.filePath.shouldBe(Some(tempFile))
    restoredState.persisted.buffers(buffer.id).document.isDirty.shouldBe(true)
  }

  it should "preserve dirty buffer content even when persistUnsavedBuffers is false" in {
    val tempFile = Files.createTempFile("session-state-no-persist", ".txt")
    Files.writeString(tempFile, "saved on disk")

    val baseBuffer = Buffer.fromFile(BufferId(11), tempFile, "unsaved in memory")
    val buffer     = baseBuffer.copy(document = baseBuffer.document.copy(isDirty = true))
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(12),
        nextPaneId = PaneId(1)
      )
    )

    val sessionState  = SessionState.fromAppState(appState, persistUnsaved = false)
    val sessionBuffer = sessionState.buffers.find(_.id == buffer.id.value).get

    sessionBuffer.unsavedContent shouldBe Some("unsaved in memory")
    sessionBuffer.isDirty shouldBe true
  }

  it should "rely on the on-disk file for clean buffer content when persistUnsavedBuffers is false" in {
    val tempFile = Files.createTempFile("session-state-clean-persist", ".txt")
    Files.writeString(tempFile, "saved on disk")

    val buffer = Buffer.fromFile(BufferId(13), tempFile, "saved on disk")
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(14),
        nextPaneId = PaneId(1)
      )
    )

    val sessionState  = SessionState.fromAppState(appState, persistUnsaved = false)
    val sessionBuffer = sessionState.buffers.find(_.id == buffer.id.value).get

    sessionBuffer.unsavedContent shouldBe None
    sessionBuffer.isDirty shouldBe false
  }

  it should "survive a JSON encode/decode round trip with content, cursor, viewport, and FindState" in {
    val tempFile = Files.createTempFile("session-json-roundtrip", ".txt")
    Files.writeString(tempFile, "json round trip content")

    val baseBuffer = Buffer.fromFile(BufferId(20), tempFile, "json round trip content")
    val buffer = baseBuffer.copy(
      editing = baseBuffer.editing.copy(cursors = List(CursorPosition(3, 7))),
      viewport = Viewport(topLine = 2, leftColumn = 1, visibleLines = 24, visibleColumns = 80),
      findState = Some(FindState("round", List(FindResult(0, 5), FindResult(5, 9)), 1)),
      annotations = baseBuffer.annotations.copy(
        bookmarks = List(CursorPosition(1, 2), CursorPosition(8, 0)),
        documentComments = List(
          DocumentComment(CursorPosition(2, 0), CursorPosition(2, 9), "Review this paragraph.")
        )
      )
    )
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(21),
        nextPaneId = PaneId(1)
      )
    )

    val sessionState = SessionState.fromAppState(appState)
    val decoded      = sessionState.asJson.as[SessionState]

    decoded.isRight shouldBe true

    val restored       = SessionState.toAppState(decoded.toOption.get, Theme.default)
    val restoredBuffer = restored.persisted.buffers(buffer.id)

    restoredBuffer.document.content.toString shouldBe "json round trip content"
    restoredBuffer.editing.cursors.head shouldBe CursorPosition(3, 7)
    restoredBuffer.viewport.topLine shouldBe 2
    restoredBuffer.viewport.leftColumn shouldBe 1
    restoredBuffer.findState shouldBe Some(FindState("round", List(FindResult(0, 5), FindResult(5, 9)), 1))
    restoredBuffer.annotations.bookmarks shouldBe List(CursorPosition(1, 2), CursorPosition(8, 0))
    restoredBuffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(2, 0), CursorPosition(2, 9), "Review this paragraph.")
    )
  }

  it should "preserve clean rich text metadata through JSON round trip" in {
    val richDocument = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("plain ", RichTextStyle.empty),
            RichTextRun("bold", RichTextStyle(marks = Set(InlineMark.Bold)))
          ),
          alignment = ParagraphAlignment.Center
        )
      )
    )
    val baseBuffer = Buffer.fromString(BufferId(22), richDocument.plainText)
    val buffer     = baseBuffer.copy(richText = baseBuffer.richText.copy(richTextDocument = Some(richDocument)))
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(23),
        nextPaneId = PaneId(1)
      )
    )

    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState]

    decoded.isRight shouldBe true

    val restoredBuffer = SessionState
      .toAppState(decoded.toOption.get, Theme.default)
      .persisted
      .buffers(buffer.id)

    restoredBuffer.document.content.toString shouldBe "plain bold"
    restoredBuffer.richText.richTextDocument shouldBe Some(richDocument)
  }

  it should "preserve rich document fidelity through JSON round trip" in {
    val fidelity = RichTextFidelity(
      unsupportedElements = Set("tbl"),
      unsupportedArchiveEntries = Set("word/media/image1.png")
    )
    val baseBuffer = Buffer.fromString(BufferId(23), "kept text")
    val buffer = baseBuffer.copy(richText =
      baseBuffer.richText.copy(
        richTextDocument = Some(RichTextDocument.oneParagraph("kept text")),
        richTextFidelity = Some(fidelity)
      )
    )
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(24),
        nextPaneId = PaneId(1)
      )
    )

    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState]

    decoded.isRight shouldBe true
    SessionState
      .toAppState(decoded.toOption.get, Theme.default)
      .persisted
      .buffers(buffer.id)
      .richText
      .richTextFidelity shouldBe Some(
      fidelity
    )
  }

  it should "drop stale rich text metadata for dirty buffers" in {
    val richDocument = RichTextDocument.oneParagraph("old text")
    val baseBuffer   = Buffer.fromString(BufferId(24), "edited text")
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(isDirty = true),
      richText = baseBuffer.richText.copy(richTextDocument = Some(richDocument))
    )
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(25),
        nextPaneId = PaneId(1)
      )
    )

    val restoredBuffer = SessionState
      .toAppState(SessionState.fromAppState(appState), Theme.default)
      .persisted
      .buffers(buffer.id)

    restoredBuffer.document.content.toString shouldBe "edited text"
    restoredBuffer.richText.richTextDocument shouldBe None
  }

  it should "preserve aligned rich text metadata for dirty formatting-only buffers" in {
    val richDocument = RichTextDocument(
      List(
        RichTextParagraph(
          List(
            RichTextRun("plain ", RichTextStyle.empty),
            RichTextRun("bold", RichTextStyle(marks = Set(InlineMark.Bold)))
          )
        )
      )
    )
    val baseBuffer = Buffer.fromString(BufferId(26), richDocument.plainText)
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(isDirty = true),
      richText = baseBuffer.richText.copy(richTextDocument = Some(richDocument))
    )
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = singlePaneLayout(buffer.id),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(27),
        nextPaneId = PaneId(1)
      )
    )

    val restoredBuffer = SessionState
      .toAppState(SessionState.fromAppState(appState), Theme.default)
      .persisted
      .buffers(buffer.id)

    restoredBuffer.document.content.toString shouldBe "plain bold"
    restoredBuffer.richText.richTextDocument shouldBe Some(richDocument)
  }

  it should "preserve distinct find state per buffer through round trip" in {
    val file1 = Files.createTempFile("session-find-buffer-1", ".txt")
    val file2 = Files.createTempFile("session-find-buffer-2", ".txt")
    Files.writeString(file1, "apple banana cherry")
    Files.writeString(file2, "dog elephant fox")

    val buffer1 = Buffer
      .fromFile(BufferId(40), file1, "apple banana cherry")
      .copy(findState = Some(FindState("apple", List(FindResult(0, 0)), 0)))
    val buffer2 = Buffer
      .fromFile(BufferId(41), file2, "dog elephant fox")
      .copy(findState = Some(FindState("elephant", List(FindResult(1, 0)), 0)))
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer1.id -> buffer1, buffer2.id -> buffer2),
        bufferOrder = List(buffer1.id, buffer2.id),
        layout = Layout(
          editorPanes = Map(
            PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer1.id),
            PaneId(1) -> EditorPane.withBuffer(PaneId(1), buffer2.id)
          ),
          activeEditorPaneId = Some(PaneId(1)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0), PaneId(1)))
        ),
        focus = Focus.EditorPane(PaneId(1))
      ),
      runtime = AppState.initial.runtime.copy(
        nextBufferId = BufferId(42),
        nextPaneId = PaneId(2)
      )
    )

    val sessionState = SessionState.fromAppState(appState)
    val decoded      = sessionState.asJson.as[SessionState]

    decoded.isRight shouldBe true

    val restored = SessionState.toAppState(decoded.toOption.get, Theme.default)

    restored.persisted.buffers(buffer1.id).findState shouldBe Some(FindState("apple", List(FindResult(0, 0)), 0))
    restored.persisted.buffers(buffer2.id).findState shouldBe Some(FindState("elephant", List(FindResult(1, 0)), 0))
  }
