package com.serenity

import java.nio.file.Files

import com.serenity.animation.AnimationConfig
import com.serenity.config.{AppConfig, BackgroundStyle, WindowChromeMode}
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.session.given
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import com.serenity.ui.theme.Theme
import _root_.io.circe.syntax.*
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

  it should "survive a JSON encode/decode round trip with content, cursor, viewport, and FindState" in {
    val tempFile = Files.createTempFile("session-json-roundtrip", ".txt")
    Files.writeString(tempFile, "json round trip content")

    val buffer = Buffer
      .fromFile(BufferId(20), tempFile, "json round trip content")
      .copy(
        cursors = List(CursorPosition(3, 7)),
        viewport = Viewport(topLine = 2, leftColumn = 1, visibleLines = 24, visibleColumns = 80),
        findState = Some(FindState("round", List(0, 5), 1))
      )
    val appState = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(21),
      nextPaneId = PaneId(1)
    )

    val sessionState = SessionState.fromAppState(appState)
    val decoded      = sessionState.asJson.as[SessionState]

    decoded.isRight shouldBe true

    val restored       = SessionState.toAppState(decoded.toOption.get, Theme.default)
    val restoredBuffer = restored.buffers(buffer.id)

    restoredBuffer.content.toString shouldBe "json round trip content"
    restoredBuffer.cursors.head shouldBe CursorPosition(3, 7)
    restoredBuffer.viewport.topLine shouldBe 2
    restoredBuffer.viewport.leftColumn shouldBe 1
    restoredBuffer.findState shouldBe Some(FindState("round", List(0, 5), 1))
  }

  it should "preserve config fields including blurRadius and backgroundStyle through JSON round trip" in {
    val appState = AppState.initial.copy(
      config = AppConfig(
        characterAnimation = AnimationConfig.quick,
        fontConfig = com.serenity.ui.fonts.FontLoader.FontConfig(
          codeFontFamily = "Monospaced",
          textFontFamily = "SansSerif",
          fontSize = 15.0f,
          uiFontSize = 13.0f,
          enableLigatures = false
        ),
        blurRadius = 0.42f,
        backgroundStyle = BackgroundStyle.GlassLike,
        windowChromeMode = WindowChromeMode.Custom,
        showLineNumbers = false,
        showGutter = false
      )
    )

    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState].toOption.get

    decoded.config.blurRadius shouldBe 0.42f
    decoded.config.backgroundStyle shouldBe BackgroundStyle.GlassLike
    decoded.config.windowChromeMode shouldBe WindowChromeMode.Custom
    decoded.config.fontConfig.codeFontFamily shouldBe "Monospaced"
    decoded.config.fontConfig.textFontFamily shouldBe "SansSerif"
    decoded.config.fontConfig.fontSize shouldBe 15.0f
    decoded.config.fontConfig.uiFontSize shouldBe 13.0f
    decoded.config.fontConfig.enableLigatures shouldBe false
    decoded.config.showLineNumbers shouldBe false
    decoded.config.showGutter shouldBe false
    decoded.config.characterAnimation.map(_.steps) shouldBe
      AnimationConfig.quick.map(_.steps)
  }

  it should "default backgroundStyle to Frosted when loading older JSON without the field" in {
    val originalJson = SessionState.fromAppState(AppState.initial.copy(config = AppConfig.default)).asJson
    val configObject = originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutBackgroundStyle =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("backgroundStyle")))
      )

    val decoded = jsonWithoutBackgroundStyle.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.backgroundStyle shouldBe BackgroundStyle.Frosted
  }

  it should "default windowChromeMode to Native when loading older JSON without the field" in {
    val originalJson = SessionState.fromAppState(AppState.initial.copy(config = AppConfig.default)).asJson
    val configObject = originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutWindowChromeMode =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("windowChromeMode")))
      )

    val decoded = jsonWithoutWindowChromeMode.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.windowChromeMode shouldBe WindowChromeMode.Native
  }

  it should "survive a multi-pane multi-buffer layout round trip" in {
    val file1 = Files.createTempFile("session-multi-pane-1", ".txt")
    val file2 = Files.createTempFile("session-multi-pane-2", ".txt")
    Files.writeString(file1, "pane one content")
    Files.writeString(file2, "pane two content")

    val buffer1 = Buffer.fromFile(BufferId(30), file1, "pane one content")
    val buffer2 = Buffer.fromFile(BufferId(31), file2, "pane two content")
    val appState = AppState.initial.copy(
      buffers = Map(buffer1.id -> buffer1, buffer2.id -> buffer2),
      bufferOrder = List(buffer1.id, buffer2.id),
      layout = Layout(
        editorPanes = Map(
          PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer1.id),
          PaneId(1) -> EditorPane.withBuffer(PaneId(1), buffer2.id)
        ),
        activeEditorPaneId = Some(PaneId(1))
      ),
      focus = Focus.EditorPane(PaneId(1)),
      nextBufferId = BufferId(32),
      nextPaneId = PaneId(2)
    )

    val restored = SessionState.toAppState(SessionState.fromAppState(appState), Theme.default)

    restored.buffers should have size 2
    restored.buffers(buffer1.id).content.toString shouldBe "pane one content"
    restored.buffers(buffer2.id).content.toString shouldBe "pane two content"
    restored.layout.editorPanes should have size 2
    restored.layout.activeEditorPaneId shouldBe Some(PaneId(1))
    restored.focus shouldBe Focus.EditorPane(PaneId(1))
    restored.bufferOrder shouldBe List(buffer1.id, buffer2.id)
  }

  it should "preserve distinct find state per buffer through round trip" in {
    val file1 = Files.createTempFile("session-find-buffer-1", ".txt")
    val file2 = Files.createTempFile("session-find-buffer-2", ".txt")
    Files.writeString(file1, "apple banana cherry")
    Files.writeString(file2, "dog elephant fox")

    val buffer1 = Buffer
      .fromFile(BufferId(40), file1, "apple banana cherry")
      .copy(findState = Some(FindState("apple", List(0), 0)))
    val buffer2 = Buffer
      .fromFile(BufferId(41), file2, "dog elephant fox")
      .copy(findState = Some(FindState("elephant", List(1), 0)))
    val appState = AppState.initial.copy(
      buffers = Map(buffer1.id -> buffer1, buffer2.id -> buffer2),
      bufferOrder = List(buffer1.id, buffer2.id),
      layout = Layout(
        editorPanes = Map(
          PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer1.id),
          PaneId(1) -> EditorPane.withBuffer(PaneId(1), buffer2.id)
        ),
        activeEditorPaneId = Some(PaneId(1))
      ),
      focus = Focus.EditorPane(PaneId(1)),
      nextBufferId = BufferId(42),
      nextPaneId = PaneId(2)
    )

    val sessionState = SessionState.fromAppState(appState)
    val decoded      = sessionState.asJson.as[SessionState]

    decoded.isRight shouldBe true

    val restored = SessionState.toAppState(decoded.toOption.get, Theme.default)

    restored.buffers(buffer1.id).findState shouldBe Some(FindState("apple", List(0), 0))
    restored.buffers(buffer2.id).findState shouldBe Some(FindState("elephant", List(1), 0))
  }
