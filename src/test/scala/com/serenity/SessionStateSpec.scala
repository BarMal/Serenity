package com.serenity

import java.awt.Font
import java.nio.file.Files

import _root_.io.circe.syntax.*
import com.serenity.animation.AnimationConfig
import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.session.given
import com.serenity.session.{SessionFindResult, SessionFindState, SessionState}
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, PaneSplitDirection}
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

    val sessionState  = SessionState.fromAppState(appState, persistUnsaved = false)
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

    val sessionState  = SessionState.fromAppState(appState, persistUnsaved = false)
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
        findState = Some(FindState("round", List(FindResult(0, 5), FindResult(5, 9)), 1)),
        bookmarks = List(CursorPosition(1, 2), CursorPosition(8, 0))
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
    restoredBuffer.findState shouldBe Some(FindState("round", List(FindResult(0, 5), FindResult(5, 9)), 1))
    restoredBuffer.bookmarks shouldBe List(CursorPosition(1, 2), CursorPosition(8, 0))
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
    val buffer = Buffer
      .fromString(BufferId(22), richDocument.plainText)
      .copy(richTextDocument = Some(richDocument))
    val appState = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(23),
      nextPaneId = PaneId(1)
    )

    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState]

    decoded.isRight shouldBe true

    val restoredBuffer = SessionState
      .toAppState(decoded.toOption.get, Theme.default)
      .buffers(buffer.id)

    restoredBuffer.content.toString shouldBe "plain bold"
    restoredBuffer.richTextDocument shouldBe Some(richDocument)
  }

  it should "drop stale rich text metadata for dirty buffers" in {
    val richDocument = RichTextDocument.oneParagraph("old text")
    val buffer = Buffer
      .fromString(BufferId(24), "edited text")
      .copy(isDirty = true, richTextDocument = Some(richDocument))
    val appState = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(25),
      nextPaneId = PaneId(1)
    )

    val restoredBuffer = SessionState
      .toAppState(SessionState.fromAppState(appState), Theme.default)
      .buffers(buffer.id)

    restoredBuffer.content.toString shouldBe "edited text"
    restoredBuffer.richTextDocument shouldBe None
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
    val buffer = Buffer
      .fromString(BufferId(26), richDocument.plainText)
      .copy(isDirty = true, richTextDocument = Some(richDocument))
    val appState = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(27),
      nextPaneId = PaneId(1)
    )

    val restoredBuffer = SessionState
      .toAppState(SessionState.fromAppState(appState), Theme.default)
      .buffers(buffer.id)

    restoredBuffer.content.toString shouldBe "plain bold"
    restoredBuffer.richTextDocument shouldBe Some(richDocument)
  }

  it should "restore legacy session find state that only stored result lines" in {
    val decoded = _root_.io.circe.parser
      .parse("""{"query":"legacy","resultLines":[2,4],"currentIndex":1}""")
      .flatMap(_.as[SessionFindState])

    decoded shouldBe Right(
      SessionFindState(
        query = "legacy",
        results = List(SessionFindResult(2, 0), SessionFindResult(4, 0)),
        currentIndex = 1
      )
    )
    decoded.toOption.map(SessionFindState.toFindState) shouldBe Some(
      FindState("legacy", List(FindResult(2, 0), FindResult(4, 0)), 1)
    )
  }

  it should "restore legacy session viewports without a wrapped visual offset" in {
    val decoded = _root_.io.circe.parser
      .parse("""{"leftColumn":1,"topLine":2,"visibleColumns":80,"visibleLines":24}""")
      .flatMap(_.as[com.serenity.session.SessionViewport])

    decoded.toOption.map(com.serenity.session.SessionViewport.toViewport) shouldBe
      Some(Viewport(leftColumn = 1, topLine = 2, visibleColumns = 80, visibleLines = 24, topVisualLine = 0))
  }

  it should "preserve config fields including blurRadius and backgroundStyle through JSON round trip" in {
    val appState = AppState.initial.copy(
      config = AppConfig(
        characterAnimation = AnimationConfig.quick,
        fontConfig = com.serenity.ui.fonts.FontLoader.FontConfig(
          codeFontFamily = "Monospaced",
          textFontFamily = "SansSerif",
          uiFontFamily = "Dialog",
          fontSize = 15.0f,
          textFontSize = 16.0f,
          uiFontSize = 13.0f,
          enableLigatures = false,
          textLigatures = true,
          uiLigatures = true
        ),
        blurRadius = 0.42f,
        backgroundStyle = BackgroundStyle.GlassLike,
        materialPreset = MaterialPreset.Crystal,
        motionPreset = MotionPreset.Reduced,
        windowChromeMode = WindowChromeMode.Custom,
        interfaceDensity = InterfaceDensity.Spacious,
        cursorInfoBarMode = CursorInfoBarMode.Detailed,
        showLineNumbers = false,
        showGutter = false,
        lspUserConfig = LspUserConfig(
          servers = Some(
            Map(
              LanguageId.Scala.id -> LspServerOverride(
                command = Some("custom-metals"),
                args = Some(List("--stdio")),
                enabled = Some(true)
              )
            )
          )
        ),
        spellCheck = SpellCheckConfig(enabled = true, languages = List("en", "fr"), additionalWords = List("serenity"))
      )
    )

    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState].toOption.get

    decoded.config.blurRadius shouldBe 0.42f
    decoded.config.backgroundStyle shouldBe BackgroundStyle.GlassLike
    decoded.config.materialPreset shouldBe MaterialPreset.Crystal
    decoded.config.motionPreset shouldBe MotionPreset.Reduced
    decoded.config.windowChromeMode shouldBe WindowChromeMode.Custom
    decoded.config.interfaceDensity shouldBe InterfaceDensity.Spacious
    decoded.config.cursorInfoBarMode shouldBe CursorInfoBarMode.Detailed
    decoded.config.fontConfig.codeFontFamily shouldBe "Monospaced"
    decoded.config.fontConfig.textFontFamily shouldBe "SansSerif"
    decoded.config.fontConfig.uiFontFamily shouldBe "Dialog"
    decoded.config.fontConfig.codeFontSize shouldBe 15.0f
    decoded.config.fontConfig.textFontSize shouldBe 16.0f
    decoded.config.fontConfig.uiFontSize shouldBe 13.0f
    decoded.config.fontConfig.codeLigatures shouldBe false
    decoded.config.fontConfig.textLigatures shouldBe true
    decoded.config.fontConfig.uiLigatures shouldBe true
    decoded.config.showLineNumbers shouldBe false
    decoded.config.showGutter shouldBe false
    decoded.config.lspUserConfig.servers.map(_(LanguageId.Scala.id)) shouldBe Some(
      LspServerOverride(
        command = Some("custom-metals"),
        args = Some(List("--stdio")),
        enabled = Some(true)
      )
    )
    decoded.config.spellCheck shouldBe SpellCheckConfig(
      enabled = true,
      languages = List("en", "fr"),
      additionalWords = List("serenity")
    )
    decoded.config.characterAnimation.map(_.steps) shouldBe
      AnimationConfig.quick.map(_.steps)
  }

  it should "default backgroundStyle to Frosted when loading older JSON without the field" in {
    val originalJson = SessionState.fromAppState(AppState.initial.copy(config = AppConfig.default)).asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutBackgroundStyle =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("backgroundStyle")))
      )

    val decoded = jsonWithoutBackgroundStyle.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.backgroundStyle shouldBe BackgroundStyle.Frosted
  }

  it should "default material and motion presets when loading older JSON without the fields" in {
    val originalJson = SessionState.fromAppState(AppState.initial.copy(config = AppConfig.default)).asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutPresets =
      originalJson.mapObject(
        _.add(
          "config",
          _root_.io.circe.Json.fromJsonObject(configObject.remove("materialPreset").remove("motionPreset"))
        )
      )

    val decoded = jsonWithoutPresets.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.materialPreset shouldBe MaterialPreset.Frosted
    decoded.toOption.get.config.motionPreset shouldBe MotionPreset.Smooth
  }

  it should "default windowChromeMode to Native when loading older JSON without the field" in {
    val originalJson = SessionState.fromAppState(AppState.initial.copy(config = AppConfig.default)).asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutWindowChromeMode =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("windowChromeMode")))
      )

    val decoded = jsonWithoutWindowChromeMode.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.windowChromeMode shouldBe WindowChromeMode.Native
  }

  it should "default interfaceDensity to Comfortable when loading older JSON without the field" in {
    val originalJson = SessionState.fromAppState(AppState.initial.copy(config = AppConfig.default)).asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutInterfaceDensity =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("interfaceDensity")))
      )

    val decoded = jsonWithoutInterfaceDensity.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.interfaceDensity shouldBe InterfaceDensity.Comfortable
  }

  it should "default cursorInfoBarMode to Off when loading older JSON without the field" in {
    val originalJson = SessionState.fromAppState(AppState.initial.copy(config = AppConfig.default)).asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutCursorInfoBarMode =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("cursorInfoBarMode")))
      )

    val decoded = jsonWithoutCursorInfoBarMode.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorInfoBarMode shouldBe CursorInfoBarMode.Off
  }

  it should "default uiFontFamily to SansSerif when loading older JSON without the field" in {
    val originalJson = SessionState.fromAppState(AppState.initial.copy(config = AppConfig.default)).asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val fontConfigObject = configObject("fontConfig").flatMap(_.asObject).getOrElse(fail("Expected fontConfig object"))
    val jsonWithoutUiFontFamily =
      originalJson.mapObject(
        _.add(
          "config",
          _root_.io.circe.Json.fromJsonObject(
            configObject.add(
              "fontConfig",
              _root_.io.circe.Json.fromJsonObject(fontConfigObject.remove("uiFontFamily"))
            )
          )
        )
      )

    val decoded = jsonWithoutUiFontFamily.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.fontConfig.uiFontFamily shouldBe Font.SANS_SERIF
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

  it should "serialize buffers and panes in their canonical order" in {
    val buffer1 = Buffer.fromString(BufferId(1), "one")
    val buffer2 = Buffer.fromString(BufferId(2), "two")
    val pane1   = EditorPane.withBuffer(PaneId(1), buffer1.id)
    val pane2   = EditorPane.withBuffer(PaneId(2), buffer2.id)
    val appState = AppState.initial.copy(
      buffers = Map(buffer1.id -> buffer1, buffer2.id -> buffer2),
      bufferOrder = List(buffer2.id, buffer1.id),
      layout = Layout(
        editorPanes = Map(pane1.id -> pane1, pane2.id -> pane2),
        activeEditorPaneId = Some(pane2.id),
        paneOrder = List(pane2.id, pane1.id)
      )
    )

    val sessionState = SessionState.fromAppState(appState)

    sessionState.buffers.map(_.id) shouldBe List(2, 1)
    sessionState.layout.editorPanes.map(_.id) shouldBe List(2, 1)
  }

  it should "preserve pane split direction through round trip" in {
    val pane1 = EditorPane.empty(PaneId(1))
    val pane2 = EditorPane.empty(PaneId(2))
    val appState = AppState.initial.copy(
      layout = Layout(
        editorPanes = Map(pane1.id -> pane1, pane2.id -> pane2),
        activeEditorPaneId = Some(pane2.id),
        paneOrder = List(pane1.id, pane2.id),
        splitDirection = PaneSplitDirection.Vertical
      ),
      focus = Focus.EditorPane(pane2.id)
    )

    val sessionState = SessionState.fromAppState(appState)
    val restored     = SessionState.toAppState(sessionState, Theme.default)

    sessionState.layout.splitDirection shouldBe "Vertical"
    restored.layout.splitDirection shouldBe PaneSplitDirection.Vertical
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

    restored.buffers(buffer1.id).findState shouldBe Some(FindState("apple", List(FindResult(0, 0)), 0))
    restored.buffers(buffer2.id).findState shouldBe Some(FindState("elephant", List(FindResult(1, 0)), 0))
  }
