package com.serenity.state.manager

import java.awt.Font

import com.serenity.config.{AppConfig, TextAreaInsets}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, LayoutEngine, UiSceneSnapshot, ViewportSize}
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MouseTargetCacheSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(buffer: Buffer, config: AppConfig = AppConfig.default): AppState =
    AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = config
    )

  "MouseTargetLayoutKey" should "ignore cursor and selection changes during mouse drags" in {
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(cursors = List(CursorPosition(0, 1)))
    val state = stateWith(buffer)
    val draggedState = state.copy(
      buffers = state.buffers.updated(
        bufferId,
        buffer.copy(
          cursors = List(CursorPosition(1, 3)),
          selection = Some(Selection(CursorPosition(0, 1), CursorPosition(1, 3)))
        )
      )
    )

    MouseTargetLayoutKey.from(state, ViewportSize(80, 24)) shouldBe
      MouseTargetLayoutKey.from(draggedState, ViewportSize(80, 24))
  }

  it should "cache full editor pane layouts for mouse hit testing" in {
    val config = AppConfig.default.withTextAreaInsets(TextAreaInsets(0.15, 0.10))
    val state  = stateWith(Buffer.fromString(bufferId, "alpha\nbeta"), config)
    val size   = ViewportSize(80, 24)
    val cache  = MouseTargetCache.fromState(state, size)
    val layout = LayoutEngine.calculateLayoutWithUI(state, size)

    cache.scene.paneLayouts shouldBe LayoutEngine.calculateEditorPaneLayouts(state, layout)
    cache.scene.paneLayouts(paneId).headerRect.bottom.shouldBe(cache.scene.paneLayouts(paneId).contentRect.y)
  }

  it should "cache the authoritative scene used for mouse-target geometry" in {
    val state  = stateWith(Buffer.fromString(bufferId, "alpha\nbeta"))
    val size   = ViewportSize(80, 24)
    val cache  = MouseTargetCache.fromState(state, size)
    val reused = MouseTargetCache.fromState(state, size)
    val layout = LayoutEngine.calculateLayoutWithUI(state, size)

    cache.scene.editorContract.workspace.paneLayouts shouldBe cache.scene.paneLayouts
    cache.scene.calculatedLayout shouldBe layout
    reused.scene should be theSameInstanceAs cache.scene
  }

  it should "reuse the prepared scene for cursor-only state changes" in {
    val buffer = Buffer.fromString(bufferId, "alpha beta")
    val state  = stateWith(buffer.copy(cursors = List(CursorPosition(0, 1))))
    val moved  = stateWith(buffer.copy(cursors = List(CursorPosition(0, 5))))
    val size   = ViewportSize(80, 24)
    val scene  = MouseTargetCache.fromState(state, size).scene

    UiSceneSnapshot.publish(moved, size, scene)

    MouseTargetCache.fromState(moved, size).scene should be theSameInstanceAs scene
  }

  it should "use the renderer's proportional wrapped snapshot for hit testing" in {
    val state   = stateWith(Buffer.fromString(bufferId, (1 to 20).map(_ => "proportional").mkString(" ")))
    val size    = ViewportSize(80, 24)
    val mono    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val text    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val surface = new com.serenity.MockRenderSurface(size.width, size.height)

    Renderer.render(state, cursorVisible = true, surface, size, mono, text, CellMetrics.fromFont(mono), None)

    val cache    = MouseTargetCache.fromState(state, size)
    val snapshot = cache.scene.textSnapshot(paneId).getOrElse(fail("expected prepared text snapshot"))

    snapshot.usesMeasuredLayout shouldBe true
    snapshot.isProportional shouldBe true
    snapshot.visualLines.size should be > 1
    cache.scene should be theSameInstanceAs MouseTargetCache.fromState(state, size).scene
  }

  it should "change when layout-affecting content changes with line numbers enabled" in {
    val shortState = stateWith(Buffer.fromString(bufferId, "one"))
    val longState  = stateWith(Buffer.fromString(bufferId, (1 to 100).map(i => s"line $i").mkString("\n")))

    MouseTargetLayoutKey.from(shortState, ViewportSize(80, 24)) should not be
      MouseTargetLayoutKey.from(longState, ViewportSize(80, 24))
  }

  it should "invalidate prepared snapshots when font, typography, language, viewport, or rich text changes" in {
    val buffer = Buffer
      .fromString(bufferId, "alpha beta")
      .copy(language = Some(LanguageId.Scala))
    val state = stateWith(buffer)
    val size  = ViewportSize(80, 24)
    val key   = MouseTargetLayoutKey.from(state, size)

    val fontChanged     = stateWith(buffer, state.config.withFontConfig(state.config.fontConfig.copy(fontSize = 14.0f)))
    val languageChanged = stateWith(buffer.copy(language = Some(LanguageId.Markdown)))
    val languageRemoved = stateWith(buffer.copy(language = None))
    val viewportChanged = stateWith(buffer.copy(viewport = buffer.viewport.copy(topVisualLine = 1)))
    val richTextChanged = stateWith(
      buffer.copy(richTextDocument = Some(com.serenity.richtext.RichTextDocument.fromPlainText("alpha beta")))
    )

    List(fontChanged, languageChanged, languageRemoved, viewportChanged, richTextChanged).foreach { changed =>
      MouseTargetLayoutKey.from(changed, size) should not be key
    }
  }
