package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.manager.{AuthoritativeUiScene, StateManager}
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The scrollbar driven by real mouse events through the whole event pipeline: a thumb taken hold of and dragged, a
  * track clicked, and neither of them reaching the text beside the bar.
  */
class ScrollbarMouseEventSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewportSize = ViewportSize(100, 30)

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(
        logger,
        dictionaryCache = SharedDictionary.default,
        initialConfig = com.serenity.config.AppConfig.conventional
      )(using
        com.serenity.rope.Balance.default,
        LoggerFactory[IO]
      )
      .unsafeRunSync()

  final private case class Rig(stateManager: StateManager, bufferId: BufferId):
    def state: AppState          = stateManager.getCurrentState.unsafeRunSync()
    def buffer: Buffer           = state.persisted.buffers(bufferId)
    def scene: UiSceneSnapshot   = AuthoritativeUiScene().forState(state, viewportSize)
    def send(event: Event): Unit = stateManager.applyEvent(event).unsafeRunSync()

    private def metrics: CellMetrics =
      CellMetrics.fromFont(FontLoader.previewCodeFont(state.persisted.config.editorConfig.fontConfig))
    private def pane: EditorPaneLayout = scene.paneLayouts.getOrElse(PaneId(0), fail("expected the pane"))
    def extent: ScrollExtent           = scene.paneScrolls.getOrElse(PaneId(0), fail("expected a scroll")).extent
    private def strip: PixelRect       = ScrollbarLayout.strip(pane.contentRect, metrics)
    def trackTopPx: Int                = strip.yPx
    def trackPx: Int                   = strip.heightPx
    def thumb: ScrollThumb             = ScrollbarGeometry.thumb(extent, trackPx).getOrElse(fail("expected a thumb"))

    def pressAlongTrack(px: Int): MousePress =
      val y = trackTopPx + px
      val x = strip.xPx + strip.widthPx / 2
      MousePress(metrics.toCol(x), metrics.toRow(y), Some(x), Some(y))

    def dragAlongTrack(px: Int): MouseDrag =
      val press = pressAlongTrack(px)
      MouseDrag(press.col, press.row, press.pixelX, press.pixelY)

    def clickAlongTrack(px: Int): MouseClick =
      val press = pressAlongTrack(px)
      MouseClick(press.col, press.row, press.pixelX, press.pixelY)

  private def openLongDocument(): Rig =
    val stateManager = makeStateManager()
    val bufferId =
      stateManager.createBuffer((0 until 400).map(line => s"line $line").mkString("\n"), None).unsafeRunSync()
    stateManager.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(viewportSize)).unsafeRunSync()
    Rig(stateManager, bufferId)

  "Dragging the thumb" should "scroll the document to where the pointer takes it, leaving the caret where it was" in {
    val rig    = openLongDocument()
    val caret  = rig.buffer.editing.cursorPositions
    val extent = rig.extent
    val thumb  = rig.thumb

    rig.send(rig.pressAlongTrack(thumb.offsetPx + 8))
    rig.send(rig.dragAlongTrack(rig.trackPx))

    rig.buffer.viewport.topLine shouldBe extent.lastFirstRow
    rig.buffer.editing.cursorPositions shouldBe caret

    rig.send(rig.dragAlongTrack(0))

    rig.buffer.viewport.topLine shouldBe 0
    rig.buffer.editing.cursorPositions shouldBe caret
  }

  "A press on the thumb" should "take hold of it, and a press in the text let it go" in {
    val rig  = openLongDocument()
    val rect = rig.scene.paneLayouts(PaneId(0)).contentRect

    rig.send(rig.pressAlongTrack(rig.thumb.offsetPx + 8))
    rig.state.runtime.pointerGesture.scrollbarGrab shouldBe Some(ScrollbarGrab(PaneId(0), 8))

    rig.send(MousePress(rect.x + 3, rect.y + 3))
    rig.state.runtime.pointerGesture.scrollbarGrab shouldBe None
  }

  "A click in the track below the thumb" should "page the document once and leave the caret where it was" in {
    val rig    = openLongDocument()
    val caret  = rig.buffer.editing.cursorPositions
    val extent = rig.extent
    val below  = rig.thumb.endPx + 40

    rig.send(rig.pressAlongTrack(below))
    rig.send(rig.clickAlongTrack(below))

    rig.buffer.viewport.topLine shouldBe extent.pageRows
    rig.buffer.editing.cursorPositions shouldBe caret
  }

  "A click on the bar" should "not reach the text beside it" in {
    val rig   = openLongDocument()
    val caret = rig.buffer.editing.cursorPositions

    rig.send(rig.clickAlongTrack(rig.thumb.offsetPx + 8))

    rig.buffer.editing.cursorPositions shouldBe caret
    rig.buffer.viewport.topLine shouldBe 0
  }
