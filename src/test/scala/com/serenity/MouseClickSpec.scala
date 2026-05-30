package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{MouseClick, ResizeEvent}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.PaneId
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MouseClickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  // Layout at 80x24, showLineNumbers=true, showGutter=true:
  //   gutterHeight=1  → contentHeight=23
  //   spacerWidth=(80*0.15).toInt=12
  //   lineNumberWidth for 4-line buffer = max(3, "4".length+1) = 3
  //   editorPanelRect = LayoutRect(x=15, y=0, width=53, height=23)
  //   PaneId(0) → LayoutRect(x=15, y=0, width=53, height=23)
  //   contentRow starts at y=1 (header at y=0)

  "MouseClick" should "move cursor to the clicked buffer position" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("aaaa\nbbbb\ncccc\ndddd").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Click at (18, 3): bufferLine = topLine(0) + (3-1) = 2, bufferCol = leftCol(0) + (18-15) = 3
    sm.applyEvent(MouseClick(18, 3)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.cursors.headOption.map(_.line) shouldBe Some(2)
    buffer.cursors.headOption.map(_.column) shouldBe Some(3)
  }

  it should "move cursor to the first row of the content area" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Click at (15, 1): first cell of content area → bufferLine=0, bufferCol=0
    sm.applyEvent(MouseClick(15, 1)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.cursors.headOption.map(_.line) shouldBe Some(0)
    buffer.cursors.headOption.map(_.column) shouldBe Some(0)
  }

  it should "clamp column to line length when clicking past end of line" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hi\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Click at (35, 1): bufferLine=0, bufferCol = 35-15 = 20, "hi" length=2 → clamp to 2
    sm.applyEvent(MouseClick(35, 1)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.cursors.headOption.map(_.line) shouldBe Some(0)
    buffer.cursors.headOption.map(_.column) shouldBe Some(2)
  }

  it should "ignore clicks in the pane header row" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val initialCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).cursors.headOption

    // Click at row=0 (header row of pane at y=0) — should be ignored
    sm.applyEvent(MouseClick(20, 0)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).cursors.headOption
    afterCursor shouldBe initialCursor
  }

  it should "ignore clicks in the spacer area outside any pane" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val initialCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).cursors.headOption

    // Click at col=5 (left spacer, pane starts at col=15) — should be ignored
    sm.applyEvent(MouseClick(5, 5)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).cursors.headOption
    afterCursor shouldBe initialCursor
  }

  it should "ignore clicks when terminal size has not been set" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    // No ResizeEvent applied — ViewportSize is None

    val initialCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).cursors.headOption

    sm.applyEvent(MouseClick(20, 5)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).cursors.headOption
    afterCursor shouldBe initialCursor
  }
