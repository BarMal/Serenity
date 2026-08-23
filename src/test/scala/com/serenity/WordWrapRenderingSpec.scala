package com.serenity

import java.awt.Font

import cats.effect.IO
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class WordWrapRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "Renderer.render" should "wrap a long document-font line onto the next visual row with the caret" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(1)
    val viewportSize = ViewportSize(50, 14)
    val codeFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val textFont     = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val cellMetrics  = CellMetrics.fromFont(codeFont)
    val content      = "abcdefghij" * 9
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(cursors = List(CursorPosition(0, content.length)))
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )
    val contentRect =
      LayoutEngine
        .calculateEditorPaneLayouts(state, LayoutEngine.calculateLayout(state, viewportSize))(paneId)
        .contentRect
    val panelWidthPx = (contentRect.width * cellMetrics.charWidth).toFloat
    val surface      = new MockRenderSurface(viewportSize.width, viewportSize.height)

    buffer.usesTextFont shouldBe true
    Renderer.render(state, cursorVisible = true, surface, viewportSize, codeFont, textFont, cellMetrics, None)

    val contentRuns =
      surface.drawRunPxCalls.filter(call => call.s.nonEmpty && call.s.forall(char => char >= 'a' && char <= 'j'))
    val contentRows = contentRuns.groupBy(_.yPx).toList.sortBy(_._1)
    val cursorRects = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)

    contentRuns.map(_.s).mkString shouldBe content
    contentRows.length should be >= 2
    contentRows.foreach { (_, runs) =>
      val rowWidthPx = TextLayoutSnapshot.caretXsForText(runs.map(_.s).mkString, textFont).lastOption.getOrElse(0.0f)
      rowWidthPx should be <= panelWidthPx
    }
    cursorRects should have size 1
    cursorRects.head.yPx shouldBe contentRows.last._1
  }
