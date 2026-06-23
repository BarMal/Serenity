package com.serenity

import scala.concurrent.duration.*

import com.serenity.keystroke.events.MoveRight
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.ui.layout.{Layout, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorLargeJsonNavigationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  "Editor navigation in large JSON buffers" should "keep horizontal movement responsive" in {
    val largeJsonLine = s"""{"items":[${List.fill(2000)("""{"id":1,"name":"value"}""").mkString(",")}]}"""
    val buffer = Buffer
      .fromString(bufferId, largeJsonLine)
      .copy(
        language = Some(LanguageId.JsonLang),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 24)
      )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      viewportSize = Some(ViewportSize(100, 30))
    )

    val started = System.nanoTime()
    val moved = (1 to 100).foldLeft(state)((current, _) => EditorEventReducer.reduce(MoveRight, paneId, current).state)
    val elapsed = (System.nanoTime() - started).nanos

    moved.buffers(bufferId).cursors.head shouldBe CursorPosition(0, 100)
    elapsed should be < 2.seconds
  }
