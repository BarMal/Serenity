package com.serenity

import com.serenity.keystroke.events.MoveRight
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.ui.layout.{Layout, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorLargeJsonNavigationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  final case class NonCollectingRope(delegate: Rope) extends Rope:
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override def newlineCount: Int =
      delegate.newlineCount

    override def lastLineLength: Int =
      delegate.lastLineLength

    override def endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def lineCount: Int =
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      delegate.getLine(lineIndex)

    override def lineColumnToOffset(line: Int, column: Int): Int =
      delegate.lineColumnToOffset(line, column)

    override def offsetToLineColumn(offset: Int): (Int, Int) =
      delegate.offsetToLineColumn(offset)

    override def collect(): String =
      throw AssertionError("large JSON navigation should not materialise the whole buffer")

  "Editor navigation in large JSON buffers" should "move horizontally without materialising the whole buffer" in {
    val largeJsonLine = s"""{"items":[${List.fill(2000)("""{"id":1,"name":"value"}""").mkString(",")}]}"""
    val buffer = Buffer
      .fromString(bufferId, largeJsonLine)
      .copy(
        content = NonCollectingRope(Rope(largeJsonLine)),
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

    val moved = (1 to 100).foldLeft(state)((current, _) => EditorEventReducer.reduce(MoveRight, paneId, current).state)

    moved.buffers(bufferId).cursors.head shouldBe CursorPosition(0, 100)
  }
