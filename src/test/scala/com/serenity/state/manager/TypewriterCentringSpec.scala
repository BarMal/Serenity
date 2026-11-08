package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2570 item 2, in the windowed editor: rows are as tall as they paint -- a heading's font and the space around it, a
  * drop cap, the prose font against the code grid -- so holding the caret at the pane's middle is a matter of pixels.
  * Counting half the pane in rows of the buffer font put the caret on rows 22-27 of a 30-row pane.
  */
class TypewriterCentringSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  import ColumnPagingFixture.{bufferId, paneId}

  private val paneSize = ViewportSize(70, 30)

  private def richState(config: AppConfig => AppConfig, terminal: Boolean = false): AppState =
    val fixture = ColumnPagingFixture(scale = 1, columnCount = 1).state
    val configured = fixture.copy(
      persisted = fixture.persisted.copy(config = config(fixture.persisted.config.withColumnMode(false))),
      runtime = fixture.runtime.copy(
        viewportSize = Some(paneSize),
        capabilities = if terminal then FrontendCapabilities.tui() else fixture.runtime.capabilities
      )
    )
    LayoutEngine.syncViewportDimensions(configured, paneSize)

  private def movedTo(state: AppState, cursor: CursorPosition): AppState =
    val buffer = state.persisted.buffers(bufferId)
    val moved = state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers + (bufferId -> buffer.copy(editing = EditingState(List(cursor))))
      )
    )
    CursorViewport.ensureVisibleCursors(state, moved)

  /** The cell row the caret is painted on, and the pane's content rect, as popups and the painter see them. */
  private def painted(state: AppState, cursor: CursorPosition): (Option[Int], Int, Int) =
    val layout = LayoutEngine.calculateLayoutWithUI(state, paneSize)
    val panes  = LayoutEngine.laidOutPanes(state, layout)
    val rect   = panes.layouts(paneId).contentRect
    (panes.screenPosition(paneId, cursor).map(_.y), rect.y, rect.bottom)

  private def scrolledTo(state: AppState, line: Int, row: Int): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers + (bufferId -> buffer.copy(viewport = buffer.viewport.scrolledTo(line, 0, row)))
      )
    )

  /** How far from the pane's middle row `cursor` is painted from each top a few screens above it: whole rows set how
    * near any top can bring it, a heading's space making some steps long.
    */
  private def bestDistance(state: AppState, cursor: CursorPosition, middle: Int): Int =
    val buffer = state.persisted.buffers(bufferId)
    val counts = CursorViewport.visualRowsFor(buffer, state, com.serenity.ui.layout.WrappedLineCache.Uncached)
    val tops = for
      line <- math.max(0, cursor.line - 20) to cursor.line
      row  <- 0 until counts.rowsIn(line)
    yield (line, row)
    tops
      .flatMap((line, row) => painted(scrolledTo(state, line, row), cursor)._1)
      .map(painted => math.abs(painted - middle))
      .minOption
      .getOrElse(Int.MaxValue)

  /** Lines the caret was painted on further from the middle than some other top would have painted it. */
  private def offCentre(start: AppState, lines: Seq[Int]): List[String] =
    lines.toList
      .foldLeft((start, List.empty[String])) {
        case ((state, problems), line) =>
          val cursor                 = CursorPosition(line, 0)
          val placed                 = movedTo(state, cursor)
          val (row, paneTop, bottom) = painted(placed, cursor)
          val middle                 = (paneTop + bottom) / 2
          val best                   = bestDistance(placed, cursor, middle)
          val problem = Option.when(!row.exists(r => math.abs(r - middle) <= best))(
            s"line $line painted on row $row, middle $middle, another top paints it $best from the middle"
          )
          (placed, problems ++ problem)
      }
      ._2

  private def outsidePane(start: AppState, lines: Seq[Int]): List[String] =
    lines.toList
      .foldLeft((start, List.empty[String])) {
        case ((state, problems), line) =>
          val cursor                 = CursorPosition(line, 0)
          val placed                 = movedTo(state, cursor)
          val (row, paneTop, bottom) = painted(placed, cursor)
          val problem = Option.when(!row.exists(r => r >= paneTop && r < bottom))(
            s"line $line painted on row $row, pane rows $paneTop until $bottom"
          )
          (placed, problems ++ problem)
      }
      ._2

  private def walkedTo(start: AppState, line: Int): AppState =
    (0 to line).foldLeft(start)((state, next) => movedTo(state, CursorPosition(next, 0)))

  "Typewriter scrolling in a rich document" should "paint the caret as near the pane's middle as any top would, moving down" in {
    offCentre(richState(_.withTypewriterScrolling(true)), 12 until 60) shouldBe Nil
  }

  it should "paint the caret as near the middle moving back up" in {
    val start = walkedTo(richState(_.withTypewriterScrolling(true)), 60)
    offCentre(start, 50 to 20 by -1) shouldBe Nil
  }

  "Scrolling without typewriter mode in a rich document" should "keep the caret painted inside the pane" in {
    outsidePane(richState(_.withTypewriterScrolling(false)), 0 until 60) shouldBe Nil
  }

  "Typewriter scrolling in a rich document in the terminal" should "hold the caret as near the middle past blank heading rows" in {
    offCentre(richState(_.withTypewriterScrolling(true), terminal = true), 12 until 60) shouldBe Nil
  }
