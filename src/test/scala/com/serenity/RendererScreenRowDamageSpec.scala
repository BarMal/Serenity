package com.serenity

import com.serenity.config.StatusLinePlacement
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1891: damage is translated into the screen rows it actually touches -- a caret move dirties the visual rows under
  * the old and new caret rather than every row its paragraph wraps into, and status/line-number chrome joins the
  * bounded repaint region instead of forcing the whole canvas to repaint.
  */
class RendererScreenRowDamageSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)
  private val font     = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val metrics  = CellMetrics.fromFont(font)

  private val wrappedParagraph = (0 until 200).map(index => f"w$index%03d").mkString(" ")

  private def columnOfWord(index: Int): Int = index * 5

  private def stateWith(content: String, cursor: CursorPosition): AppState =
    val buffer = Buffer.fromString(bufferId, content).copy(editing = EditingState(List(cursor)))
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light
      )
    )

  private def withBuffer(state: AppState)(update: Buffer => Buffer): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, update(state.persisted.buffers(bufferId)))
      )
    )

  private def movedCaret(state: AppState, cursor: CursorPosition): AppState =
    withBuffer(state)(_.copy(editing = EditingState(List(cursor))))

  private def inserted(state: AppState, offset: Int, text: String, cursor: CursorPosition): AppState =
    withBuffer(state) { buffer =>
      buffer.copy(
        document = buffer.document.copy(content =
          buffer.document.content.insert(offset, text).getOrElse(fail(s"expected insert at $offset to succeed"))
        ),
        editing = EditingState(List(cursor))
      )
    }

  private def withStatusLine(state: AppState, placement: StatusLinePlacement): AppState =
    val config = state.persisted.config
    state.copy(persisted =
      state.persisted.copy(config = config.copy(statusLine = config.statusLine.copy(placement = placement)))
    )

  private def render(surface: MockRenderSurface, state: AppState, damage: Damage, caches: RenderCaches) =
    RendererEntryPoints.renderWithRepaintRegion(
      state,
      cursorVisible = false,
      surface,
      viewport,
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      metrics,
      metrics,
      None,
      damage,
      caches
    )

  private def drew(surface: MockRenderSurface, text: String): Boolean =
    (surface.putStringCalls.map(_.s) ++ surface.drawRunPxCalls.map(_.s)).exists(_.contains(text))

  private val canvasHeightPx = viewport.height * metrics.lineHeight

  "Screen-row damage" should "redraw only the rows under the old and new caret when it moves within a wrapped paragraph" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val before  = stateWith(wrappedParagraph, CursorPosition(0, 0))
    val after   = movedCaret(before, CursorPosition(0, columnOfWord(40)))
    val caches  = RenderCaches.create()

    val _ = render(surface, before, Damage.Everything, caches)
    surface.clear()
    val _ = render(surface, after, DamageProducer.forTransition(before, after), caches)

    withClue("the old caret's row: ")(drew(surface, "w000") shouldBe true)
    withClue("the new caret's row: ")(drew(surface, "w040") shouldBe true)
    withClue("a row of the same paragraph far from either caret: ")(drew(surface, "w150") shouldBe false)
  }

  it should "bound the repaint region on a caret move instead of repainting the whole canvas" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val before  = stateWith(Vector.tabulate(20)(line => s"line $line").mkString("\n"), CursorPosition(3, 0))
    val after   = movedCaret(before, CursorPosition(3, 4))
    val caches  = RenderCaches.create()

    val _      = render(surface, before, Damage.Everything, caches)
    val region = render(surface, after, DamageProducer.forTransition(before, after), caches)

    // The pinned status row shows the caret's column, so it joins the region -- as a rect, not a whole-canvas veto.
    region should not be None
    region.map(_.heightPx).getOrElse(canvasHeightPx) should be < canvasHeightPx
  }

  it should "keep a caret move's repaint region to the caret rows when no status row shows the caret" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val lines   = Vector.tabulate(20)(line => s"line $line").mkString("\n")
    val before  = withStatusLine(stateWith(lines, CursorPosition(3, 0)), StatusLinePlacement.Off)
    val after   = movedCaret(before, CursorPosition(3, 4))
    val caches  = RenderCaches.create()

    val _      = render(surface, before, Damage.Everything, caches)
    val region = render(surface, after, DamageProducer.forTransition(before, after), caches)

    // One caret row, dilated by one row either side for glyph overflow.
    region.map(_.heightPx).getOrElse(0) should (be > 0 and be <= 3 * metrics.lineHeight)
  }

  it should "repaint only the typed paragraph's rows, not the rows above it, when one character is typed" in {
    val surface   = new MockRenderSurface(80, 24, persistentContent = true)
    val lines     = Vector.tabulate(20)(line => s"line $line")
    val lineStart = lines.take(10).map(_.length + 1).sum
    val before    = withStatusLine(stateWith(lines.mkString("\n"), CursorPosition(10, 0)), StatusLinePlacement.Off)
    val after     = inserted(before, lineStart, "x", CursorPosition(10, 1))
    val caches    = RenderCaches.create()

    val _      = render(surface, before, Damage.Everything, caches)
    val region = render(surface, after, DamageProducer.forTransition(before, after), caches)

    // The typed row, dilated by one row either side for glyph overflow.
    region.map(_.heightPx).getOrElse(0) should (be > 0 and be <= 3 * metrics.lineHeight)
  }

  it should "carry a shifted row's line number into the repaint region along with the row" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val lines   = Vector.tabulate(10)(line => s"line $line")
    val before  = stateWith(lines.mkString("\n"), CursorPosition(0, 0))
    // A newline inserted with the caret left where it was: every row below shifts down a line number, and nothing
    // about the caret or the status row changes to drag the line-number column in by some other route.
    val after  = inserted(before, lines.head.length, "\nnew", CursorPosition(0, 0))
    val caches = RenderCaches.create()

    val _ = render(surface, before, Damage.Everything, caches)
    surface.clear()
    val region = render(surface, after, DamageProducer.forTransition(before, after), caches)

    // Line 11 exists only after the insert; its number is painted in the line-number column.
    val lineNumberLeftPx =
      surface.putStringCalls.filter(_.s.trim == "11").map(_.x * metrics.charWidth) ++
        surface.drawRunPxCalls.filter(_.s.trim == "11").map(_.xPx.toInt)
    lineNumberLeftPx should not be empty
    region.map(_.xPx).getOrElse(Int.MaxValue) should be <= lineNumberLeftPx.min
  }
