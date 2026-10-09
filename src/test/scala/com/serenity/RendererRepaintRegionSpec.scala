package com.serenity

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.{PixelRect, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `RendererEntryPoints.renderWithRepaintRegion`'s bounded-vs-unbounded repaint-rectangle calculation -- split
  * out of `RendererDirtyRegionSpec` (which covers the row-level dirty-line drawing this sits on top of) to keep that
  * file under the architecture ratchet's file-length target (#1677).
  */
class RendererRepaintRegionSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)

  private val lines = Vector("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta")

  private val font = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

  // The logical monospaced font differs by OS (15 px rows on Linux, 17 px on Windows), so a whole-canvas height
  // measured in a fixed 16 px per row would pass or fail depending only on where the spec runs.
  private val canvasHeightPx = viewport.height * com.serenity.ui.layout.CellMetrics.fromFont(font).lineHeight

  private def stateWith(content: Vector[String], cursor: CursorPosition = CursorPosition(0, 0)): AppState =
    val buffer0 = Buffer.fromString(bufferId, content.mkString("\n"))
    val buffer  = buffer0.copy(editing = EditingState(List(cursor)))
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

  private def repaintRegionFor(
    surface: MockRenderSurface,
    state: AppState,
    damage: Damage,
    caches: com.serenity.state.manager.RenderCaches
  ): Option[PixelRect] =
    RendererEntryPoints.renderWithRepaintRegion(
      state,
      cursorVisible = false,
      surface,
      viewport,
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      com.serenity.ui.layout.CellMetrics.fromFont(font),
      com.serenity.ui.layout.CellMetrics.fromFont(font),
      None,
      damage,
      caches
    )

  "The repaint region" should "cover the whole canvas for the first frame" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    repaintRegionFor(
      surface,
      stateWith(lines),
      Damage.Nothing,
      com.serenity.state.manager.RenderCaches.create()
    ) shouldBe None
  }

  it should "be empty when the frame is identical to the one on screen" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)
    val caches  = com.serenity.state.manager.RenderCaches.create()

    val _ = repaintRegionFor(surface, state, Damage.Everything, caches)

    repaintRegionFor(
      surface,
      state,
      DamageProducer.forTransition(state, state),
      caches
    ) shouldBe Some(PixelRect(0, 0, 0, 0))
  }

  it should "cover only the edited row when one line changes" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)
    // Keep every other state object identical, exactly as an edit does in the app: the buffer's rope is edited in
    // place via insert (preserving the shared tree structure RopeDiff needs for a narrow diff), so the chrome around
    // the pane is provably unchanged and the repaint can stay bounded.
    val zetaEndOffset = lines.take(6).map(_.length + 1).sum - 1
    val edited = state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          bufferId,
          state.persisted
            .buffers(bufferId)
            .copy(document =
              state.persisted
                .buffers(bufferId)
                .document
                .copy(content =
                  state.persisted
                    .buffers(bufferId)
                    .document
                    .content
                    .insert(zetaEndOffset, "X")
                    .getOrElse(fail("expected insert to succeed"))
                )
            )
        )
      )
    )

    val caches = com.serenity.state.manager.RenderCaches.create()
    val _      = repaintRegionFor(surface, state, Damage.Everything, caches)
    val region = repaintRegionFor(surface, edited, DamageProducer.forTransition(state, edited), caches)

    region.map(_.heightPx).getOrElse(0) should be > 0
    region.map(_.heightPx).getOrElse(0) should be < canvasHeightPx
  }

  it should "stay bounded when a cursor move also changes the status row" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val before  = stateWith(lines, CursorPosition(0, 0))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing = EditingState(List(CursorPosition(3, 2))))
        )
      )
    )

    val caches = com.serenity.state.manager.RenderCaches.create()
    val _      = repaintRegionFor(surface, before, Damage.Everything, caches)

    // The status row shows the cursor's line/column, so a cursor move also reports Chrome damage -- which joins the
    // region as the status row's own rect rather than forcing a whole-canvas repaint (#1835, #1891).
    val region = repaintRegionFor(surface, after, DamageProducer.forTransition(before, after), caches)
    region should not be None
    region.map(_.heightPx).getOrElse(0) should be < canvasHeightPx
  }
