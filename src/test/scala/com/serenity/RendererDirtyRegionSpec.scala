package com.serenity

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, LayoutEngine, LayoutManager, PixelRect, TextLayoutSnapshot, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end cover for dirty-line rendering of editor pane text: a surface that keeps its pixels between frames must
  * only be asked to draw the rows that actually changed, and everything else must still be drawn exactly as before.
  *
  * `planFrame` no longer discovers this by diffing two frames itself -- it trusts the `Damage` the caller reports for
  * the transition, so every second call here passes what [[DamageProducer.forTransition]] actually reports for
  * `before`/`after`, exactly as `AppRuntime` does in production.
  */
class RendererDirtyRegionSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)

  private val lines = Vector("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta")

  private def stateWith(content: Vector[String], cursor: CursorPosition = CursorPosition(0, 0)): AppState =
    val buffer0 = Buffer.fromString(bufferId, content.mkString("\n"))
    val buffer  = buffer0.copy(editing = buffer0.editing.copy(cursors = List(cursor)))
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light
      )
    )

  /** Every string the frame asked the surface to paint, whichever text path it took. */
  private def drawnText(surface: MockRenderSurface): List[String] =
    surface.putStringCalls.map(_.s) ++ surface.drawRunPxCalls.map(_.s)

  private def drew(surface: MockRenderSurface, text: String): Boolean =
    drawnText(surface).exists(_.contains(text))

  "Dirty-line rendering" should "draw every visible row on the first frame" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    Renderer.render(stateWith(lines), cursorVisible = false, surface, viewport, None, Damage.Nothing)

    lines.foreach(line => drew(surface, line) shouldBe true)
  }

  it should "draw no pane rows again when nothing changed" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)

    Renderer.render(state, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(state, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(state, state))

    lines.foreach(line => drew(surface, line) shouldBe false)
  }

  it should "draw pane rows again when the surface does not preserve its pixels" in {
    val surface = new MockRenderSurface(80, 24)
    val state   = stateWith(lines)

    Renderer.render(state, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(state, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(state, state))

    lines.foreach(line => drew(surface, line) shouldBe true)
  }

  it should "draw only the edited row again when one line changes" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val before  = stateWith(lines)
    // An edit via the rope's own insert, not a second independent Buffer.fromString call: RopeDiff finds a narrow
    // changed range by walking shared tree structure between before/after, which two freshly-built ropes with no
    // shared lineage don't have -- matching how a real keystroke actually mutates the buffer in production.
    val zetaEndOffset = lines.take(6).map(_.length + 1).sum - 1
    val editedContent =
      before.persisted
        .buffers(bufferId)
        .document
        .content
        .insert(zetaEndOffset, "X")
        .getOrElse(fail("expected insert to succeed"))
    val after =
      before.copy(persisted =
        before.persisted.copy(buffers =
          before.persisted.buffers.updated(
            bufferId,
            before.persisted
              .buffers(bufferId)
              .copy(document = before.persisted.buffers(bufferId).document.copy(content = editedContent))
          )
        )
      )

    Renderer.render(before, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(after, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(before, after))

    drew(surface, "zetaX") shouldBe true
    drew(surface, "alpha") shouldBe false
    drew(surface, "gamma") shouldBe false
  }

  it should "draw the rows the caret left and entered again when it moves" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val before  = stateWith(lines, CursorPosition(0, 0))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing = before.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(5, 0))))
        )
      )
    )

    Renderer.render(before, cursorVisible = true, surface, viewport)
    surface.clear()
    Renderer.render(after, cursorVisible = true, surface, viewport, None, DamageProducer.forTransition(before, after))

    drew(surface, "alpha") shouldBe true
    drew(surface, "zeta") shouldBe true
    drew(surface, "gamma") shouldBe false
  }

  it should "draw only the rows a new selection covers again" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val before  = stateWith(lines)
    val after = before.copy(persisted =
      before.persisted.copy(
        buffers = before.persisted.buffers.view
          .mapValues(buf =>
            buf
              .copy(editing = buf.editing.copy(selection = Some(Selection(CursorPosition(5, 0), CursorPosition(5, 4)))))
          )
          .toMap
      )
    )

    Renderer.render(before, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(after, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(before, after))

    drew(surface, "zeta") shouldBe true
    drew(surface, "alpha") shouldBe false
    drew(surface, "gamma") shouldBe false
  }

  it should "draw every row again when the theme changes" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val before  = stateWith(lines)
    val after   = before.copy(persisted = before.persisted.copy(theme = Theme.default))

    Renderer.render(before, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(after, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(before, after))

    lines.foreach(line => drew(surface, line) shouldBe true)
  }

  it should "still draw the gutter and line numbers on an otherwise unchanged frame" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)

    Renderer.render(state, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(state, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(state, state))

    drawnText(surface).mkString should include("Line 1, Col 1")
  }

  "The repaint region" should "cover the whole canvas for the first frame" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    repaintRegionFor(surface, stateWith(lines), Damage.Nothing) shouldBe None
  }

  it should "be empty when the frame is identical to the one on screen" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)

    val _ = repaintRegionFor(surface, state, Damage.Everything)

    repaintRegionFor(surface, state, DamageProducer.forTransition(state, state)) shouldBe Some(PixelRect(0, 0, 0, 0))
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

    val _      = repaintRegionFor(surface, state, Damage.Everything)
    val region = repaintRegionFor(surface, edited, DamageProducer.forTransition(state, edited))

    region.map(_.heightPx).getOrElse(0) should be > 0
    region.map(_.heightPx).getOrElse(0) should be < viewport.height * 16
  }

  it should "cover the whole canvas when the chrome changed too" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val before  = stateWith(lines, CursorPosition(0, 0))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing = before.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(3, 2))))
        )
      )
    )

    val _ = repaintRegionFor(surface, before, Damage.Everything)

    // The gutter shows the cursor's line/column, so a cursor move also reports Chrome damage -- which
    // Damage.isBufferRowsOnly excludes, correctly falling back to an unbounded (whole-canvas) repaint.
    repaintRegionFor(surface, after, DamageProducer.forTransition(before, after)) shouldBe None
  }

  "Dirty-line rendering" should "redraw the pane rows a floating panel vacates when it moves, without any buffer change" in {
    val surface   = new MockRenderSurface(80, 24, persistentContent = true)
    val surfaceId = SurfaceId("floating-panel")
    val state     = stateWith(lines)

    def withPanelAnchor(anchor: CursorPosition): AppState =
      state.copy(runtime =
        state.runtime.copy(
          uiSurfaces = List(
            UiSurface(
              id = surfaceId,
              content = SurfaceContent.CursorInfoBar("info"),
              presentation = SurfacePresentation.Floating(Some(anchor), SurfacePlacement.AboveCursor)
            )
          )
        )
      )

    val before = withPanelAnchor(CursorPosition(3, 0))
    val after  = withPanelAnchor(CursorPosition(7, 0))

    // Sanity-check the premise: only the floating panel's own position changed (the buffer, cursor, layout and
    // config are all identical), so the transition damage is scoped narrowly to that one surface.
    val damage = DamageProducer.forTransition(before, after)
    damage shouldBe Damage.Surface(surfaceId)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.clear()
    Renderer.render(after, cursorVisible = false, surface, viewport, None, damage)

    // The panel repainted its own new rect (around row 7) but not the rows it used to sit over (around row 3) -- a
    // pane row under the vacated rect must be redrawn even though nothing about the buffer content changed there, or
    // a transparent theme would leave the panel's old opaque background stuck on screen forever. "alpha" sits under
    // the panel's old (row-3-anchored) rect and must come back; "epsilon" sits under neither the old nor the new rect
    // and must stay preserved -- this isn't a fallback to redrawing the whole pane.
    drew(surface, "alpha") shouldBe true
    drew(surface, "epsilon") shouldBe false
  }

  "Dirty-line rendering" should
    "redraw a later paragraph's screen row after an earlier paragraph's edit reflows it down a row" in {
      val surface = new MockRenderSurface(80, 24, persistentContent = true)

      val words                               = Vector.tabulate(80)(i => f"word$i%02d")
      def paragraphOf(n: Int): String        = words.take(n).mkString(" ")
      def contentFor(n: Int): Vector[String] = Vector(paragraphOf(n), "MARKERLINE", "TAILLINE")

      // How many visual rows the first paragraph's `n` words wrap into, computed the same way `Renderer` itself
      // derives a pane's wrap width (`contentRect.width` cells -> `panelWidthPx` -> `TextLayoutSnapshot.fromBuffer`),
      // so the word count found below reflects this test's real viewport/pane geometry rather than a guessed
      // constant.
      def wrappedRowCountFor(n: Int): Int =
        val state            = stateWith(contentFor(n))
        val calculatedLayout = LayoutManager.calculateLayout(state, viewport)
        val workspaceLayout  = LayoutEngine.calculateEditorWorkspaceLayout(state, calculatedLayout)
        val contentRect  = workspaceLayout.activeContentRect(state).getOrElse(fail("expected an active content rect"))
        val font         = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
        val cellMetrics  = CellMetrics.fromFont(font)
        val panelWidthPx = contentRect.width * cellMetrics.charWidth
        val rawBuffer    = state.persisted.buffers(bufferId)
        val buffer =
          rawBuffer.copy(viewport =
            LayoutEngine.updateBufferViewportDimensions(rawBuffer, contentRect, wordWrapEnabled = true)
          )
        val snapshot = TextLayoutSnapshot.fromBuffer(
          buffer,
          panelWidthPx,
          font,
          TextLayoutSnapshot.defaultFontRenderContext(),
          wordWrapEnabled = true,
          cellMetricsOverride = Some(cellMetrics)
        )
        snapshot.visualLines.count(_.bufferLine == 0)

      // The fewest words whose paragraph wraps onto one more visual row than one word fewer -- the exact wrap
      // boundary a user crosses by typing one more word at the end of a long line.
      val n = LazyList
        .from(1)
        .take(words.length - 1)
        .find(k => wrappedRowCountFor(k + 1) > wrappedRowCountFor(k))
        .getOrElse(fail("expected some word count in range to cross a wrap boundary"))

      val before = stateWith(contentFor(n))
      // Insert the extra word via the rope's own insert (not a second independent Buffer.fromString call), exactly
      // like the "only the edited row again" test above, and exactly how a real keystroke extends the paragraph.
      val paragraphEndOffset = paragraphOf(n).length
      val editedContent =
        before.persisted
          .buffers(bufferId)
          .document
          .content
          .insert(paragraphEndOffset, " " + words(n))
          .getOrElse(fail("expected insert to succeed"))
      val after =
        before.copy(persisted =
          before.persisted.copy(buffers =
            before.persisted.buffers.updated(
              bufferId,
              before.persisted
                .buffers(bufferId)
                .copy(document = before.persisted.buffers(bufferId).document.copy(content = editedContent))
            )
          )
        )

      val damage = DamageProducer.forTransition(before, after)

      Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
      surface.clear()
      Renderer.render(after, cursorVisible = false, surface, viewport, None, damage)

      // MARKERLINE's own buffer line, cursor, selection and content are all unchanged -- only paragraph 0's extra
      // wrapped row pushed it one screen row further down. It must still be redrawn there, or the frame leaves
      // whatever was on screen at its new row (stale pixels, or nothing) instead of "MARKERLINE".
      drew(surface, "MARKERLINE") shouldBe true
      // TAILLINE, further downstream still, must be redrawn for the same reason.
      drew(surface, "TAILLINE") shouldBe true
    }

  private def repaintRegionFor(surface: MockRenderSurface, state: AppState, damage: Damage): Option[PixelRect] =
    val font = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    Renderer.renderWithRepaintRegion(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      font,
      com.serenity.ui.layout.CellMetrics.fromFont(font),
      com.serenity.ui.layout.CellMetrics.fromFont(font),
      None,
      damage
    )
