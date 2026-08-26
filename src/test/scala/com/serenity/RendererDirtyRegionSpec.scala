package com.serenity

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.{PixelRect, ViewportSize}
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
    AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      theme = Theme.light
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
    val editedContent = before.buffers(bufferId).document.content.insert(zetaEndOffset, "X")
    val after =
      before.copy(buffers =
        before.buffers.updated(
          bufferId,
          before.buffers(bufferId).copy(document = before.buffers(bufferId).document.copy(content = editedContent))
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
    val after = before.copy(buffers =
      before.buffers.updated(
        bufferId,
        before
          .buffers(bufferId)
          .copy(editing = before.buffers(bufferId).editing.copy(cursors = List(CursorPosition(5, 0))))
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
    val after = before.copy(
      buffers = before.buffers.view
        .mapValues(buf =>
          buf.copy(editing = buf.editing.copy(selection = Some(Selection(CursorPosition(5, 0), CursorPosition(5, 4)))))
        )
        .toMap
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
    val after   = before.copy(theme = Theme.default)

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
    val edited = state.copy(buffers =
      state.buffers.updated(
        bufferId,
        state
          .buffers(bufferId)
          .copy(document =
            state
              .buffers(bufferId)
              .document
              .copy(content = state.buffers(bufferId).document.content.insert(zetaEndOffset, "X"))
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
    val after = before.copy(buffers =
      before.buffers.updated(
        bufferId,
        before
          .buffers(bufferId)
          .copy(editing = before.buffers(bufferId).editing.copy(cursors = List(CursorPosition(3, 2))))
      )
    )

    val _ = repaintRegionFor(surface, before, Damage.Everything)

    // The gutter shows the cursor's line/column, so a cursor move also reports Chrome damage -- which
    // Damage.isBufferRowsOnly excludes, correctly falling back to an unbounded (whole-canvas) repaint.
    repaintRegionFor(surface, after, DamageProducer.forTransition(before, after)) shouldBe None
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
