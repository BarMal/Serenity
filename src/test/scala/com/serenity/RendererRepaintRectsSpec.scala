package com.serenity

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.serenity.config.StatusLinePlacement
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{
  CellMetrics,
  LayoutRect,
  PixelRect,
  ViewportSize,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import com.serenity.ui.renderer.{
  EditorPaneRenderPlan,
  FrameOutput,
  Java2DRenderSurface,
  RendererFramePlanner,
  ScreenIdentity
}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1891: a bounded repaint publishes each separate change as its own rect rather than one rect spanning everything
  * between them, and pane-header chrome joins it instead of forcing the whole canvas. Every case presents frames the
  * way the window does -- pooled base images, only the published rects copied onto the screen -- and checks the screen
  * against a cold render of the same state, so a rect left out shows up as stale pixels.
  */
class RendererRepaintRectsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(120, 40)
  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val metrics  = CellMetrics.fromFont(font)
  private val widthPx  = viewport.width * metrics.charWidth
  private val heightPx = viewport.height * metrics.lineHeight
  private val lines    = Vector.tabulate(60)(line => s"line $line")

  private def stateWith(cursor: CursorPosition, placement: StatusLinePlacement): AppState =
    val buffer = Buffer.fromString(bufferId, lines.mkString("\n")).copy(editing = EditingState(List(cursor)))
    val config = AppState.initial.persisted.config
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
        theme = Theme.light,
        config = config.copy(statusLine = config.statusLine.copy(placement = placement))
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

  /** The first keystroke into a clean buffer: one character typed, and the buffer turning dirty. */
  private def firstKeystroke(state: AppState, line: Int): AppState =
    val offset = lines.take(line).map(_.length + 1).sum
    withBuffer(state) { buffer =>
      buffer.copy(
        document = buffer.document.copy(
          content = buffer.document.content.insert(offset, "x").getOrElse(fail(s"expected insert at $offset")),
          isDirty = true
        ),
        editing = EditingState(List(CursorPosition(line, 1)))
      )
    }

  private def render(
    state: AppState,
    image: BufferedImage,
    persists: Boolean,
    output: Option[FrameOutput],
    damage: Damage,
    caches: RenderCaches,
    screen: ScreenIdentity
  ): EditorPaneRenderPlan =
    val surface = new Java2DRenderSurface(
      image,
      metrics,
      font,
      _ => (),
      logicalWidthPx = widthPx,
      logicalHeightPx = heightPx,
      contentPersists = persists,
      layerCacheOwnerOverride = Some(screen)
    )
    RendererFramePlanner
      .renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, font, font),
        font,
        font,
        font,
        metrics,
        metrics,
        None,
        output,
        damage,
        caches
      )
      .getOrElse(fail("expected an editor frame"))

  /** Frames presented the way `SwingWindow` presents them: drawn into two pooled images in turn, and only the published
    * rects of each copied onto what the screen shows.
    */
  final private class PresentedScreen:
    private val canvas       = new javax.swing.JPanel
    private val token        = ScreenIdentity(canvas)
    private val caches       = RenderCaches.create()
    private val pool         = Vector.fill(2)(new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB))
    private val frames       = new AtomicInteger(0)
    val shown: BufferedImage = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)

    def present(state: AppState, damage: Damage): (Option[List[PixelRect]], EditorPaneRenderPlan) =
      val image   = pool(frames.getAndIncrement() % pool.size)
      val region  = new AtomicReference[Option[List[PixelRect]]](None)
      val plan    = render(state, image, persists = true, Some(FrameOutput(token, region)), damage, caches, token)
      val painted = region.get().getOrElse(List(PixelRect(0, 0, widthPx, heightPx)))
      val g       = shown.createGraphics()
      try
        painted.foreach { rect =>
          val _ = g.drawImage(
            image,
            rect.xPx,
            rect.yPx,
            rect.rightPx,
            rect.bottomPx,
            rect.xPx,
            rect.yPx,
            rect.rightPx,
            rect.bottomPx,
            null
          )
        }
      finally g.dispose()
      (region.get(), plan)

    /** Shows `state` from scratch, then once more so both pooled images have drawn it: a steady state, not the window's
      * very first frames.
      */
    def settle(state: AppState): Unit =
      val _ = present(state, Damage.Everything)
      val _ = present(state, Damage.Nothing)

  private def cold(state: AppState): BufferedImage =
    val image = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val _ =
      render(state, image, persists = false, None, Damage.Everything, RenderCaches.create(), ScreenIdentity(image))
    image

  private def stalePixels(shown: BufferedImage, expected: BufferedImage): Int =
    (for
      x <- 0 until widthPx
      y <- 0 until heightPx
      if shown.getRGB(x, y) != expected.getRGB(x, y)
    yield 1).size

  private def pixelRectOf(rect: LayoutRect): PixelRect =
    PixelRect(
      metrics.toPixelX(rect.x),
      metrics.toPixelY(rect.y),
      rect.width * metrics.charWidth,
      rect.height * metrics.lineHeight
    )

  private def covers(rects: List[PixelRect], target: PixelRect): Boolean =
    (for
      x <- target.xPx until target.rightPx
      y <- target.yPx until target.bottomPx
    yield rects.exists(_.intersects(PixelRect(x, y, 1, 1)))).forall(identity)

  private def areaPx(rects: List[PixelRect]): Long = rects.map(rect => rect.widthPx.toLong * rect.heightPx).sum

  private val canvasAreaPx = widthPx.toLong * heightPx

  "A bounded repaint" should "cover only the caret rows and the pinned status row on a caret move" in {
    val screen = PresentedScreen()
    val before = stateWith(CursorPosition(10, 0), StatusLinePlacement.Pinned)
    val after  = movedCaret(before, CursorPosition(10, 4))

    screen.settle(before)
    val (region, plan) = screen.present(after, DamageProducer.forTransition(before, after))

    val rects     = region.getOrElse(fail("expected a bounded repaint"))
    val statusRow = plan.layoutContract.gutterRect.map(pixelRectOf).getOrElse(fail("expected a pinned status row"))
    withClue(s"rects $rects: ")(areaPx(rects).toDouble / canvasAreaPx should be < 0.10)
    withClue(s"rects $rects miss the status row $statusRow: ")(covers(rects, statusRow) shouldBe true)
    withClue("pixels left stale on screen: ")(stalePixels(screen.shown, cold(after)) shouldBe 0)
  }

  it should "cover only the caret rows when no status row is shown" in {
    val screen = PresentedScreen()
    val before = stateWith(CursorPosition(10, 0), StatusLinePlacement.Off)
    val after  = movedCaret(before, CursorPosition(10, 4))

    screen.settle(before)
    val (region, _) = screen.present(after, DamageProducer.forTransition(before, after))

    val rects = region.getOrElse(fail("expected a bounded repaint"))
    // One caret row, dilated by one row either side for glyph overflow, across at most the whole width.
    withClue(s"rects $rects: ")(areaPx(rects) should be <= 3L * metrics.lineHeight * widthPx)
    withClue("pixels left stale on screen: ")(stalePixels(screen.shown, cold(after)) shouldBe 0)
  }

  it should "repaint the edited row and the pane header, not the whole canvas, on the first keystroke" in {
    val screen = PresentedScreen()
    val before = stateWith(CursorPosition(10, 0), StatusLinePlacement.Off)
    val after  = firstKeystroke(before, line = 10)

    screen.settle(before)
    val (region, plan) = screen.present(after, DamageProducer.forTransition(before, after))

    val rects  = region.getOrElse(fail("expected a bounded repaint"))
    val header = plan.layoutContract.paneHeaderRect(paneId).map(pixelRectOf).getOrElse(fail("expected a pane header"))
    withClue(s"rects $rects miss the header $header: ")(covers(rects, header) shouldBe true)
    // The typed row dilated by one row either side, plus the header.
    withClue(s"rects $rects: ")(areaPx(rects) should be <= 3L * metrics.lineHeight * widthPx + areaPx(List(header)))
    withClue("pixels left stale on screen: ")(stalePixels(screen.shown, cold(after)) shouldBe 0)
  }

  it should "leave no stale tab-bar pixels when the first keystroke marks its tab dirty" in {
    val screen = PresentedScreen()
    val single = stateWith(CursorPosition(10, 0), StatusLinePlacement.Off)
    val other  = Buffer.fromString(BufferId(2), "other")
    val before = single.copy(persisted =
      single.persisted.copy(
        buffers = single.persisted.buffers.updated(other.id, other),
        bufferOrder = single.persisted.bufferOrder :+ other.id
      )
    )
    val after = firstKeystroke(before, line = 10)

    screen.settle(before)
    val _ = screen.present(after, DamageProducer.forTransition(before, after))

    withClue("pixels left stale on screen: ")(stalePixels(screen.shown, cold(after)) shouldBe 0)
  }

  it should "leave no stale pixels when a later frame draws into the image two frames behind" in {
    val screen = PresentedScreen()
    val first  = stateWith(CursorPosition(10, 0), StatusLinePlacement.Pinned)
    val second = firstKeystroke(first, line = 10)
    val third  = movedCaret(second, CursorPosition(30, 2))

    screen.settle(first)
    val _ = screen.present(second, DamageProducer.forTransition(first, second))
    val _ = screen.present(third, DamageProducer.forTransition(second, third))

    withClue("pixels left stale on screen: ")(stalePixels(screen.shown, cold(third)) shouldBe 0)
  }
