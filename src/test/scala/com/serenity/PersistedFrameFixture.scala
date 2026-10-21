package com.serenity

import java.awt.Font
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.serenity.config.{AppConfig, TextAreaInsets}
import com.serenity.diagnostics.FrameCounts
import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.testkit.FrameMismatchDump
import com.serenity.ui.layout.{CellMetrics, PixelRect, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.{EditorPaneRenderPlan, FrameOutput, RenderSurface, RendererFramePlanner, ScreenIdentity}
import com.serenity.ui.theme.Theme
import org.scalatest.matchers.should.Matchers

/** A window's frame loop at the laptop's device scale, for specs that compare what a damaged frame leaves in a
  * persisting image with a frame drawn from scratch: two pooled images drawn in turn, as `SwingWindow` alternates them.
  *
  * Which renderer paints the frames is [[frames]]: Java2D unless a spec overrides it, so one body of assertions runs
  * against every [[RenderSurface]] that claims to persist and copy pixels.
  */
trait PersistedFrameFixture:
  self: Matchers =>

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  protected def frames: FrameBackend = Java2DFrames

  protected val paneId: PaneId           = PaneId(0)
  protected val bufferId: BufferId       = BufferId(1)
  protected val codeFont: Font           = Font(Font.MONOSPACED, Font.PLAIN, 12)
  protected val textFont: Font           = Font(Font.SERIF, Font.PLAIN, 14)
  protected val uiFont: Font             = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  protected val cellMetrics: CellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics                  = CellMetrics.fromFont(uiFont)
  protected val logicalW                 = 900
  protected val logicalH                 = 600
  protected val viewport: ViewportSize =
    ViewportSize(logicalW / cellMetrics.charWidth, logicalH / cellMetrics.lineHeight)

  protected val codeLines: Vector[String] = Vector.tabulate(120)(line => s"line $line")
  protected val proseLines: Vector[String] =
    Vector.tabulate(80)(line => s"Paragraph $line " + "lorem ipsum dolor sit amet " * (4 + line % 5 * 4))

  protected def deviceSize(logicalPx: Int, scale: Double): Int = math.ceil(logicalPx * scale).toInt

  protected def withMargins(config: AppConfig): AppConfig =
    config.copy(surfaceConfig = config.surfaceConfig.copy(textAreaInsets = TextAreaInsets(0.04, 0.04, 0.04, 0.04)))

  protected def stateOf(
    text: String,
    cursor: CursorPosition,
    dirty: Boolean = true,
    topLine: Int = 0,
    language: Option[LanguageId] = None
  ): AppState =
    val base = Buffer.fromString(bufferId, text)
    val buffer = base.copy(
      document = base.document.copy(isDirty = dirty, language = language),
      viewport =
        Viewport(topLine = topLine, leftColumn = 0, visibleLines = viewport.height, visibleColumns = viewport.width),
      editing = EditingState(List(cursor))
    )
    val initial = AppState.initial(AppConfig.conventional)
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light,
        config = withMargins(AppConfig.conventional.withoutStatusLine)
      )
    )

  protected def typedAt(state: AppState, lines: Vector[String], line: Int, text: String): AppState =
    val offset = lines.take(line).map(_.length + 1).sum
    val buffer = state.persisted.buffers(bufferId)
    val edited = buffer.copy(
      document = buffer.document.copy(
        content = buffer.document.content.insert(offset, text).getOrElse(fail(s"expected insert at $offset")),
        isDirty = true
      ),
      editing = EditingState(List(CursorPosition(line, 1)))
    )
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, edited)))

  protected def scrolledTo(state: AppState, topLine: Int): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, buffer.copy(viewport = buffer.viewport.copy(topLine = topLine)))
      )
    )

  private def surfaceOver(frame: FramePixels, scale: Double, persists: Boolean, caches: RenderCaches): RenderSurface =
    frames.surfaceOver(frame, cellMetrics, codeFont, logicalW, logicalH, scale, persists, caches)

  final protected class Session(scale: Double = 2.0):
    private val caches = RenderCaches.create()
    private val token  = ScreenIdentity(new javax.swing.JPanel)
    caches.frameCounters.setEnabled(true)

    private val pool =
      Vector.fill(2)(frames.newFrame(deviceSize(logicalW, scale), deviceSize(logicalH, scale)))

    private val drawn  = new AtomicInteger(0)
    private val region = new AtomicReference[Option[List[PixelRect]]](None)

    /** The pixel region the last frame bounded its present to, `None` when it asked for the whole canvas. */
    def repaintRegion: Option[List[PixelRect]] = region.get()

    /** What the last frame drew, kept and presented, as the frame counters counted it. */
    def lastFrame: FrameCounts = caches.frameCounters.lastFrame

    /** The image the next frame draws into, which holds the frame before the last. */
    def pendingImage: FramePixels = pool(drawn.get() % pool.size)

    def draw(state: AppState, damage: Damage): (FramePixels, EditorPaneRenderPlan) =
      val image = pendingImage
      drawn.incrementAndGet()
      val plan = RendererFramePlanner
        .renderFrame(
          state,
          cursorVisible = false,
          surfaceOver(image, scale, persists = true, caches),
          viewport,
          caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics,
          None,
          Some(FrameOutput(token, region)),
          damage,
          caches
        )
        .getOrElse(fail("expected an editor frame"))
      (image, plan)

    /** Draws `state` into both images, so the next frame meets a steady window rather than its first frames. */
    def settle(state: AppState): EditorPaneRenderPlan =
      val (_, plan) = draw(state, Damage.Everything)
      val _         = draw(state, Damage.Nothing)
      plan

  protected def cold(state: AppState, scale: Double = 2.0): FramePixels =
    val image  = frames.newFrame(deviceSize(logicalW, scale), deviceSize(logicalH, scale))
    val caches = RenderCaches.create()
    val _ = RendererFramePlanner.renderFrame(
      state,
      cursorVisible = false,
      surfaceOver(image, scale, persists = false, caches),
      viewport,
      caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      None,
      None,
      Damage.Everything,
      caches
    )
    image

  protected def differingPixels(a: FramePixels, b: FramePixels): Seq[(Int, Int)] =
    val left  = a.rgbPixels
    val right = b.rgbPixels
    val differing =
      for
        y <- 0 until a.heightPx
        x <- 0 until a.widthPx
        if (left(y * a.widthPx + x) & 0xffffff) != (right(y * b.widthPx + x) & 0xffffff)
      yield (x, y)
    if differing.nonEmpty then
      println("DEBUG " + FrameMismatchDump.write(getClass.getSimpleName, a.toImage, b.toImage, differing))
    differing
