package com.serenity

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.serenity.config.{AppConfig, TextAreaInsets}
import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.{
  EditorPaneRenderPlan,
  FrameOutput,
  Java2DRenderSurface,
  RendererFramePlanner,
  ScreenIdentity
}
import com.serenity.ui.theme.Theme
import org.scalatest.matchers.should.Matchers

/** A window's frame loop at the laptop's device scale, for specs that compare what a damaged frame leaves in a
  * persisting image with a frame drawn from scratch: two pooled images drawn in turn, as `SwingWindow` alternates them.
  */
trait PersistedFrameFixture:
  self: Matchers =>

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

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
    val initial = AppState.initial
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
        config = withMargins(AppConfig.default.withoutStatusLine)
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

  private def surfaceOver(image: BufferedImage, scale: Double, persists: Boolean): Java2DRenderSurface =
    new Java2DRenderSurface(
      image,
      cellMetrics,
      codeFont,
      _ => (),
      logicalWidthPx = logicalW,
      logicalHeightPx = logicalH,
      deviceScaleX = scale,
      deviceScaleY = scale,
      contentPersists = persists,
      layerCacheOwnerOverride = Some(ScreenIdentity(image))
    )

  final protected class Session(scale: Double = 2.0):
    private val caches = RenderCaches.create()
    private val token  = ScreenIdentity(new javax.swing.JPanel)

    private val pool =
      Vector.fill(2)(
        new BufferedImage(
          deviceSize(logicalW, scale),
          deviceSize(logicalH, scale),
          BufferedImage.TYPE_INT_RGB
        )
      )

    private val frames = new AtomicInteger(0)

    /** The image the next frame draws into, which holds the frame before the last. */
    def pendingImage: BufferedImage = pool(frames.get() % pool.size)

    def draw(state: AppState, damage: Damage): (BufferedImage, EditorPaneRenderPlan) =
      val image = pendingImage
      frames.incrementAndGet()
      val plan = RendererFramePlanner
        .renderFrame(
          state,
          cursorVisible = false,
          surfaceOver(image, scale, persists = true),
          viewport,
          caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics,
          None,
          Some(FrameOutput(token, new AtomicReference(None))),
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

  protected def cold(state: AppState, scale: Double = 2.0): BufferedImage =
    val image  = new BufferedImage(deviceSize(logicalW, scale), deviceSize(logicalH, scale), BufferedImage.TYPE_INT_RGB)
    val caches = RenderCaches.create()
    val _ = RendererFramePlanner.renderFrame(
      state,
      cursorVisible = false,
      surfaceOver(image, scale, persists = false),
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

  protected def differingPixels(a: BufferedImage, b: BufferedImage): Seq[(Int, Int)] =
    for
      x <- 0 until a.getWidth
      y <- 0 until a.getHeight
      if (a.getRGB(x, y) & 0xffffff) != (b.getRGB(x, y) & 0xffffff)
    yield (x, y)
