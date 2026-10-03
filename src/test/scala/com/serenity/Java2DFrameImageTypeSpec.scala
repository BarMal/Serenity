package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.{Java2DRenderSurface, RendererEntryPoints}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** An opaque window's frames are drawn into `TYPE_INT_RGB` images, which Java2D blends glyphs into faster than
  * `TYPE_INT_ARGB`. What reaches the screen must not change: an ARGB frame is presented over the canvas's opaque black
  * background, so an RGB frame has to come out as that composite, pixel for pixel.
  */
class Java2DFrameImageTypeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val metrics  = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12)
  private val font     = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val viewport = ViewportSize(60, 20)
  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val text     = (1 to 30).map(i => s"line $i: the quick brown fox jumps over the lazy dog").mkString("\n")

  private def stateWith(theme: Theme, content: String = text): AppState =
    val buffer = Buffer.fromString(bufferId, content).copy(editing = EditingState(List(CursorPosition(3, 5))))
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        theme = theme
      )
    )

  private def canvas: JPanel =
    val panel = new JPanel()
    panel.setSize(viewport.width * metrics.charWidth, viewport.height * metrics.lineHeight)
    panel

  private def frame(state: AppState, imageType: Int): BufferedImage =
    val image = new BufferedImage(
      viewport.width * metrics.charWidth,
      viewport.height * metrics.lineHeight,
      imageType
    )
    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      new Java2DRenderSurface(image, metrics, font, _ => ()),
      viewport,
      font,
      font,
      metrics,
      None,
      RenderCaches.create()
    )
    image

  /** `image` as the canvas presents it: drawn over opaque black. */
  private def presented(image: BufferedImage): BufferedImage =
    val onScreen = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_RGB)
    val g        = onScreen.createGraphics()
    try
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, image.getWidth, image.getHeight)
      g.drawImage(image, 0, 0, null)
    finally g.dispose()
    onScreen

  private def differingPixels(a: BufferedImage, b: BufferedImage): Int =
    (for
      y <- 0 until a.getHeight
      x <- 0 until a.getWidth
      if a.getRGB(x, y) != b.getRGB(x, y)
    yield 1).size

  "SwingWindow.frameImageType" should "choose RGB for an opaque window showing an opaque theme" in {
    SwingWindow.frameImageType(translucentWindow = false, backgroundAlpha = 255) shouldBe BufferedImage.TYPE_INT_RGB
  }

  it should "keep ARGB for a translucent window" in {
    SwingWindow.frameImageType(translucentWindow = true, backgroundAlpha = 255) shouldBe BufferedImage.TYPE_INT_ARGB
  }

  it should "keep ARGB whenever the theme background is not opaque, including the transparent-backdrop sentinel" in {
    SwingWindow.frameImageType(translucentWindow = false, backgroundAlpha = 0) shouldBe BufferedImage.TYPE_INT_ARGB
    SwingWindow.frameImageType(translucentWindow = false, backgroundAlpha = 128) shouldBe BufferedImage.TYPE_INT_ARGB
    SwingWindow.frameImageType(translucentWindow = true, backgroundAlpha = 0) shouldBe BufferedImage.TYPE_INT_ARGB
  }

  "SwingWindow.pooledFrameImages" should "give a frame the window's image type, not the renderer's default" in {
    val pool = new SwingWindow.ReusableImagePool
    val types = List(BufferedImage.TYPE_INT_RGB, BufferedImage.TYPE_INT_ARGB).map { imageType =>
      val flushed = new AtomicReference[Option[BufferedImage]](None)
      Java2DRenderSurface
        .forFrame(
          metrics,
          font,
          canvas,
          image => flushed.set(Some(image)),
          SwingWindow.pooledFrameImages(pool, imageType)
        )
        .flush()
      flushed.get().map(_.getType)
    }
    types shouldBe List(Some(BufferedImage.TYPE_INT_RGB), Some(BufferedImage.TYPE_INT_ARGB))
  }

  "An RGB frame" should "present exactly what an ARGB frame presents over the opaque canvas" in
    List(Theme.light, Theme.default).foreach { theme =>
      val state = stateWith(theme)
      differingPixels(
        frame(state, BufferedImage.TYPE_INT_RGB),
        presented(frame(state, BufferedImage.TYPE_INT_ARGB))
      ) shouldBe 0
    }

  it should "show an alpha-0 fill as the black an ARGB frame shows through to" in {
    def filled(imageType: Int): BufferedImage =
      val image   = new BufferedImage(40, 32, imageType)
      val surface = new Java2DRenderSurface(image, metrics, font, _ => ())
      surface.setBackgroundColor(Color.WHITE)
      surface.fillRect(0, 0, 5, 2, ' ')
      surface.setBackgroundColor(new Color(200, 100, 50, 0))
      surface.setForegroundColor(Color.YELLOW)
      surface.putString(1, 0, "ab")
      surface.flush()
      image

    differingPixels(filled(BufferedImage.TYPE_INT_RGB), presented(filled(BufferedImage.TYPE_INT_ARGB))) shouldBe 0
  }

  "Switching frame image type mid-session" should "redraw the whole frame rather than reuse another image's pixels" in {
    val pool   = new SwingWindow.ReusableImagePool
    val caches = RenderCaches.create()
    val panel  = canvas
    val before = stateWith(Theme.light)
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.map((id, buffer) =>
          id -> buffer.copy(document =
            buffer.document.copy(content = buffer.document.content.insert(10, "edited ").getOrElse(fail("insert")))
          )
        )
      )
    )
    def pooledFrame(state: AppState, damage: Damage, imageType: Int): BufferedImage =
      val surface =
        Java2DRenderSurface.forFrame(metrics, font, panel, pool.publish, SwingWindow.pooledFrameImages(pool, imageType))
      RendererEntryPoints.render(
        state,
        cursorVisible = true,
        surface,
        viewport,
        font,
        font,
        metrics,
        None,
        damage,
        caches
      )
      surface.persistentContentKey.fold(fail("a pooled frame persists its content"))(_ => ())
      pool.leasePublished().getOrElse(fail("the frame was not published"))

    val argb = pooledFrame(before, Damage.Everything, BufferedImage.TYPE_INT_ARGB)
    pool.releaseLease()
    val rgb = pooledFrame(after, DamageProducer.forTransition(before, after), BufferedImage.TYPE_INT_RGB)
    pool.releaseLease()

    argb.getType shouldBe BufferedImage.TYPE_INT_ARGB
    rgb.getType shouldBe BufferedImage.TYPE_INT_RGB
    differingPixels(rgb, frame(after, BufferedImage.TYPE_INT_RGB)) shouldBe 0
  }
end Java2DFrameImageTypeSpec
