package com.serenity.perf

import java.awt.image.BufferedImage
import java.awt.{Font, Rectangle}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandRegistry
import com.serenity.config.{AppConfig, StatusLinePlacement, TabStripMode}
import com.serenity.diagnostics.FrameCounter
import com.serenity.keystroke.events.{Event, InsertChar}
import com.serenity.richtext.RichTextDocument
import com.serenity.state.manager.StateManagerTestFacade.{executeCommand, updateState}
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{BundledInterFont, CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, RendererFramePlanner, ScreenIdentity}
import com.serenity.{FrameBackend, FramePixels, SkiaFrames, setCursorPosition}

/** Draws a document through the window's incremental path, one image of a pool at a time, and compares each frame with
  * a cold frame of the same state. The window shows only the rects each frame names, as in
  * [[PageDownRichDocumentFramesSpec]]; the font is pinned so the wraps do not depend on what the platform maps a
  * logical family to.
  */
trait ColdFrameHarness:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  protected val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  protected val textFont    = BundledInterFont.plain(15f)
  protected val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  protected val cellMetrics = CellMetrics.fromFont(codeFont)
  protected val uiMetrics   = CellMetrics.fromFont(uiFont)

  protected val windowWidth  = 640
  protected val windowHeight = 600
  protected val viewport     = ViewportSize(windowWidth / cellMetrics.charWidth, windowHeight / cellMetrics.lineHeight)

  protected val baseConfig: AppConfig =
    AppConfig.default
      .withStatusLinePlacement(StatusLinePlacement.Pinned)
      .withLineNumbers(false)
      .withPaneHeaders(false)
      .withScrollbar(false)
      .withTabStrip(TabStripMode.Off)

  /** The renderer the frames are drawn with. A spec pins another only for a gap the suite records. */
  protected def referenceFrames: FrameBackend = SkiaFrames

  private def imagePoolSize(persists: Boolean): Int = if persists then 2 else 1

  private def deviceWidthPx(scale: Double): Int  = math.ceil(windowWidth * scale).toInt
  private def deviceHeightPx(scale: Double): Int = math.ceil(windowHeight * scale).toInt

  final protected class Pooled(scale: Double, persists: Boolean, frames: FrameImages):

    def this(scale: Double, persists: Boolean = true) =
      this(
        scale,
        persists,
        FrameImages(deviceWidthPx(scale), deviceHeightPx(scale), imagePoolSize(persists), referenceFrames)
      )

    private val images      = frames.drawn
    private val screenImage = frames.screen
    private val caches      = RenderCaches.create()
    caches.frameCounters.setEnabled(true)
    private val screen = ScreenIdentity(this)
    private val drawn  = AtomicInteger(0)

    def render(state: AppState, damage: Damage): BufferedImage =
      val image = images(drawn.getAndIncrement() % images.length)
      val surface = referenceFrames.surfaceOver(
        image,
        cellMetrics,
        codeFont,
        windowWidth,
        windowHeight,
        scale,
        persists,
        caches,
        Some(screen)
      )
      val repaintRegion = new AtomicReference[Option[List[PixelRect]]](None)
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, repaintRegion)),
        damage,
        caches = caches
      )
      present(image, repaintRegion.get())
      screenImage.toImage

    def rowsDrawn: Long = caches.frameCounters.lastFrame(FrameCounter.RowsDrawn)

    private def present(image: FramePixels, region: Option[List[PixelRect]]): Unit =
      region match
        case None => screenImage.blit(image, 0, 0, image.widthPx, image.heightPx)
        case Some(rects) =>
          rects.foreach { rect =>
            val x       = math.floor(rect.xPx * scale).toInt
            val y       = math.floor(rect.yPx * scale).toInt
            val w       = math.ceil(rect.widthPx * scale).toInt
            val h       = math.ceil(rect.heightPx * scale).toInt
            val clipped = new Rectangle(x, y, w, h).intersection(new Rectangle(0, 0, image.widthPx, image.heightPx))
            if !clipped.isEmpty then screenImage.blit(image, clipped.x, clipped.y, clipped.width, clipped.height)
          }

  protected val oracleImages: OracleImages = OracleImages(referenceFrames)

  /** Draws the cold frame of `state` into lent images, with caches made for this frame alone, and runs `use` on it
    * before the images go back; the frame is valid only inside `use`.
    */
  protected def coldFrameWith[A](state: AppState, scale: Double)(use: BufferedImage => A): A =
    oracleImages.lend(deviceWidthPx(scale), deviceHeightPx(scale), imagePoolSize(persists = false)) { frames =>
      use(Pooled(scale, persists = false, frames).render(state, Damage.Everything))
    }

  protected def differing(drawn: BufferedImage, state: AppState, scale: Double): Seq[(Int, Int)] =
    coldFrameWith(state, scale) { cold =>
      for
        x <- 0 until drawn.getWidth
        y <- 0 until drawn.getHeight
        if (drawn.getRGB(x, y) & 0xffffff) != (cold.getRGB(x, y) & 0xffffff)
      yield (x, y)
    }

  protected def summary(d: Seq[(Int, Int)]): String =
    val ys = d.map(_._2)
    val xs = d.map(_._1)
    s"${d.size} px differ, x ${xs.minOption.getOrElse(0)}-${xs.maxOption.getOrElse(0)}, y ${ys.minOption.getOrElse(0)}-${ys.maxOption.getOrElse(0)}"

  final protected class Session(document: RichTextDocument, scale: Double, config: AppConfig = baseConfig):
    private val (stateManager, id) =
      LaptopFrameBenchmarks.stateManagerHolding(document.plainText, config, grid = viewport)
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(buffers =
            state.persisted.buffers.updatedWith(id)(
              _.map(b => b.copy(richText = b.richText.withSyncedDocument(Some(document), b.document.contentVersion)))
            )
          )
        )
      )
      .unsafeRunSync()
    private val frames    = Pooled(scale)
    private val paneId    = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
    private val shown     = AtomicReference(stateManager.getCurrentState.unsafeRunSync())
    private val failures  = AtomicReference(Vector.empty[String])
    private val stepCount = AtomicInteger(0)
    private val onScreen  = AtomicReference(frames.render(shown.get, Damage.Everything))

    def placeCursor(line: Int, column: Int): Unit =
      val before = shown.get
      stateManager.setCursorPosition(paneId, line, column).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()
      shown.set(after)
      frames.render(after, DamageProducer.forTransition(before, after))

    /** The first edit changes the buffer's chrome as well as its rows; later keystrokes change rows alone. */
    def markModified(): Unit =
      placeCursor(2, 0)
      apply(InsertChar('w'), "first edit")

    def apply(event: Event, label: String): Unit =
      step(label)(stateManager.applyEvent(event).unsafeRunSync())

    /** Runs the command called `name`, as the command palette would. */
    def command(name: String, label: String): Unit =
      CommandRegistry.withToggleUI.findCommand(name) match
        case Some(found) => step(label)(stateManager.executeCommand(found).unsafeRunSync())
        case None        => failures.updateAndGet(_ :+ s"$label: no command named $name")

    /** Replaces the whole state the way a setting change would, then compares the frame it draws with a cold one. */
    def change(label: String)(update: AppState => AppState): Unit =
      step(label)(stateManager.updateState(update).unsafeRunSync())

    private def step(label: String)(act: => Unit): Unit =
      val before = shown.get
      act
      val after = stateManager.getCurrentState.unsafeRunSync()
      shown.set(after)
      val drawn = frames.render(after, DamageProducer.forTransition(before, after))
      onScreen.set(drawn)
      val diff = differing(drawn, after, scale)
      val n    = stepCount.incrementAndGet()
      if diff.nonEmpty then failures.updateAndGet(_ :+ s"step $n ($label): ${summary(diff)}")

    /** What a cold frame draws for the state as it now stands. */
    def coldFrame(): BufferedImage = Pooled(scale, persists = false).render(shown.get, Damage.Everything)

    def currentState: AppState = shown.get

    /** What the window shows after the latest step. */
    def screen: BufferedImage = onScreen.get

    def mismatches: List[String] = failures.get.take(4).toList

    def rowsDrawnByLastFrame: Long = frames.rowsDrawn
