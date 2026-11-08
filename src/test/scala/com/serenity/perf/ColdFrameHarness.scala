package com.serenity.perf

import java.awt.{Font, Rectangle}
import java.awt.image.BufferedImage
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
import com.serenity.testkit.{FrameDiff, FrameMismatchDump, FramePlanReport, PresentAreas}
import com.serenity.ui.layout.{BundledInterFont, CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, FramePlan, RendererFramePlanner, ScreenIdentity}
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

  final protected class Pooled(scale: Double, persists: Boolean, frames: FrameImages, caches: RenderCaches):

    def this(scale: Double, persists: Boolean = true, caches: RenderCaches = RenderCaches.create()) =
      this(
        scale,
        persists,
        FrameImages(deviceWidthPx(scale), deviceHeightPx(scale), imagePoolSize(persists), referenceFrames),
        caches
      )

    private val images      = frames.drawn
    private val screenImage = frames.screen
    caches.frameCounters.setEnabled(true)
    private val screen     = ScreenIdentity(this)
    private val drawn      = AtomicInteger(0)
    private val lastOutput = AtomicReference(FrameOutput(screen, new AtomicReference(None)))
    private val slotPixels = AtomicReference(Vector.fill(images.length)(Array.empty[Int]))
    private val lastSlot   = AtomicInteger(0)
    private val lastShown  = AtomicReference[Option[List[Rectangle]]](None)
    private val lastPaint  = AtomicReference(List.empty[Rectangle])

    /** What each image of the pool held after its latest frame, the latest frame's image first. */
    def imagesNewestFirst: Vector[Array[Int]] =
      val held = slotPixels.get
      val last = lastSlot.get
      held.indices.map(i => held((last - i + held.length) % held.length)).toVector

    /** The rects the latest frame presented, `None` meaning the whole frame. */
    def presentedAreas: Option[List[Rectangle]] = lastShown.get

    /** The rects the latest frame's recorded paint covers. */
    def paintedAreas: List[Rectangle] = lastPaint.get

    /** What the planner drew and kept for the latest frame. */
    def lastPlan: Option[FramePlan] = lastOutput.get.lastPlan.get

    def render(state: AppState, damage: Damage): BufferedImage = renderFrame(state, damage).toImage

    /** Draws the frame and returns the pixels the window shows, which later frames draw over. */
    def renderFrame(state: AppState, damage: Damage): FramePixels =
      val slot  = drawn.getAndIncrement() % images.length
      val image = images(slot)
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
      val output        = FrameOutput(screen, repaintRegion)
      lastOutput.set(output)
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
        Some(output),
        damage,
        caches = caches
      )
      lastSlot.set(slot)
      slotPixels.updateAndGet(_.updated(slot, image.rgbPixels.clone()))
      lastPaint.set(image.recordedPaint)
      lastShown.set(PresentAreas.of(repaintRegion.get(), image.recordedPaint, scale, image.widthPx, image.heightPx))
      present(image, repaintRegion.get())
      screenImage

    def rowsDrawn: Long = caches.frameCounters.lastFrame(FrameCounter.RowsDrawn)

    private def present(image: FramePixels, region: Option[List[PixelRect]]): Unit =
      PresentAreas.of(region, image.recordedPaint, scale, image.widthPx, image.heightPx) match
        case None => screenImage.blit(image, 0, 0, image.widthPx, image.heightPx)
        case Some(areas) =>
          areas.foreach(area => screenImage.blit(image, area.x, area.y, area.width, area.height))

  protected val oracleImages: OracleImages = OracleImages(referenceFrames)

  /** How many failing steps of one session get the full report and its PNGs; the rest get the short line. */
  private val FullReports = 3

  /** Draws the cold frame of `state` into lent images, with caches made for this frame alone, and runs `use` on it
    * before the images go back; the frame is valid only inside `use`.
    */
  protected def coldFrameWith[A](state: AppState, scale: Double)(use: BufferedImage => A): A =
    coldFramePixelsWith(state, scale)(pixels =>
      use(FramePixels.image(deviceWidthPx(scale), deviceHeightPx(scale), pixels))
    )

  /** The same, read as pixels, which builds no image. */
  protected def coldFramePixelsWith[A](state: AppState, scale: Double)(use: Array[Int] => A): A =
    oracleImages.lend(deviceWidthPx(scale), deviceHeightPx(scale), imagePoolSize(persists = false)) { frames =>
      use(
        Pooled(scale, persists = false, frames, RenderCaches.create()).renderFrame(state, Damage.Everything).rgbPixels
      )
    }

  protected def differing(drawn: BufferedImage, state: AppState, scale: Double): Seq[(Int, Int)] =
    coldFrameWith(state, scale)(cold => FrameDiff.differing(drawn, cold))

  protected def differingPixels(drawn: Array[Int], state: AppState, scale: Double): Seq[(Int, Int)] =
    coldFramePixelsWith(state, scale)(cold =>
      FrameDiff.differing(drawn, cold, deviceWidthPx(scale), deviceHeightPx(scale))
    )

  protected def summary(d: Seq[(Int, Int)]): String =
    val ys = d.map(_._2)
    val xs = d.map(_._1)
    s"${d.size} px differ, x ${xs.minOption.getOrElse(0)}-${xs.maxOption.getOrElse(0)}, y ${ys.minOption.getOrElse(0)}-${ys.maxOption.getOrElse(0)}"

  protected def failureLine(step: Int, label: String, diff: Seq[(Int, Int)], plan: Option[FramePlan]): String =
    s"step $step ($label): ${summary(diff)}; ${FramePlanReport.describe(plan)}"

  /** [[failureLine]] plus what a log needs to say why: the colours of the first differing pixels in each frame, which
    * of them lie outside every rect the window presented, and the frames as PNGs beside the JUnit reports.
    */
  protected def failureReport(
    step: Int,
    label: String,
    drawn: BufferedImage,
    cold: BufferedImage,
    diff: Seq[(Int, Int)],
    plan: Option[FramePlan],
    scale: Double
  ): String =
    val coverage = FramePlanReport.coverage(plan, diff, scale, drawn.getWidth, drawn.getHeight)
    s"${failureLine(step, label, diff, plan)}\n${FrameMismatchDump.write(s"step $step $label", drawn, cold, diff, Some(coverage))}"

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
    private val reported  = AtomicInteger(0)
    private val onScreen  = AtomicReference(frames.renderFrame(shown.get, Damage.Everything).rgbPixels)
    private val built     = AtomicInteger(0)
    private val seenPlan  = AtomicReference[Option[FramePlan]](None)
    private val priorCold = AtomicReference(Array.empty[Int])

    def placeCursor(line: Int, column: Int): Unit =
      val before = shown.get
      stateManager.setCursorPosition(paneId, line, column).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()
      shown.set(after)
      val _ = frames.renderFrame(after, DamageProducer.forTransition(before, after))

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
      val drawn        = frames.renderFrame(after, DamageProducer.forTransition(before, after)).rgbPixels.clone()
      val previousPlan = seenPlan.getAndSet(frames.lastPlan)
      val previousScreen = onScreen.getAndSet(drawn)
      val previousCold   = priorCold.get
      val n = stepCount.incrementAndGet()
      coldFramePixelsWith(after, scale) { cold =>
        priorCold.set(cold.clone())
        val diff0 = FrameDiff.differing(drawn, cold, deviceWidthPx(scale), deviceHeightPx(scale))
        val diff  = if diff0.isEmpty && sys.props.contains("probe.force") then Seq((20, 20)) else diff0
        if diff.nonEmpty || sys.props.contains("probe.force") then
          val full = reported.incrementAndGet() <= FullReports
          val report =
            if full then
              val (width, height) = (deviceWidthPx(scale), deviceHeightPx(scale))
              val drawnImage      = FramePixels.image(width, height, drawn)
              val coldImage       = FramePixels.image(width, height, cold)
              failureReport(n, label, drawnImage, coldImage, diff, frames.lastPlan, scale)
            else failureLine(n, label, diff, frames.lastPlan)
          val probe = scala.util
            .Try(probeReport(n, label, after, drawn, cold, diff, previousPlan) + lastWriter(diff, drawn, cold, previousScreen, previousCold))
            .fold(e => s"PROBE step $n failed: $e", identity)
          println(probe)
          failures.updateAndGet(_ :+ s"$probe\n$report")
      }

    /** Why the frame differs: whether a first persisting frame of this state already differs from the cold one (a
      * painting difference), whether the incremental frame differs from that persisting frame (an invalidation one),
      * the plan of the step before and the bands that own the first differing pixels.
      */
    private def probeReport(
      n: Int,
      label: String,
      state: AppState,
      drawn: Array[Int],
      cold: Array[Int],
      diff: Seq[(Int, Int)],
      previousPlan: Option[FramePlan]
    ): String =
      val (width, height) = (deviceWidthPx(scale), deviceHeightPx(scale))
      val fresh           = Pooled(scale).renderFrame(state, Damage.Everything).rgbPixels
      val freshVsCold     = FrameDiff.differing(fresh, cold, width, height)
      val drawnVsFresh    = FrameDiff.differing(drawn, fresh, width, height)
      s"PROBE step $n ($label) at ${scale}x: persisting first frame vs cold: ${summary(freshVsCold)}; " +
        s"incremental vs persisting first frame: ${summary(drawnVsFresh)}; " +
        s"owners ${FramePlanReport.owners(frames.lastPlan, previousPlan, diff, scale)}; " +
        s"previous ${FramePlanReport.describe(previousPlan)}"

    /** For the first differing pixels, who last wrote them: whether the screen was already wrong there after the step
      * before, what this frame's image and the other image of the pool held, and whether the frame painted and
      * presented that pixel.
      */
    private def lastWriter(
      diff: Seq[(Int, Int)],
      drawn: Array[Int],
      cold: Array[Int],
      previousScreen: Array[Int],
      previousCold: Array[Int]
    ): String =
      val width  = deviceWidthPx(scale)
      val held   = frames.imagesNewestFirst
      def hex(a: Array[Int], i: Int): String = if i < a.length then f"${a(i) & 0xffffff}%06x" else "none"
      def within(rects: List[Rectangle], x: Int, y: Int) = rects.exists(r => r.contains(x, y))
      val lines = diff.take(3).map { (x, y) =>
        val i       = y * width + x
        val wrongBefore = previousScreen.length > i && previousCold.length > i && previousScreen(i) != previousCold(i)
        val presented = frames.presentedAreas.fold("whole frame")(rs => within(rs, x, y).toString)
        s"($x,$y): cold ${hex(cold, i)} screen ${hex(drawn, i)} image-now ${hex(held.head, i)} " +
          s"image-other ${held.drop(1).headOption.fold("none")(hex(_, i))} screen-before ${hex(previousScreen, i)} " +
          s"cold-before ${hex(previousCold, i)} wrong-before $wrongBefore presented $presented " +
          s"painted ${within(frames.paintedAreas, x, y)}"
      }
      s" LASTWRITER ${lines.mkString(" | ")}"

    /** What a cold frame draws for the state as it now stands. */
    def coldFrame(): BufferedImage =
      built.incrementAndGet()
      Pooled(scale, persists = false).render(shown.get, Damage.Everything)

    def currentState: AppState = shown.get

    /** What the planner drew and kept for the latest frame. */
    def planOfLastFrame: Option[FramePlan] = frames.lastPlan

    /** What the window shows after the latest step. */
    def screen: BufferedImage =
      built.incrementAndGet()
      FramePixels.image(deviceWidthPx(scale), deviceHeightPx(scale), onScreen.get)

    /** How many images this session has built; a step builds none. */
    def imagesBuilt: Int = built.get

    def mismatches: List[String] = failures.get.take(4).toList

    def rowsDrawnByLastFrame: Long = frames.rowsDrawn
