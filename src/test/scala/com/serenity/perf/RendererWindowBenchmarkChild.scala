package com.serenity.perf

import java.awt.{Dimension, Rectangle, Robot, Toolkit}
import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicReference
import javax.swing.SwingUtilities

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.serenity.app.{
  GraphicsStartup,
  GuiFramePainting,
  LaunchOptions,
  RenderBackend,
  RuntimeDisplayState,
  ToolkitSelection
}
import com.serenity.config.{AppConfig, PreferredWindowSize, RenderBackendPeek, WaylandRender, WindowChromeMode}
import com.serenity.keystroke.events.{InsertChar, ScrollDown}
import com.serenity.perf.RendererBenchmarkReport.{Sample, environmentLine, sampleLine}
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.HostedOutcome
import com.serenity.ui.skia.SkiaProbe
import com.serenity.ui.terminal.SwingWindow
import org.typelevel.log4cats.noop.NoOpLogger

/** One renderer, measured in a real window. Run by [[RendererWindowBenchmark]], one JVM per renderer, because the
  * backend is chosen once at startup from the same flags the editor takes.
  *
  * It resolves the renderer through `GraphicsStartup.current`, opens a [[SwingWindow]] exactly as `Main` does for that
  * decision, and paints typing, scrolling and new-letter frames through `GuiFramePainting.paintFullFrame`, the call the
  * frame loop makes. A frame ends when it is on screen: the Java2D path waits for the event-dispatch thread to paint it
  * and for the display connection to catch up, and the hosted Skia canvas returns once the layer has rendered.
  *
  * Output is tagged lines on stdout (see [[RendererBenchmarkReport]]); the parent pools them.
  */
private[perf] object RendererWindowBenchmarkChild:

  given Balance = Balance.default

  final private case class Settings(
      frames: Int,
      warmupFrames: Int,
      logicalSize: Dimension,
      simulateWaylandRaster: Boolean,
      launchFlags: List[String]
  )

  final private case class Scenario(name: String, prime: () => Unit, frame: () => Unit)

  private val DefaultFrames = 200
  private val DefaultWarmup = 60
  private val DefaultSize   = new Dimension(1200, 800)
  private val SizePattern   = raw"(\d+)x(\d+)".r
  private val ScrollLines   = 3
  private val ShowTimeout   = 20.seconds
  private val PollInterval  = 50.milliseconds
  private val OwnOptions    = List("--frames=", "--warmup=", "--logical-size=", "--simulate-wayland-raster")

  private def settingsOf(args: List[String]): Either[String, Settings] =
    val (own, launchFlags) = args.partition(arg => OwnOptions.exists(arg.startsWith))
    def valueOf(prefix: String): Option[String] =
      own.collectFirst { case arg if arg.startsWith(prefix) => arg.drop(prefix.length) }
    def count(prefix: String, default: Int, atLeast: Int): Either[String, Int] =
      valueOf(prefix).fold(Right(default))(value =>
        value.toIntOption.filter(_ >= atLeast).toRight(s"$prefix$value is not a count of at least $atLeast")
      )
    for
      size <- valueOf("--logical-size=") match
        case None                             => Right(DefaultSize)
        case Some(SizePattern(width, height)) => Right(new Dimension(width.toInt, height.toInt))
        case Some(other)                      => Left(s"--logical-size must look like 1200x800, not '$other'")
      frames <- count("--frames=", DefaultFrames, 1)
      warmup <- count("--warmup=", DefaultWarmup, 0)
    yield Settings(frames, warmup, size, own.contains("--simulate-wayland-raster"), launchFlags)

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    settingsOf(args.toList) match
      case Left(problem) =>
        System.err.println(problem)
        System.exit(2)
      case Right(settings) =>
        val outcome = run(settings).attempt.unsafeRunSync()
        outcome.left.foreach(error => System.err.println(s"benchmark child failed: $error"))
        System.exit(if outcome.isRight then 0 else 1)

  /** The editor's own startup decision for these flags. The simulated form answers as a native Wayland session would,
    * without applying its toolkit, so the raster backend can be measured where no Wayland session exists: its frames
    * are still presented by the Java2D canvas of whatever toolkit is running.
    */
  private def resolve(settings: Settings): IO[GraphicsStartup.Resolved] =
    LaunchOptions.parse(settings.launchFlags :+ "--gui") match
      case Left(help) => IO.raiseError(new IllegalArgumentException(help.toString))
      case Right(options) =>
        val launch = GraphicsStartup.launchOf(options)
        if settings.simulateWaylandRaster then
          ToolkitSelection.currentEnvironment.flatMap { real =>
            val wayland = real.copy(
              osName = "Linux",
              properties = real.properties.updated(ToolkitSelection.ToolkitProperty, "WLToolkit"),
              env = real.env
                .updated("WAYLAND_DISPLAY", "benchmark")
                .updated("DISPLAY", real.env.getOrElse("DISPLAY", ":0")),
              waylandToolkitAvailable = true
            )
            GraphicsStartup.resolve(
              launch.copy(waylandRender = Some(WaylandRender.SkiaRaster)),
              wayland,
              GraphicsStartup.catalogue,
              RenderBackendPeek.read(),
              IO.defer(SkiaProbe.run)
            )
          }
        else GraphicsStartup.current(launch)

  private def run(settings: Settings)(using IORuntime): IO[Unit] =
    val fontConfig = AppConfig.default.editorConfig.fontConfig
      .resolveAutoTextScale(DisplayScale.defaultDeviceScale.textScale)
    for
      resolved     <- resolve(settings)
      displayState <- RuntimeDisplayState.create(fontConfig)(using NoOpLogger[IO])
      display = displayState.snapshot
      _ <- SwingWindow
        .resource(
          display.codeMetrics,
          display.uiMetrics,
          WindowChromeMode.Auto,
          Some(PreferredWindowSize(settings.logicalSize.width, settings.logicalSize.height)),
          title = "Serenity renderer benchmark",
          hosted = resolved.surfaces.hostedCanvas
        )
        .use(window => measure(settings, resolved, display, window))
    yield ()

  private def measure(
    settings: Settings,
    resolved: GraphicsStartup.Resolved,
    display: RuntimeDisplayState.Snapshot,
    window: SwingWindow
  )(using IORuntime): IO[Unit] =
    awaitShown(window) >> IO.blocking {
      val caches = RenderCaches.create()
      def paint(state: AppState, damage: Damage): Unit =
        GuiFramePainting.paintFullFrame(
          state,
          cursorVisible = true,
          None,
          window,
          display,
          damage,
          caches,
          resolved.surfaces
        ) match
          case HostedOutcome.Skipped(reason) => sys.error(s"a frame was not drawn: $reason")
          case outcome                       => GuiFramePainting.settle(outcome).unsafeRunSync()
        settleOnScreen()
      val measured = scenarios(window.viewportSize, paint).flatMap(timed(settings, _))
      println(environmentLine(environmentOf(settings, resolved, window)))
      measured.foreach(sample => println(sampleLine(sample)))
    }

  /** The window has been asked to show; its canvas has a size once the event-dispatch thread has laid it out. */
  private def awaitShown(window: SwingWindow): IO[Unit] =
    def ready: Boolean = window.inputComponent.isShowing && window.inputComponent.getWidth > 0 && window.canvas.getWidth > 0 && window.canvas.getHeight > 0
    def poll(waited: FiniteDuration): IO[Unit] =
      if ready then IO.unit
      else if waited >= ShowTimeout then IO.raiseError(
          new IllegalStateException(
            s"the window never became visible: ${window.inputComponent.getClass.getSimpleName} " +
              s"${window.inputComponent.getWidth}x${window.inputComponent.getHeight} " +
              s"showing=${window.inputComponent.isShowing}, canvas ${window.canvas.getWidth}x${window.canvas.getHeight} " +
              s"children=${window.canvas.getComponentCount} layout=${window.canvas.getLayout} parent=${window.inputComponent.getParent == window.canvas} bounds=${window.inputComponent.getBounds}"
          )
        )
      else IO.sleep(PollInterval) >> poll(waited + PollInterval)
    poll(Duration.Zero)

  /** A frame is not over until Swing has painted it, so the event-dispatch thread is flushed twice (the second run
    * queues behind any repaint the first one posts) and the display connection is synchronised.
    */
  private def settleOnScreen(): Unit =
    SwingUtilities.invokeAndWait(() => ())
    SwingUtilities.invokeAndWait(() => ())
    Toolkit.getDefaultToolkit.sync()

  /** The three frames a writer produces, over one prose document: a keystroke in a modified buffer and its undoing, a
    * three-line scroll and its reverse, and a fresh letter each frame. Each is told what the transition into it
    * damaged, as the frame loop tells the renderer.
    */
  private def scenarios(grid: ViewportSize, paint: (AppState, Damage) => Unit)(using IORuntime): List[Scenario] =
    val (stateManager, _)           = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    def current: AppState           = stateManager.getCurrentState.unsafeRunSync()
    val (beforeTyping, afterTyping) = TypingTransition.of(stateManager)
    stateManager.applyEvent(ScrollDown(ScrollLines)).unsafeRunSync()
    val afterScroll = current

    def alternating(name: String, before: AppState, after: AppState): Scenario =
      val forward  = DamageProducer.forTransition(before, after)
      val backward = DamageProducer.forTransition(after, before)
      val steps    = Iterator.continually(List(after -> forward, before -> backward)).flatten
      Scenario(
        name,
        () => paint(before, Damage.Everything),
        () =>
          val (state, damage) = steps.next()
          paint(state, damage)
      )

    val letters = BenchmarkFixtures.randomLetters(seed = 42L)
    val shown   = new AtomicReference(current)
    val newLetter = Scenario(
      "new letter",
      () => paint(shown.get, Damage.Everything),
      () =>
        stateManager.applyEvent(InsertChar(letters.next())).unsafeRunSync()
        val next     = current
        val previous = shown.getAndSet(next)
        paint(next, DamageProducer.forTransition(previous, next))
    )
    List(
      alternating("typing", beforeTyping, afterTyping),
      alternating("scrolling", afterTyping, afterScroll),
      newLetter
    )

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  /** Bytes every live Java thread has allocated so far: the editor's work is spread over the frame loop, the
    * event-dispatch thread and Skiko's own, so one thread's count would miss the other renderers' share. Native memory
    * (Skia's) is not counted.
    */
  private def allocatedByAllThreads(): Long =
    allocationBean.fold(0L)(bean => bean.getThreadAllocatedBytes(bean.getAllThreadIds).filter(_ >= 0).sum)

  private def timed(settings: Settings, scenario: Scenario): List[Sample] =
    scenario.prime()
    (1 to settings.warmupFrames).foreach(_ => scenario.frame())
    val reading = (1 to 20).map { _ =>
      val first = allocatedByAllThreads()
      allocatedByAllThreads() - first
    }.min
    (1 to settings.frames).toList.map { _ =>
      val allocatedBefore = allocatedByAllThreads()
      val started         = System.nanoTime()
      scenario.frame()
      val nanos     = System.nanoTime() - started
      val allocated = allocatedByAllThreads() - allocatedBefore - reading
      Sample(scenario.name, nanos, allocated.max(0L))
    }

  private def environmentOf(
    settings: Settings,
    resolved: GraphicsStartup.Resolved,
    window: SwingWindow
  ): Map[String, String] =
    val scale = DisplayScale.forComponent(window.canvas)
    val api = resolved.backend.backend match
      case RenderBackend.Skia       => resolved.surfaces.hostedCanvas.fold("unknown")(_.renderApi)
      case RenderBackend.SkiaRaster => "CPU raster"
      case RenderBackend.Java2D     => "none (Java2D)"
    Map(
      "requested" -> (settings.launchFlags ++ Option.when(settings.simulateWaylandRaster)("(simulated Wayland)"))
        .mkString(" "),
      "backend"    -> resolved.backend.backend.configKey,
      "reason"     -> resolved.backend.reason,
      "api"        -> api,
      "scale"      -> scale.x.toString,
      "canvas"     -> s"${window.canvas.getWidth}x${window.canvas.getHeight} logical",
      "toolkit"    -> Toolkit.getDefaultToolkit.getClass.getSimpleName,
      "os"         -> s"${sys.props.getOrElse("os.name", "?")} ${sys.props.getOrElse("os.arch", "?")}",
      "java"       -> s"${sys.props.getOrElse("java.runtime.version", "?")} ${sys.props.getOrElse("java.vendor", "?")}",
      "jvm"        -> ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.mkString(" "),
      "allocation" -> (if allocationBean.isDefined then "all Java threads" else "unavailable"),
      "colours"    -> screenColours(window)
    )

  /** How many distinct colours a sparse grid of the window's on-screen pixels shows: a blank window shows one. It is
    * best effort: a desktop may refuse screen capture, which is reported rather than failed on.
    */
  private def screenColours(window: SwingWindow): String =
    Try {
      val component = window.inputComponent
      val origin    = component.getLocationOnScreen
      val capture =
        new Robot().createScreenCapture(new Rectangle(origin.x, origin.y, component.getWidth, component.getHeight))
      val colours = for
        x <- 0 until capture.getWidth by 7
        y <- 0 until capture.getHeight by 5
      yield capture.getRGB(x, y)
      colours.distinct.length.toString
    }.getOrElse("unchecked")
