package com.serenity.ui.skia

import java.awt.{Dimension, GraphicsEnvironment}
import java.util.Locale
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import java.util.concurrent.{CountDownLatch, FutureTask, TimeUnit}
import javax.swing.{JFrame, JPanel, SwingUtilities}

import com.serenity.app.SkiaVsync
import com.serenity.testkit.DisplayExclusive
import org.jetbrains.skia.{Font, Paint, Picture, PictureRecorder, Rect}

/** How long the layer's draw cycle takes as the scene it replays grows: a hosted [[SkiaLayerHost]] is handed a recorded
  * picture of N rows of text and timed from the start of its draw to the end of Skiko's draw cycle (see
  * `PresentPoint`), for several N. A cost that grows with the rows says the flush executes the scene's ops, so drawing
  * fewer of them would pay; a cost that is already large at one row is a fixed price per frame.
  *
  * It measures the host as it is: no editor scene, so it says nothing of what one frame of the editor costs, only how
  * the cost moves with the ops.
  */
object FlushSweep:

  enum Variant:
    /** The same picture every draw. */
    case Same

    /** Alternate draws differ in the last row, as a typed character does. */
    case OneRowChanged

  final case class Measurement(rows: Int, variant: Variant, millis: Vector[Double])

  final case class Summary(
      rows: Int,
      variant: Variant,
      draws: Int,
      minMs: Double,
      p50Ms: Double,
      p95Ms: Double,
      maxMs: Double
  )

  final case class Fit(interceptMs: Double, slopeMsPerRow: Double)

  def summarise(measurement: Measurement): Summary =
    val sorted = measurement.millis.sorted
    def rank(fraction: Double): Double =
      sorted.lift(math.max(1, math.ceil(fraction * sorted.size).toInt) - 1).getOrElse(0.0)
    Summary(
      measurement.rows,
      measurement.variant,
      sorted.size,
      sorted.headOption.getOrElse(0.0),
      rank(0.5),
      rank(0.95),
      sorted.lastOption.getOrElse(0.0)
    )

  /** A least-squares line through the medians of `variant`'s summaries, by rows. */
  def fit(summaries: List[Summary], variant: Variant = Variant.Same): Option[Fit] =
    val points = summaries.filter(_.variant == variant)
    val n      = points.size.toDouble
    val meanX  = points.map(_.rows.toDouble).sum / n
    val meanY  = points.map(_.p50Ms).sum / n
    val spread = points.map(p => math.pow(p.rows - meanX, 2)).sum
    Option.when(points.map(_.rows).distinct.size >= 2 && spread > 0.0) {
      val slope = points.map(p => (p.rows - meanX) * (p.p50Ms - meanY)).sum / spread
      Fit(meanY - slope * meanX, slope)
    }

  private def formatted(pattern: String, values: Any*): String =
    String.format(Locale.ROOT, pattern, values.map(_.asInstanceOf[AnyRef])*)

  def report(renderApi: String, summaries: List[Summary]): List[String] =
    val header  = s"Skia flush sweep: renderApi=$renderApi (draw start to the end of Skiko's draw cycle, milliseconds)"
    val columns = formatted("%5s  %-13s %6s %9s %9s %9s %9s", "rows", "variant", "draws", "min", "p50", "p95", "max")
    val body = summaries.sortBy(s => (s.variant.ordinal, s.rows)).map { s =>
      formatted(
        "%5d  %-13s %6d %9.2f %9.2f %9.2f %9.2f",
        s.rows,
        s.variant,
        s.draws,
        s.minMs,
        s.p50Ms,
        s.p95Ms,
        s.maxMs
      )
    }
    val fits = Variant.values.toList.flatMap(variant =>
      fit(summaries, variant).map(found =>
        formatted(
          "fit %s: intercept %.2f ms + %.2f ms per row (line through the p50s)",
          variant,
          found.interceptMs,
          found.slopeMsPerRow
        )
      )
    )
    header :: columns :: body ::: fits

  /** `on` or `off`, from the command line; off when absent, as the app runs. */
  def vsyncRequested(argument: Option[String]): Either[String, Boolean] =
    argument.map(_.trim.toLowerCase) match
      case None | Some("off") => Right(false)
      case Some("on")         => Right(true)
      case Some(other)        => Left(s"vsync must be on or off, not '$other'")

  /** What the app sets before its first layer is made (`GraphicsStartup`): with vsync on, a draw waits for the display
    * and the sweep measures the wait. A value the user already set stays.
    */
  def vsyncProperty(
    enabled: Boolean,
    env: Map[String, String],
    properties: Map[String, String]
  ): Option[(String, String)] =
    SkiaVsync.property(enabled, env, properties)

  private val LineHeightPx = 16f

  private val Lorem = "The quick brown fox jumps over the lazy dog 0123456789 pack my box with five dozen liquor jugs. "

  private def rowText(row: Int, changed: Boolean): String =
    val text = (Lorem * 2).drop((row * 11) % Lorem.length).take(96)
    if changed then "*" + text.drop(1) else text

  private def font: Font =
    SkiaTypefaces.bundledTypefaces
      .find(face => face.getFamilyName == "Monaspace Neon" && !face.isBold)
      .fold(new Font())(new Font(_, 14f))

  /** `rows` rows of a background fill and a line of text, as a picture; draw `drawIndex` of a variant that changes a
    * row differs from the one before it.
    */
  def scene(rows: Int, variant: Variant, drawIndex: Int, widthPx: Int, heightPx: Int): Picture =
    val recorder   = new PictureRecorder()
    val canvas     = PictureRecording.begin(recorder, widthPx, heightPx)
    val background = new Paint()
    val ink        = new Paint()
    val face       = font
    try
      canvas.clear(0xff202830)
      ink.setColor(0xffe0e0e0)
      (0 until rows).foreach { row =>
        background.setColor(if row % 2 == 0 then 0xff263040 else 0xff202830)
        canvas.drawRect(Rect.Companion.makeXYWH(0f, row * LineHeightPx, widthPx.toFloat, LineHeightPx), background)
        val changed = variant == Variant.OneRowChanged && drawIndex % 2 == 1 && row == rows - 1
        canvas.drawString(rowText(row, changed), 4f, (row + 1) * LineHeightPx - 4f, face, ink)
      }
      recorder.finishRecordingAsPicture()
    finally
      face.close()
      ink.close()
      background.close()
      recorder.close()

  /** One draw's time, if the cycle it started also ended: a cycle already under way when the picture was published ends
    * without a start of ours, and is left out.
    */
  final private class Timer(host: SkiaLayerHost):
    final private case class Pending(startedAt: AtomicLong, elapsedNanos: AtomicLong, done: CountDownLatch)

    private val pending = new AtomicReference(Option.empty[Pending])

    host.onDrawStarted(() => pending.get().foreach(_.startedAt.compareAndSet(0L, System.nanoTime())))
    host.onDrawEnded { () =>
      pending.get().foreach { draw =>
        val started = draw.startedAt.get()
        if started != 0L && draw.elapsedNanos.compareAndSet(0L, System.nanoTime() - started) then draw.done.countDown()
      }
    }

    def timedMs(picture: Picture): Either[String, Double] =
      val draw = Pending(new AtomicLong(0L), new AtomicLong(0L), new CountDownLatch(1))
      pending.set(Some(draw))
      host.publish(picture)
      Either.cond(
        draw.done.await(15, TimeUnit.SECONDS),
        draw.elapsedNanos.get() / 1e6,
        "no draw cycle ended within 15 s"
      )

  private val Warmups = 5

  private def onEventThread[A](body: => A): A =
    val task = new FutureTask[A](() => body)
    SwingUtilities.invokeAndWait(task)
    task.get()

  private def measure(
    timer: Timer,
    rows: Int,
    variant: Variant,
    draws: Int,
    size: Dimension
  ): Either[String, Measurement] =
    def timed(index: Int): Either[String, Double] =
      timer.timedMs(scene(rows, variant, index, size.width, size.height))
    (0 until Warmups)
      .foldLeft[Either[String, Unit]](Right(()))((outcome, index) => outcome.flatMap(_ => timed(index).map(_ => ())))
      .flatMap(_ =>
        (0 until draws).foldLeft[Either[String, Vector[Double]]](Right(Vector.empty))((outcome, index) =>
          outcome.flatMap(done => timed(Warmups + index).map(done :+ _))
        )
      )
      .map(Measurement(rows, variant, _))

  /** The render API the layer ended up with, and a summary for every row count and variant. */
  def run(
    rowCounts: List[Int],
    draws: Int,
    variants: List[Variant],
    size: Dimension = new Dimension(1000, 800)
  ): Either[String, (String, List[Summary])] =
    if GraphicsEnvironment.isHeadless then Left("a hosted layer needs a display")
    else
      DisplayExclusive:
        val canvas = new JPanel(null)
        canvas.setFocusable(true)
        canvas.setPreferredSize(size)
        val frame = onEventThread {
          val window = new JFrame("flush sweep")
          window.setContentPane(canvas)
          window.pack()
          window.setLocation(0, 0)
          window.setVisible(true)
          window
        }
        val host = SkiaLayerHost.createWith(canvas, syncGpu = false, LayerRedraw.forLayer, _ => ())
        try
          val timer = new Timer(host)
          val measurements = for
            variant <- variants
            rows    <- rowCounts
          yield measure(timer, rows, variant, draws, size)
          measurements
            .foldLeft[Either[String, List[Summary]]](Right(Nil))((outcome, next) =>
              outcome.flatMap(done => next.map(measured => done :+ summarise(measured)))
            )
            .map(summaries => (host.renderApi, summaries))
        finally
          host.dispose()
          onEventThread(frame.dispose())
