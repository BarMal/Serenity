package com.serenity.spike

import java.awt.Dimension
import java.awt.event.{InputMethodEvent, InputMethodListener, KeyAdapter, KeyEvent, WindowAdapter, WindowEvent}
import java.text.CharacterIterator
import java.util.concurrent.{ConcurrentHashMap, Executors, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import javax.swing.{JFrame, SwingUtilities, WindowConstants}

import org.jetbrains.skia.{Canvas, Image, Picture, PictureRecorder, Rect, Surface}
import org.jetbrains.skiko.{SkiaLayer, SkiaLayerProperties, SkikoRenderDelegate}

/** One frame: which request it drew, `onRender`'s own time, and the whole EDT frame task's time.
  *
  * `SkiaLayer.update` records `onRender` into an `SkPicture` (bytecode of Skiko 0.150.1: `pictureRecorder`
  * `.beginRecording` before the delegate call); the redrawer rasterises that picture afterwards. So `onRender` time is
  * recording only. `frameMs`/`atNanos` come from an `invokeLater` posted inside `onRender`, which runs once the
  * current EDT task finishes, and so includes Skiko's raster and present when they run in that same task.
  */
final case class Painted(request: Long, atNanos: Long, edtMs: Double, frameMs: Double)

/** A `JFrame` holding a `SkiaLayer`, and the three ways of getting a frame into it (see [[Pipeline]]). */
final class SkikoHost(pipeline: Pipeline, initial: EditorState, shaping: Shaping, subpixelText: Boolean):
  val painted: LinkedBlockingQueue[Painted] = LinkedBlockingQueue()
  val renderCalls: AtomicLong               = AtomicLong()
  val direct: AtomicReference[(EditorState, Long)] = AtomicReference((initial, 0L))

  private var edtRenderer: SkiaSceneRenderer = null
  private var picture: Picture               = null
  private var image: Image                   = null
  private var published: Long                = 0L

  @volatile var contentScale: Float = 0f

  private val delegate = new SkikoRenderDelegate:
    def onRender(canvas: Canvas, width: Int, height: Int, nanoTime: Long): Unit =
      renderCalls.incrementAndGet()
      val started = System.nanoTime
      val request = pipeline match
        case Pipeline.Direct =>
          val (state, request) = direct.get
          if edtRenderer == null then edtRenderer = SkiaSceneRenderer(layer.getContentScale, shaping, subpixelText)
          edtRenderer.draw(canvas, state)
          request
        case Pipeline.Picture =>
          SkikoHost.this.synchronized {
            if picture != null then
              val _ = picture.playback(canvas, null)
            published
          }
        case Pipeline.Raster =>
          SkikoHost.this.synchronized {
            if image != null then canvas.drawImage(image, 0f, 0f)
            published
          }
      val recorded = (System.nanoTime - started) / 1e6
      SwingUtilities.invokeLater { () =>
        val done = System.nanoTime
        painted.put(Painted(request, done, recorded, (done - started) / 1e6))
      }

  // The 4-argument constructor with Kotlin's defaults for the accessibility factory, analytics and pixel geometry:
  // mask bits 0, 2 and 3 select the defaults, the properties (which read skiko.renderApi) are passed explicitly.
  val layer: SkiaLayer = SkiaLayer(null, SkiaLayerProperties(), null, null, 13, null)
  layer.setRenderDelegate(delegate)
  layer.setPreferredSize(Dimension(Geometry.LogicalWidth, Geometry.LogicalHeight))

  val frame: JFrame = JFrame("Serenity Skiko spike")
  frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  frame.getContentPane.add(layer)
  frame.pack()
  frame.setVisible(true)
  layer.setFocusable(true)
  layer.requestFocus()

  def requestRedraw(): Unit = SwingUtilities.invokeLater(() => layer.needRedraw())

  def publishPicture(next: Picture, request: Long): Unit =
    val old = synchronized {
      val previous = picture
      picture = next
      published = request
      previous
    }
    if old != null then old.close()

  def publishImage(next: Image, request: Long): Unit =
    val old = synchronized {
      val previous = image
      image = next
      published = request
      previous
    }
    if old != null then old.close()

  /** The first paint of `request` or later, or `None` after `timeoutMs`. */
  def awaitPainted(request: Long, timeoutMs: Long): Option[Painted] =
    val deadline = System.nanoTime + timeoutMs * 1_000_000L
    @annotation.tailrec
    def loop(): Option[Painted] =
      val remaining = deadline - System.nanoTime
      if remaining <= 0 then None
      else
        Option(painted.poll(remaining, TimeUnit.NANOSECONDS)) match
          case Some(p) if p.request >= request => Some(p)
          case Some(_)                         => loop()
          case None                            => None
    loop()

  def onEdt[A](body: => A): A =
    val result = AtomicReference[A]()
    SwingUtilities.invokeAndWait(() => result.set(body))
    result.get

/** Produces frames for the pipeline off the EDT (picture and raster); direct mode only hands the state over. */
final class FrameProducer(pipeline: Pipeline, host: SkikoHost, scale: Float, shaping: Shaping, subpixelText: Boolean):
  private val renderer = SkiaSceneRenderer(scale, shaping, subpixelText)
  private val width    = math.round(Geometry.LogicalWidth * scale)
  private val height   = math.round(Geometry.LogicalHeight * scale)
  // Two surfaces so the one being drawn never has a live snapshot (no copy-on-write); each frame redraws its own
  // damage plus the previous frame's, which the other surface already holds.
  private val surfaces      = Vector.fill(2)(Surface.Companion.makeRasterN32Premul(width, height))
  private var next          = 0
  private var lastDamage    = renderer.fullViewport
  private var primedSurfaces = 0

  /** Worker-thread milliseconds spent producing `state` for `request`, with `damage` in logical pixels. */
  def produce(state: EditorState, request: Long, damage: Rect): Double =
    pipeline match
      case Pipeline.Direct =>
        host.direct.set((state, request))
        0.0
      case Pipeline.Picture =>
        var recorded: Picture = null
        val ms = Timing.ms {
          val recorder = PictureRecorder()
          val canvas   = recorder.beginRecording(Rect.Companion.makeWH(width.toFloat, height.toFloat), null)
          renderer.draw(canvas, state)
          recorded = recorder.finishRecordingAsPicture()
          recorder.close()
        }
        host.publishPicture(recorded, request)
        ms
      case Pipeline.Raster =>
        var snapshot: Image = null
        val ms = Timing.ms {
          val clip =
            if primedSurfaces < 2 then renderer.fullViewport
            else
              Rect.Companion.makeLTRB(
                math.min(damage.getLeft, lastDamage.getLeft),
                math.min(damage.getTop, lastDamage.getTop),
                math.max(damage.getRight, lastDamage.getRight),
                math.max(damage.getBottom, lastDamage.getBottom)
              )
          val surface = surfaces(next)
          renderer.draw(surface.getCanvas, state, clip)
          snapshot = surface.makeImageSnapshot()
        }
        primedSurfaces += 1
        lastDamage = damage
        next = 1 - next
        host.publishImage(snapshot, request)
        ms

  def fullViewport: Rect = renderer.fullViewport

  def damage(before: EditorState, after: EditorState): Rect = renderer.damage(before, after)

object WindowBench:

  def run(options: SpikeOptions, interactive: Boolean): Unit =
    val lines   = LoremDocument.load(options.document)
    val wrap    = SerenityWrap()
    val initial = EditorState.initial(lines, wrap.apply)
    val host    = onEdtCreate(options.pipeline, initial, options.shaping, options.subpixelText)
    val name    = s"window.${options.pipeline.toString.toLowerCase}"

    host.requestRedraw()
    host.awaitPainted(0L, 15_000L) match
      case None =>
        Report.note(s"$name.first_frame", "FAILED: no onRender within 15 s")
        val _ = host.onEdt(host.frame.dispose())
      case Some(first) =>
        val scale = host.onEdt(host.layer.getContentScale)
        val api   = host.onEdt(host.layer.getRenderApi.toString)
        val info  = host.onEdt(Option(host.layer.getRenderInfo).getOrElse("").replace('\n', ' '))
        Report.note(s"$name.setup", s"render_api=$api content_scale=$scale first_frame_ms=${f"${first.frameMs}%.2f"}")
        Report.note(s"$name.render_info", info)
        val producer = FrameProducer(options.pipeline, host, scale, options.shaping, options.subpixelText)
        if interactive then Interactive(host, producer, initial, wrap, name).start()
        else
          measure(options, host, producer, initial, wrap, name)
          options.screenshot.foreach(path => screenshot(host, path))
          val _ = host.onEdt(host.frame.dispose())

  private def onEdtCreate(pipeline: Pipeline, initial: EditorState, shaping: Shaping, subpixelText: Boolean): SkikoHost =
    val created = AtomicReference[SkikoHost]()
    SwingUtilities.invokeAndWait(() => created.set(SkikoHost(pipeline, initial, shaping, subpixelText)))
    created.get

  private def measure(
    options: SpikeOptions,
    host: SkikoHost,
    producer: FrameProducer,
    initial: EditorState,
    wrap: SerenityWrap,
    name: String
  ): Unit =
    val request = AtomicLong(0L)
    val workerMs, edtMs, frameMs, totalMs, roundTripMs = Vector.newBuilder[Double]
    var missed = 0
    (0 until options.warmupFrames + options.frames).foreach { index =>
      val id      = request.incrementAndGet()
      val started = System.nanoTime
      val worker  = producer.produce(initial, id, producer.fullViewport)
      host.requestRedraw()
      host.awaitPainted(id, 5_000L) match
        case Some(p) if index >= options.warmupFrames =>
          workerMs += worker; edtMs += p.edtMs; frameMs += p.frameMs; totalMs += worker + p.frameMs
          roundTripMs += (p.atNanos - started) / 1e6
        case Some(_) => ()
        case None    => missed += 1
    }
    Report.result(s"$name.full_frame.worker", workerMs.result(), "(off-EDT record/raster; 0 for direct)")
    Report.result(s"$name.full_frame.on_render", edtMs.result(), "(onRender only: Skiko records it into a picture)")
    Report.result(s"$name.full_frame.edt_frame_task", frameMs.result(), "(onRender start -> end of that EDT task: incl. Skiko raster/present)")
    Report.result(s"$name.full_frame.total", totalMs.result(), "(worker + EDT frame task)")
    Report.result(s"$name.full_frame.request_to_painted", roundTripMs.result(), s"missed=$missed")

    val letters                                        = TypingLetters()
    var state                                          = initial
    val updateMs, keyWorkerMs, keyEdtMs, damagedMs, i2p = Vector.newBuilder[Double]
    var typedMissed                                    = 0
    (0 until options.keys).foreach { _ =>
      val id      = request.incrementAndGet()
      val started = System.nanoTime
      val before  = state
      updateMs += Timing.ms { state = state.insert(letters.next(), wrap.apply) }
      val damage = producer.damage(before, state)
      val worker = producer.produce(state, id, damage)
      host.requestRedraw()
      host.awaitPainted(id, 5_000L) match
        case Some(p) =>
          keyWorkerMs += worker; keyEdtMs += p.frameMs
          if damage.getHeight < Geometry.LogicalHeight / 2 then damagedMs += worker + p.frameMs
          i2p += (p.atNanos - started) / 1e6
        case None => typedMissed += 1
      Timing.parkMs(math.max(0L, options.paceMs - (System.nanoTime - started) / 1_000_000L))
    }
    Report.result(s"$name.typing.state_update", updateMs.result())
    Report.result(s"$name.typing.worker", keyWorkerMs.result())
    Report.result(s"$name.typing.edt_frame_task", keyEdtMs.result())
    Report.result(
      s"$name.typing.damaged_frame",
      damagedMs.result(),
      "(worker + EDT frame task on keys whose damage is one line; only raster draws less than the full frame)"
    )
    Report.result(
      s"$name.typing.input_to_paint",
      i2p.result(),
      s"(key -> state update -> frame -> end of the EDT frame task) missed=$typedMissed pace_ms=${options.paceMs}"
    )

    host.painted.clear()
    val callsBefore = host.renderCalls.get
    val cpuBefore   = Timing.processCpuNanos
    val wallBefore  = System.nanoTime
    Timing.parkMs(options.idleSeconds * 1000L)
    val cpuMs  = (Timing.processCpuNanos - cpuBefore) / 1e6
    val wallMs = (System.nanoTime - wallBefore) / 1e6
    Report.note(
      s"$name.idle",
      f"seconds=${options.idleSeconds} cpu_ms=$cpuMs%.1f cpu_pct_of_one_core=${100 * cpuMs / wallMs}%.2f " +
        s"onRender_calls=${host.renderCalls.get - callsBefore}"
    )

  private def screenshot(host: SkikoHost, path: java.nio.file.Path): Unit =
    val bitmap = host.onEdt(host.layer.screenshot())
    val image  = Image.Companion.makeFromBitmap(bitmap)
    val data   = image.encodeToData(org.jetbrains.skia.EncodedImageFormat.PNG, 100, 0)
    java.nio.file.Files.write(path, data.getBytes)
    Report.note("window.screenshot", s"$path (SkiaLayer.screenshot: the layer's own pixels, not the compositor's)")

/** Typing by hand (and through an input method): every key is timed from the listener to the end of its paint. */
final class Interactive(
    host: SkikoHost,
    producer: FrameProducer,
    initial: EditorState,
    wrap: SerenityWrap,
    name: String
):
  private val worker  = Executors.newSingleThreadExecutor()
  private val pending = ConcurrentHashMap[Long, java.lang.Long]()
  private val samples = java.util.concurrent.ConcurrentLinkedQueue[java.lang.Double]()
  private val request = AtomicLong(0L)
  private val state   = AtomicReference(initial)

  private def typed(text: String, receivedNanos: Long): Unit =
    worker.execute { () =>
      text.foreach { char =>
        val before = state.get
        val after  = before.insert(char, wrap.apply)
        state.set(after)
        val id = request.incrementAndGet()
        pending.put(id, receivedNanos)
        val _ = producer.produce(after, id, producer.damage(before, after))
        host.requestRedraw()
      }
    }

  private def report(): Unit =
    val all = samples.toArray.toVector.map(_.asInstanceOf[java.lang.Double].doubleValue)
    Report.result(s"$name.interactive.input_to_paint", all, "(KeyEvent/InputMethodEvent listener -> end of the EDT frame task)")

  def start(): Unit =
    host.onEdt {
      host.layer.enableInputMethods(true)
      host.layer.addKeyListener(new KeyAdapter:
        override def keyTyped(event: KeyEvent): Unit =
          val char = event.getKeyChar
          if !Character.isISOControl(char) && char != KeyEvent.CHAR_UNDEFINED then typed(char.toString, System.nanoTime)
      )
      host.layer.addInputMethodListener(new InputMethodListener:
        def inputMethodTextChanged(event: InputMethodEvent): Unit =
          val text      = Option(event.getText)
          val committed = event.getCommittedCharacterCount
          val chars = text.map { iterator =>
            val builder = StringBuilder()
            var char    = iterator.first()
            while char != CharacterIterator.DONE do
              builder.append(char)
              char = iterator.next()
            builder.result()
          }.getOrElse("")
          println(s"IME event: committed='${chars.take(committed)}' composing='${chars.drop(committed)}'")
          if committed > 0 then typed(chars.take(committed), System.nanoTime)
          event.consume()
        def caretPositionChanged(event: InputMethodEvent): Unit = ()
      )
      host.frame.addWindowListener(new WindowAdapter:
        override def windowClosed(event: WindowEvent): Unit =
          report()
          sys.exit(0)
      )
      host.layer.requestFocusInWindow()
    }
    val _ = Thread
      .ofPlatform()
      .daemon()
      .start { () =>
        while true do
          val painted = host.painted.take()
          pending.keySet.forEach { id =>
            if id <= painted.request then
              val received = pending.remove(id)
              if received != null then
                samples.add((painted.atNanos - received.longValue) / 1e6)
                if samples.size % 25 == 0 then report()
          }
      }
    println("Type into the window (IME commits are logged). Close the window to print the summary.")
