package com.serenity.spike

import java.nio.file.{Files, Path}

import org.jetbrains.skia.{Bitmap, EncodedImageFormat, Surface}

/** Frame costs with no window: Skia rasterising the editor into an offscreen 3000x2000 (at 2x) surface on the calling
  * thread. This is the "rasterise off-EDT" pipeline minus the blit, and the only numbers a display-less box can give.
  */
object HeadlessBench:

  def run(options: SpikeOptions): Unit =
    val lines              = LoremDocument.load(options.document)
    val wrap               = SerenityWrap()
    var state: EditorState = null
    val initialWrapMs      = Timing.ms { state = EditorState.initial(lines, wrap.apply) }
    val initial            = state
    Report.note("headless.document", s"lines=${lines.length} rows=${state.totalRows} initial_wrap_ms=${f"$initialWrapMs%.1f"}")

    val renderer = SkiaSceneRenderer(options.scale.toFloat, options.shaping, options.subpixelText, options.textCache)
    val width    = (Geometry.LogicalWidth * options.scale).toInt
    val height   = (Geometry.LogicalHeight * options.scale).toInt
    val surface  = Surface.Companion.makeRasterN32Premul(width, height)
    Report.note(
      "headless.setup",
      s"surface=${surface.getWidth}x${surface.getHeight} typeface=${renderer.typefaceName} scale=${options.scale} " +
        s"shaping=${options.shaping} subpixel_text=${options.subpixelText} cache=${options.textCache}"
    )
    val canvas = surface.getCanvas

    (0 until options.warmupFrames).foreach(_ => renderer.draw(canvas, state))
    val full = (0 until options.frames).map(_ => Timing.ms(renderer.draw(canvas, state)))
    Report.result("headless.full_frame_shaped_cached", full, s"(cache=${options.textCache}: every cache warm)")
    val fills = (0 until options.frames).map(_ => Timing.ms(renderer.drawFillsOnly(canvas, state)))
    Report.result("headless.full_frame_fills_only", fills, "(background, gutter and line band; no text)")

    if options.textCache != TextCache.Rows then
      val coldParagraphs = (0 until math.max(10, options.frames / 4)).map { _ =>
        renderer.clearParagraphCache()
        Timing.ms(renderer.draw(canvas, state))
      }
      Report.result(
        "headless.full_frame_paragraph_cache_cold",
        coldParagraphs,
        s"(rows shaped, every visible paragraph's ${options.textCache} rebuilt)"
      )

    val cold = (0 until math.max(10, options.frames / 4)).map { _ =>
      renderer.clearShapingCache()
      Timing.ms(renderer.draw(canvas, state))
    }
    Report.result("headless.full_frame_reshape_all_rows", cold)

    val letters     = TypingLetters()
    val updateMs    = Vector.newBuilder[Double]
    val damagedMs   = Vector.newBuilder[Double]
    val scrolledMs  = Vector.newBuilder[Double]
    val inputToDone = Vector.newBuilder[Double]
    (0 until options.keys).foreach { _ =>
      val started = System.nanoTime
      val before  = state
      updateMs += Timing.ms { state = state.insert(letters.next(), wrap.apply) }
      val damage = renderer.damage(before, state)
      val render = Timing.ms(renderer.draw(canvas, state, damage))
      if damage.getHeight >= renderer.viewport.getHeight / 2 then scrolledMs += render
      else damagedMs += render
      inputToDone += (System.nanoTime - started) / 1e6
    }
    Report.result("headless.typing.state_update", updateMs.result(), "(string insert + Serenity rewrap of one line)")
    Report.result("headless.typing.damaged_frame", damagedMs.result(), "(clip = the edited line's rows)")
    Report.result("headless.typing.large_damage_frame", scrolledMs.result(), "(row count or caret row changed)")
    Report.result("headless.typing.input_to_rendered", inputToDone.result(), "(update + render, no present)")

    options.screenshot.foreach { path =>
      renderer.draw(canvas, state)
      writePng(surface, path)
      Report.note("headless.screenshot", path.toString)
    }

    ScrollBench.run(options, renderer, initial, width, height, "headless")

  def writePng(surface: Surface, path: Path): Unit =
    val image = surface.makeImageSnapshot()
    val data  = image.encodeToData(EncodedImageFormat.PNG, 100, 0)
    Files.write(path, data.getBytes)
    image.close()

/** Keeps the last frame in one of two raster surfaces and, when the view moves by less than half the viewport, draws
  * it shifted into the other and paints only the exposed band. Two surfaces so a snapshot is never of the surface being
  * drawn (no copy-on-write).
  */
final class ScrollBlitter(renderer: SkiaSceneRenderer, width: Int, height: Int):
  private val surfaces = Vector.fill(2)(Surface.Companion.makeRasterN32Premul(width, height))
  private var current  = 0
  private var last: Option[EditorState] = None

  /** The surface holding the last frame. */
  def front: Surface = surfaces(current)

  /** Draws `state`; true when it reused the previous frame. */
  def frame(state: EditorState, allowBlit: Boolean): Boolean =
    val target = surfaces(1 - current)
    val exposed = last
      .filter(_ => allowBlit)
      .flatMap(before => renderer.exposedByScroll(before, state).map(before -> _))
      .filter((_, band) => band.getHeight <= renderer.viewport.getHeight / 2)
    exposed match
      case Some((before, band)) =>
        val snapshot = surfaces(current).makeImageSnapshot()
        val shiftPx  = -(state.anchorRow - before.anchorRow) * Geometry.LineHeight * renderer.scale
        target.getCanvas.drawImage(snapshot, 0f, shiftPx)
        snapshot.close()
        renderer.draw(target.getCanvas, state, band)
      case None => renderer.draw(target.getCanvas, state)
    current = 1 - current
    last = Some(state)
    exposed.isDefined

/** Scrolling without typing: small wheel steps and PageDown-sized jumps, each frame a full repaint or a blit of the
  * previous frame plus the exposed band (`--scroll-blit=true`, raster surfaces only).
  */
object ScrollBench:

  def run(options: SpikeOptions, renderer: SkiaSceneRenderer, initial: EditorState, width: Int, height: Int, prefix: String)
    : Unit =
    val pageRows = math.max(1, renderer.visibleRowCount - 1)
    Report.note(
      s"$prefix.scroll.setup",
      s"wheel_rows=${options.wheelRows} page_rows=$pageRows events=${options.scrolls} blit=${options.scrollBlit} " +
        s"cache=${options.textCache}"
    )
    renderer.clearShapingCache()
    val blitter = ScrollBlitter(renderer, width, height)
    (0 until options.warmupFrames).foreach(_ => blitter.frame(initial, allowBlit = false))

    def series(name: String, step: Int, note: String): EditorState =
      val half  = math.min(options.scrolls / 2, (initial.totalRows - 1 - initial.anchorRow) / step)
      var state = initial
      var blits = 0
      val ms = (0 until 2 * half).map { index =>
        state = state.scrolled(if index < half then step else -step)
        var blitted = false
        val took    = Timing.ms { blitted = blitter.frame(state, options.scrollBlit) }
        if blitted then blits += 1
        took
      }
      Report.result(s"$prefix.scroll.$name", ms, s"($note; $half down then back up; blitted=$blits)")
      state

    val wheelEnd = series("wheel", options.wheelRows, s"${options.wheelRows} rows per event")
    if options.scrollBlit then
      val check = Surface.Companion.makeRasterN32Premul(width, height)
      renderer.draw(check.getCanvas, wheelEnd)
      Report.note(
        s"$prefix.scroll.blit_vs_full_differing_pixels",
        s"${differingPixels(blitter.front, check)} (last blitted frame against a full repaint of the same state)"
      )
      check.close()
    val _ = series("page", pageRows, s"$pageRows rows per event (PageDown)")

  private def differingPixels(a: Surface, b: Surface): Int =
    def pixels(surface: Surface): Array[Byte] =
      val bitmap = Bitmap()
      bitmap.allocN32Pixels(surface.getWidth, surface.getHeight, false)
      val _ = surface.readPixels(bitmap, 0, 0)
      bitmap.readPixels(bitmap.getImageInfo, bitmap.getRowBytes, 0, 0)
    val (left, right) = (pixels(a), pixels(b))
    (0 until left.length by 4).count(i => left(i) != right(i) || left(i + 1) != right(i + 1) || left(i + 2) != right(i + 2))

/** Lower-case letters with a space roughly every sixth key, seeded so runs type the same text. */
final class TypingLetters:
  private val random = scala.util.Random(42L)
  def next(): Char   = if random.nextInt(6) == 0 then ' ' else ('a' + random.nextInt(26)).toChar
