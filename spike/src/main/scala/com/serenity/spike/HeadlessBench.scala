package com.serenity.spike

import java.nio.file.{Files, Path}

import org.jetbrains.skia.{EncodedImageFormat, Surface}

/** Frame costs with no window: Skia rasterising the editor into an offscreen 3000x2000 (at 2x) surface on the calling
  * thread. This is the "rasterise off-EDT" pipeline minus the blit, and the only numbers a display-less box can give.
  */
object HeadlessBench:

  def run(options: SpikeOptions): Unit =
    val lines              = LoremDocument.load(options.document)
    val wrap               = SerenityWrap()
    var state: EditorState = null
    val initialWrapMs      = Timing.ms { state = EditorState.initial(lines, wrap.apply) }
    Report.note("headless.document", s"lines=${lines.length} rows=${state.totalRows} initial_wrap_ms=${f"$initialWrapMs%.1f"}")

    val renderer = SkiaSceneRenderer(options.scale.toFloat, options.shaping, options.subpixelText)
    val surface  = Surface.Companion.makeRasterN32Premul(
      (Geometry.LogicalWidth * options.scale).toInt,
      (Geometry.LogicalHeight * options.scale).toInt
    )
    Report.note(
      "headless.setup",
      s"surface=${surface.getWidth}x${surface.getHeight} typeface=${renderer.typefaceName} scale=${options.scale} shaping=${options.shaping} subpixel_text=${options.subpixelText}"
    )
    val canvas = surface.getCanvas

    (0 until options.warmupFrames).foreach(_ => renderer.draw(canvas, state))
    val full = (0 until options.frames).map(_ => Timing.ms(renderer.draw(canvas, state)))
    Report.result("headless.full_frame_shaped_cached", full)
    val fills = (0 until options.frames).map(_ => Timing.ms(renderer.drawFillsOnly(canvas, state)))
    Report.result("headless.full_frame_fills_only", fills, "(background, gutter and line band; no text)")

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
      if damage == renderer.fullViewport || damage.getHeight >= Geometry.LogicalHeight / 2 then scrolledMs += render
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

  def writePng(surface: Surface, path: Path): Unit =
    val image = surface.makeImageSnapshot()
    val data  = image.encodeToData(EncodedImageFormat.PNG, 100, 0)
    Files.write(path, data.getBytes)
    image.close()

/** Lower-case letters with a space roughly every sixth key, seeded so runs type the same text. */
final class TypingLetters:
  private val random = scala.util.Random(42L)
  def next(): Char   = if random.nextInt(6) == 0 then ' ' else ('a' + random.nextInt(26)).toChar
