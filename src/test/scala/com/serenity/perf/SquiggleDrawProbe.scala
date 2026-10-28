package com.serenity.perf

import org.jetbrains.skia.{
  BlendMode,
  IRect,
  Paint,
  PaintMode,
  PaintStrokeCap,
  PathBuilder,
  PathDirection,
  Rect,
  Region,
  Surface
}
import com.serenity.ui.skia.SkiaRasterSurfaceFactory

/** What drawing a typing frame's squiggles costs in Skia by call shape: the same 62 words of 21 two-pixel segments
  * drawn as a `drawRect` each, as one path, as one region, and as one batch of lines.
  */
object SquiggleDrawProbe:

  private val Words    = 62
  private val Segments = 21
  private val width    = 1920
  private val height   = 1080

  /** x, y, w, h for every segment: words spread down the surface, each a zig-zag two pixels deep. */
  private val rects: Vector[Array[Int]] = Vector.tabulate(Words) { word =>
    val originX = 40 + (word * 97) % (width - 200)
    val originY = 30 + (word * 17) % (height - 60)
    Array.tabulate(Segments * 4) { i =>
      val segment = i / 4
      i % 4 match
        case 0 => originX + segment * 2
        case 1 => originY + (if segment % 4 <= 2 then segment % 4 else 4 - segment % 4)
        case 2 => 2
        case _ => 1
    }
  }

  private val paint = new Paint()
  paint.setAntiAlias(false)
  paint.setColor(0xffc81e1e)
  paint.setBlendMode(BlendMode.SRC_OVER)
  private val stroke = new Paint()
  stroke.setAntiAlias(false)
  stroke.setColor(0xffc81e1e)
  stroke.setMode(PaintMode.STROKE)
  stroke.setStrokeWidth(1f)
  stroke.setStrokeCap(PaintStrokeCap.BUTT)

  private def loop(canvas: org.jetbrains.skia.Canvas): Unit =
    rects.foreach { word =>
      (0 until word.length by 4).foreach(i =>
        canvas.drawRect(
          Rect.Companion.makeLTRB(word(i).toFloat, word(i + 1).toFloat, (word(i) + word(i + 2)).toFloat, (word(i + 1) + word(i + 3)).toFloat),
          paint
        )
      )
    }

  private def path(canvas: org.jetbrains.skia.Canvas): Unit =
    rects.foreach { word =>
      val builder = PathBuilder()
      (0 until word.length by 4).foreach(i =>
        builder.addRect(
          Rect.Companion.makeLTRB(word(i).toFloat, word(i + 1).toFloat, (word(i) + word(i + 2)).toFloat, (word(i + 1) + word(i + 3)).toFloat),
          PathDirection.CLOCKWISE,
          0
        )
      )
      val built = builder.detach()
      canvas.drawPath(built, paint)
      built.close()
      builder.close()
    }

  private def region(canvas: org.jetbrains.skia.Canvas): Unit =
    rects.foreach { word =>
      val boxes = Array.tabulate(word.length / 4)(k =>
        IRect.Companion.makeLTRB(word(4 * k), word(4 * k + 1), word(4 * k) + word(4 * k + 2), word(4 * k + 1) + word(4 * k + 3))
      )
      val shape = new Region()
      shape.setRects(boxes)
      canvas.drawRegion(shape, paint)
      shape.close()
    }

  private def lines(canvas: org.jetbrains.skia.Canvas): Unit =
    rects.foreach { word =>
      val coords = new Array[Float](word.length)
      (0 until word.length / 4).foreach { k =>
        coords(4 * k)     = word(4 * k).toFloat
        coords(4 * k + 1) = word(4 * k + 1) + 0.5f
        coords(4 * k + 2) = (word(4 * k) + word(4 * k + 2)).toFloat
        coords(4 * k + 3) = word(4 * k + 1) + 0.5f
      }
      canvas.drawLines(coords, stroke)
    }

  private def median(values: Array[Double]): Double = values.sorted.apply(values.length / 2)

  private def time(name: String, surface: Surface)(draw: org.jetbrains.skia.Canvas => Unit): Unit =
    val canvas = surface.getCanvas
    (0 until 300).foreach(_ => draw(canvas))
    val samples = Array.fill(400) {
      val started = System.nanoTime()
      draw(canvas)
      (System.nanoTime() - started) / 1e6
    }
    println(f"$name%-28s median ${median(samples)}%.3f ms per frame of ${Words * Segments} rects in $Words batches")

  def main(args: Array[String]): Unit =
    val info = org.jetbrains.skia.ImageInfo.Companion
      .makeN32Premul(width, height)
      .withColorType(org.jetbrains.skia.ColorType.BGRA_8888)
    val surface = Surface.Companion.makeRaster(info)
    surface.getCanvas.clear(0xfff0f0eb)
    (0 until 3).foreach { round =>
      println(s"-- round $round")
      time("drawRect each", surface)(loop)
      time("PathBuilder + drawPath", surface)(path)
      time("Region.setRects + drawRegion", surface)(region)
      time("drawLines (stroke 1, butt)", surface)(lines)
    }
    sys.exit(0)
