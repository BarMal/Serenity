package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, Font, Rectangle}
import javax.swing.JPanel

import com.serenity.state.models.CaretStyle
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{CaretGeometry, CaretPaint, CaretRecordingSurface}
import com.serenity.ui.skia.SkiaCaretInk
import com.serenity.ui.terminal.{SwingCanvasGeometry, SwingWindow}
import org.jetbrains.skia.{Bitmap, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A styled caret is drawn as the same shapes on Java2D and Skia, a plain one exactly as before, and what a caret
  * covers is what a repaint of its rect covers.
  */
class CaretStylePaintSpec extends AnyFlatSpec with Matchers:

  private val body   = new Rectangle(20, 10, 2, 20)
  private val size   = 60
  private val black  = 0xff000000
  private val white  = 0xffffffff
  private val italic = CaretStyle(italic = true)
  private val styles =
    List(italic, CaretStyle(bold = true), CaretStyle(underline = true), CaretStyle(strikethrough = true))
  private val font    = Font(Font.MONOSPACED, Font.PLAIN, 10)
  private val metrics = CellMetrics.fromFont(font)

  private def java2d(carets: List[CaretPaint]): Int => Int => Int =
    val base   = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
    val target = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
    val g      = target.createGraphics()
    try
      g.setClip(0, 0, size, size)
      SwingWindow.paintPresentedFrame(g, Some(base), carets, size, size)
    finally g.dispose()
    x => y => target.getRGB(x, y) | black

  private def skia(carets: List[CaretPaint], scale: Float = 1f): Int => Int => Int =
    val surface = Surface.Companion.makeRasterN32Premul(size, size)
    surface.getCanvas.clear(black)
    SkiaCaretInk.draw(surface.getCanvas, carets, scale)
    val bitmap = Bitmap.Companion.makeFromImage(surface.makeImageSnapshot())
    x => y => bitmap.getColor(x, y) | black

  private val backends: List[(String, List[CaretPaint] => Int => Int => Int)] =
    List("Java2D" -> (java2d(_)), "Skia" -> (skia(_)))

  backends.foreach { (name, paint) =>
    s"$name" should "paint a plain caret as its rect, byte for byte" in {
      val pixel = paint(List(CaretPaint(body, Color.WHITE)))
      for x <- 0 until size; y <- 0 until size do pixel(x)(y) shouldBe (if body.contains(x, y) then white else black)
    }

    it should "slant an italic caret to the right at the top, about its base" in {
      val pixel = paint(List(CaretPaint.styled(body, Color.WHITE, italic)))
      pixel(21)(28) shouldBe white
      pixel(24)(12) shouldBe white
      pixel(20)(12) shouldBe black
      pixel(26)(28) shouldBe black
    }

    it should "thicken a bold caret" in {
      val pixel = paint(List(CaretPaint.styled(body, Color.WHITE, CaretStyle(bold = true))))
      pixel(23)(20) shouldBe white
      pixel(24)(20) shouldBe black
    }

    it should "rule under an underlined caret and through a struck-through one" in {
      val under  = paint(List(CaretPaint.styled(body, Color.WHITE, CaretStyle(underline = true))))
      val struck = paint(List(CaretPaint.styled(body, Color.WHITE, CaretStyle(strikethrough = true))))
      under(16)(29) shouldBe white
      under(16)(20) shouldBe black
      struck(16)(19) shouldBe white
      struck(16)(29) shouldBe black
    }

    it should "paint nothing outside the bounds a styled caret reports" in
      styles.foreach { style =>
        val caret  = CaretPaint.styled(body, Color.WHITE, style)
        val pixel  = paint(List(caret))
        val bounds = caret.rect
        for x <- 0 until size; y <- 0 until size if !bounds.contains(x, y) do pixel(x)(y) shouldBe black
      }
  }

  "CaretPaint" should "report the bounds of a styled caret as its rect, so a repaint of it covers every pixel" in {
    CaretPaint.styled(body, Color.WHITE, italic).rect shouldBe CaretGeometry.bounds(body, italic)
    CaretPaint.styled(body, Color.WHITE, CaretStyle.Plain) shouldBe CaretPaint(body, Color.WHITE)
  }

  "Skia" should "scale a styled caret with the layer's content scale" in {
    val pixel = skia(List(CaretPaint.styled(new Rectangle(10, 5, 2, 10), Color.WHITE, CaretStyle(bold = true))), 2f)
    pixel(20)(20) shouldBe white
    pixel(27)(20) shouldBe white
    pixel(28)(20) shouldBe black
  }

  "CaretRecordingSurface" should "record the style of a caret fill beside its placed rect" in {
    val canvas = JPanel()
    canvas.setSize(Dimension(400, 300))
    val surface = CaretRecordingSurface.forGeometry(metrics, font, SwingCanvasGeometry.of(canvas))
    surface.fillCaret(10, 20, 2, 14, RenderColor.fromAwt(Color.WHITE), italic)
    surface.fillPixelRect(50, 60, 2, 14, RenderColor.fromAwt(Color.WHITE))

    surface.recordedFills shouldBe List(
      CaretRecordingSurface.CaretFill(PixelRect(10, 20, 2, 14), Color.WHITE, italic),
      CaretRecordingSurface.CaretFill(PixelRect(50, 60, 2, 14), Color.WHITE)
    )
  }
end CaretStylePaintSpec
