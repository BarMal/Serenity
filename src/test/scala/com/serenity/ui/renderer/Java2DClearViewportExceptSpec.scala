package com.serenity.ui.renderer

import java.awt.geom.Area
import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color, Font, Rectangle}

import com.serenity.ui.layout.{CellMetrics, PixelRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Java2DClearViewportExceptSpec extends AnyFlatSpec with Matchers:

  private val metrics       = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font          = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val logicalWidth  = 211
  private val logicalHeight = 157

  private val rowBands: List[PixelRect] = (0 until 9).map(row => PixelRect(20, 4 + row * 13, 120, 13)).toList

  private val preservedSets: List[(String, List[PixelRect])] = List(
    "a single rectangle"             -> List(PixelRect(30, 40, 50, 20)),
    "stacked pane rows"              -> rowBands,
    "rows in two side-by-side panes" -> (rowBands ++ rowBands.map(r => r.copy(xPx = 150, widthPx = 61))),
    "overlapping and offscreen rects" -> List(
      PixelRect(-10, -10, 40, 40),
      PixelRect(20, 20, 40, 40),
      PixelRect(200, 150, 90, 90)
    ),
    "a rect covering everything" -> List(PixelRect(0, 0, logicalWidth, logicalHeight))
  )

  private val colours: List[(String, Color)] = List(
    "opaque"      -> new Color(30, 40, 50),
    "translucent" -> new Color(200, 100, 50, 120),
    "alpha 0"     -> new Color(10, 20, 30, 0)
  )

  private val scales = List(1.0, 2.0, 1.25)

  for
    (setName, preserved) <- preservedSets
    (colourName, colour) <- colours
    scale                <- scales
  do
    "Java2DRenderSurface.clearViewportExcept" should s"match the clip-based clear for $setName, $colourName, scale $scale" in {
      val expected = noisyImage(scale)
      clipBasedClear(expected, scale, colour, preserved)

      val actual  = noisyImage(scale)
      val surface = new Java2DRenderSurface(actual, metrics, font, _ => (), logicalWidth, logicalHeight, scale, scale)
      surface.clearViewportExcept(colour, preserved)
      surface.flush()

      val fullyCleared = noisyImage(scale)
      val clearAll =
        new Java2DRenderSurface(fullyCleared, metrics, font, _ => (), logicalWidth, logicalHeight, scale, scale)
      clearAll.clearViewport(colour)
      clearAll.flush()

      // The clip used to leave the device column straddling a fractional right edge stale; filling rectangles clears it,
      // exactly as `clearViewport` always has.
      val straddlingColumn = Option.when(logicalWidth * scale != math.floor(logicalWidth * scale))(
        math.floor(logicalWidth * scale).toInt
      )
      differingPixels(expected, actual).foreach { (x, y) =>
        straddlingColumn should contain(x)
        actual.getRGB(x, y) shouldBe fullyCleared.getRGB(x, y)
      }
      if straddlingColumn.isEmpty then differingPixels(expected, actual) shouldBe empty
    }

  /** The `Area`-clip implementation `clearViewportExcept` used before it filled the uncovered rectangles directly. */
  private def clipBasedClear(image: BufferedImage, scale: Double, colour: Color, preserved: List[PixelRect]): Unit =
    val graphics = image.createGraphics()
    try
      graphics.scale(scale, scale)
      val clearable = new Area(new Rectangle(0, 0, logicalWidth, logicalHeight))
      preserved.foreach(rect =>
        clearable.subtract(new Area(new Rectangle(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx)))
      )
      graphics.clip(clearable)
      if colour.getAlpha == 0 then graphics.setComposite(AlphaComposite.Src)
      graphics.setColor(colour)
      graphics.fillRect(0, 0, logicalWidth, logicalHeight)
    finally graphics.dispose()

  private def noisyImage(scale: Double): BufferedImage =
    val width  = Java2DRenderSurface.deviceImageDimension(logicalWidth, scale)
    val height = Java2DRenderSurface.deviceImageDimension(logicalHeight, scale)
    val image  = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val random = new scala.util.Random(width * 31 + height)
    for
      y <- 0 until height
      x <- 0 until width
    do image.setRGB(x, y, random.nextInt())
    image

  private def differingPixels(expected: BufferedImage, actual: BufferedImage): IndexedSeq[(Int, Int)] =
    for
      y <- 0 until expected.getHeight
      x <- 0 until expected.getWidth
      if expected.getRGB(x, y) != actual.getRGB(x, y)
    yield (x, y)
