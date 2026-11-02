package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

import com.serenity.ui.renderer.DeviceArea
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Frames that move rows and paint over them, shown in pooled images that each miss the frames the others carried: the
  * plan must leave every image equal to the surface, whatever it was told, and copy less of a scroll than its damage.
  */
class RasterCopyPlanSimulationSpec extends AnyFlatSpec with Matchers:

  private val width   = 48
  private val height  = 64
  private val whole   = DeviceArea(0, 0, width, height)
  private val colours = Vector(0x101010, 0x2040a0, 0xa04020, 0x30a050, 0xe0e0e0)

  /** The surface as the renderer sees it: a frame moves rows first, then paints. */
  final private class Surface:
    val pixels: Array[Int] = Array.fill(width * height)(0x000000)

    def move(move: RasterCopyPlan.Move): Unit =
      val before = pixels.clone()
      for
        row <- 0 until move.area.height
        col <- 0 until move.area.width
        sourceY = move.area.top + row
        sourceX = move.area.left + col
        targetY = sourceY + move.dyPx
        if sourceX >= 0 && sourceX < width && sourceY >= 0 && sourceY < height && targetY >= 0 && targetY < height
      do pixels(targetY * width + sourceX) = before(sourceY * width + sourceX)

    def paint(area: DeviceArea, colour: Int): Unit =
      for
        y <- area.top.max(0) until (area.top + area.height).min(height);
        x <- area.left.max(0) until (area.left + area.width).min(width)
      do pixels(y * width + x) = colour

  private def imageOf(): BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

  private def pixelsOf(image: BufferedImage): Array[Int] =
    image.getRGB(0, 0, width, height, null, 0, width).map(_ & 0xffffff)

  private def copy(surface: Surface, image: BufferedImage, area: DeviceArea): Unit =
    DeviceAreas.intersect(area, whole) shouldBe area
    for y <- area.top until area.top + area.height; x <- area.left until area.left + area.width do
      image.setRGB(x, y, surface.pixels(y * width + x))

  /** One frame as the renderer reports it, and the plan's result on the image it is shown in. */
  final private class Window(images: Vector[BufferedImage], choose: Int => Int):
    val surface            = new Surface
    private val last       = new AtomicReference[Option[RasterCopyPlan.Filled]](None)
    private val beforeLast = new AtomicReference[Option[RasterCopyPlan.Filled]](None)
    private val frame      = new AtomicInteger(0)
    private val copied     = new AtomicLong(0L)

    def copiedPixels: Long = copied.get()

    def show(
      moves: List[RasterCopyPlan.Move],
      paints: List[(DeviceArea, Int)],
      knownDamage: Boolean,
      knownWrites: Boolean,
      reported: DeviceArea => DeviceArea
    ): Unit =
      moves.foreach(surface.move)
      paints.foreach((area, colour) => surface.paint(area, colour))
      val writes = paints.map((area, _) => DeviceAreas.intersect(reported(area), whole))
      val damage = moves.map(move => DeviceAreas.intersect(DeviceAreas.shifted(move.area, move.dyPx), whole)) ++ writes
      val image  = images(choose(frame.getAndIncrement()))
      val now = RasterCopyPlan.Filled(
        image,
        Option.when(knownDamage)(damage),
        moves,
        Option.when(knownWrites)(writes)
      )
      val plan = RasterCopyPlan.plan(image, last.get(), beforeLast.get(), now, whole)
      plan.shifts.foreach(move => ImageMove(image, move.area, move.dyPx))
      plan.copies.foreach(copy(surface, image, _))
      val _ = copied.addAndGet(
        plan.copies.map(area => DeviceAreas.intersect(area, whole)).map(area => area.width.toLong * area.height).sum
      )
      pixelsOf(image) shouldBe surface.pixels
      beforeLast.set(last.get())
      last.set(Some(now))

  private def randomArea(random: scala.util.Random): DeviceArea =
    DeviceArea(random.nextInt(width), random.nextInt(height), 1 + random.nextInt(width), 1 + random.nextInt(24))

  for poolSize <- List(2, 3) do
    s"A plan over $poolSize pooled images" should "leave each image equal to the surface, whatever moved and was painted" in
      (1 to 400).foreach { seed =>
        val random = new scala.util.Random(seed)
        val window = new Window(
          Vector.fill(poolSize)(imageOf()),
          frame => if poolSize == 2 then frame % 2 else random.nextInt(poolSize)
        )
        (1 to 14).foreach { _ =>
          val moves = List.fill(random.nextInt(3))(
            RasterCopyPlan.Move(randomArea(random), (1 + random.nextInt(20)) * (if random.nextBoolean() then 1 else -1))
          )
          val paints = List.fill(random.nextInt(4))(randomArea(random) -> colours(random.nextInt(colours.size)))
          val slack  = random.nextInt(3)
          window.show(
            moves,
            paints,
            knownDamage = random.nextInt(6) != 0,
            knownWrites = random.nextInt(6) != 0,
            reported =
              area => DeviceArea(area.left - slack, area.top - slack, area.width + 2 * slack, area.height + 2 * slack)
          )
        }
      }

  it should "copy far less of a scroll than the damage the scroll reports" in {
    val random = new scala.util.Random(2380L)
    def scrollOf(window: Window, dy: Int): Unit =
      window.show(
        List(RasterCopyPlan.Move(DeviceArea(0, 0, width, height), dy)),
        List(
          DeviceArea(0, height - 4, width, 4) -> colours(random.nextInt(colours.size)),
          DeviceArea(width - 3, 0, 3, height) -> 0xe0e0e0
        ),
        knownDamage = true,
        knownWrites = true,
        reported = identity
      )
    val window = new Window(Vector(imageOf(), imageOf()), _ % 2)
    window.show(Nil, List(whole -> 0x2040a0), knownDamage = true, knownWrites = true, reported = identity)
    scrollOf(window, -4)
    val before = window.copiedPixels
    (1 to 10).foreach(_ => scrollOf(window, -4))
    val perFrame = (window.copiedPixels - before) / 10
    perFrame should be < (width.toLong * height / 4)
  }
