package com.serenity.ui.skia

import scala.util.Random

import com.serenity.text.AllocationProbe
import com.serenity.ui.renderer.DeviceArea
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `disjoint` runs twice on every frame, over a record per spell-check squiggle: it must give the list it always gave,
  * for any areas, and not allocate in proportion to bands times areas.
  */
class RasterCopyPlanDisjointSpec extends AnyFlatSpec with Matchers:

  /** The algorithm `disjoint` had before it became a sweep, kept as the reference its output is held to. */
  private object Reference:

    def disjoint(areas: List[DeviceArea]): List[DeviceArea] =
      val present = areas.filterNot(_.isEmpty)
      val edges   = present.flatMap(area => List(area.top, area.top + area.height)).distinct.sorted
      val bands = edges.zip(edges.drop(1)).flatMap { (top, bottom) =>
        val spans = joined(
          present
            .filter(area => area.top < bottom && area.top + area.height > top)
            .map(area => (area.left, area.left + area.width))
            .sorted
        )
        Option.when(spans.nonEmpty)(Band(top, bottom, spans))
      }
      stacked(
        bands
          .foldLeft(List.empty[Band]) {
            case (latest :: earlier, band) if latest.bottom == band.top && latest.spans == band.spans =>
              latest.copy(bottom = band.bottom) :: earlier
            case (done, band) => band :: done
          }
          .reverse
          .flatMap(band =>
            band.spans.map((left, right) => DeviceArea(left, band.top, right - left, band.bottom - band.top))
          )
      )

    private def stacked(areas: List[DeviceArea]): List[DeviceArea] =
      areas
        .sortBy(area => (area.left, area.width, area.top))
        .foldLeft(List.empty[DeviceArea]) {
          case (latest :: earlier, area)
              if latest.left == area.left && latest.width == area.width && latest.top + latest.height == area.top =>
            latest.copy(height = latest.height + area.height) :: earlier
          case (done, area) => area :: done
        }
        .reverse

    final private case class Band(top: Int, bottom: Int, spans: List[(Int, Int)])

    private def joined(sorted: List[(Int, Int)]): List[(Int, Int)] =
      sorted
        .foldLeft(List.empty[(Int, Int)]) {
          case ((left, right) :: earlier, (nextLeft, nextRight)) if nextLeft <= right =>
            (left, right.max(nextRight)) :: earlier
          case (done, span) => span :: done
        }
        .reverse

  private def anyAreas(random: Random, count: Int, width: Int, height: Int): List[DeviceArea] =
    List.fill(count)(
      DeviceArea(
        random.nextInt(width),
        random.nextInt(height),
        random.nextInt(width / 3) - 2,
        random.nextInt(height / 3) - 2
      )
    )

  /** Lines of text with a squiggle under each of many words, as a typing frame records them, besides larger areas. */
  private def squiggles(random: Random, lines: Int, perLine: Int): List[DeviceArea] =
    val underlines = for
      line <- 0 until lines
      word <- 0 until perLine
      if random.nextInt(4) != 0
    yield DeviceArea(8 + word * 30 + random.nextInt(6), 14 + line * 18, 2 + random.nextInt(24), 1 + random.nextInt(2))
    underlines.toList ++ List(DeviceArea(0, 0, 900, 12), DeviceArea(880, 0, 20, 600), DeviceArea(0, 580, 900, 20))

  /** What a typing frame records on prose the dictionary flags throughout: each line underlined end to end, bar a gap.
    */
  private def runsOfSquiggles(random: Random, lines: Int, perLine: Int): List[DeviceArea] =
    val underlines = for
      line <- 0 until lines
      gap = random.nextInt(perLine)
      word <- 0 until perLine
      if word != gap
    yield DeviceArea(8 + word * 30, 14 + line * 18, 30 + random.nextInt(3), 1)
    underlines.toList ++ List(DeviceArea(0, 0, 900, 12), DeviceArea(880, 0, 20, 600), DeviceArea(0, 580, 900, 20))

  "disjoint" should "give the list it always gave for areas that overlap, touch and are empty" in {
    val random = Random(7)
    (1 to 400).foreach { round =>
      val areas = anyAreas(random, count = 1 + random.nextInt(40), width = 48, height = 64)
      withClue(s"round $round, $areas: ")(RasterCopyPlan.disjoint(areas) shouldBe Reference.disjoint(areas))
    }
  }

  it should "give the list it always gave for squiggles along lines of text" in {
    val random = Random(11)
    (1 to 60).foreach { round =>
      val areas = random.shuffle(squiggles(random, lines = 1 + random.nextInt(40), perLine = 1 + random.nextInt(30)))
      withClue(s"round $round: ")(RasterCopyPlan.disjoint(areas) shouldBe Reference.disjoint(areas))
    }
  }

  it should "give the list it always gave for squiggles that run end to end" in {
    val random = Random(13)
    (1 to 60).foreach { round =>
      val areas =
        random.shuffle(runsOfSquiggles(random, lines = 1 + random.nextInt(40), perLine = 2 + random.nextInt(30)))
      withClue(s"round $round: ")(RasterCopyPlan.disjoint(areas) shouldBe Reference.disjoint(areas))
    }
  }

  it should "give nothing for no areas and for areas with no pixels" in {
    RasterCopyPlan.disjoint(Nil) shouldBe Nil
    RasterCopyPlan.disjoint(List(DeviceArea(3, 3, 0, 5), DeviceArea(1, 1, 4, -1))) shouldBe Nil
  }

  it should "allocate less than half of what the band-by-band merge did for a typing frame's squiggles" in {
    val areas = runsOfSquiggles(Random(3), lines = 40, perLine = 32)
    areas.size should be > 800
    val before = AllocationProbe.allocatedBytes(Reference.disjoint(areas))
    val after  = AllocationProbe.allocatedBytes(RasterCopyPlan.disjoint(areas))
    assume(before.isDefined && after.isDefined, "per-thread allocation is not supported on this JVM")
    withClue(s"before ${before.get} bytes, after ${after.get}: ")(after.get should be < before.get / 2)
  }
