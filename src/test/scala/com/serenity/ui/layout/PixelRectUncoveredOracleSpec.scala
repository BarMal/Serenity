package com.serenity.ui.layout

import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `uncoveredWithin` runs on every frame that keeps rows, over a rect per kept row: it must give the list it always
  * gave, for any holes.
  */
class PixelRectUncoveredOracleSpec extends AnyFlatSpec with Matchers:

  /** The algorithm `uncoveredWithin` had before it became a sweep, kept as the reference its output is held to. */
  private object Reference:

    def uncoveredWithin(bounds: PixelRect, holes: List[PixelRect]): List[PixelRect] =
      val clipped = holes.flatMap(clippedTo(bounds))
      val edges =
        (bounds.yPx :: bounds.bottomPx :: clipped.flatMap(hole => List(hole.yPx, hole.bottomPx))).distinct.sorted
      val bands = edges.zip(edges.drop(1)).map { (top, bottom) =>
        val covered = clipped.filter(hole => hole.yPx < bottom && top < hole.bottomPx).map(h => (h.xPx, h.rightPx))
        Band(top, bottom, spansBetween(bounds.xPx, bounds.rightPx, covered.sortBy(_._1)))
      }
      mergeStackedBands(bands).flatMap { band =>
        band.spans.map((left, right) => PixelRect(left, band.top, right - left, band.bottom - band.top))
      }

    final private case class Band(top: Int, bottom: Int, spans: List[(Int, Int)])

    private def clippedTo(bounds: PixelRect)(rect: PixelRect): Option[PixelRect] =
      val left   = rect.xPx.max(bounds.xPx)
      val top    = rect.yPx.max(bounds.yPx)
      val right  = rect.rightPx.min(bounds.rightPx)
      val bottom = rect.bottomPx.min(bounds.bottomPx)
      Option.when(left < right && top < bottom)(PixelRect(left, top, right - left, bottom - top))

    private def spansBetween(left: Int, right: Int, coveredByStart: List[(Int, Int)]): List[(Int, Int)] =
      val (cursor, spans) = coveredByStart.foldLeft((left, List.empty[(Int, Int)])) {
        case ((cursor, spans), (coveredLeft, coveredRight)) =>
          val withGap = if coveredLeft > cursor then (cursor, coveredLeft) :: spans else spans
          (cursor.max(coveredRight), withGap)
      }
      (if cursor < right then (cursor, right) :: spans else spans).reverse

    private def mergeStackedBands(bands: List[Band]): List[Band] =
      bands
        .foldLeft(List.empty[Band]) {
          case (previous :: rest, band) if previous.bottom == band.top && previous.spans == band.spans =>
            previous.copy(bottom = band.bottom) :: rest
          case (merged, band) => band :: merged
        }
        .reverse

  private val viewport = PixelRect(0, 0, 1920, 1080)

  /** What a frame that keeps rows hands in: a band per kept row across the pane, the gutter bands beside them, and a
    * few odd rects (a scrollbar strip, a panel).
    */
  private def keptRows(random: Random, rows: Int, rowHeight: Int, keepEvery: Int): List[PixelRect] =
    val bands = (0 until rows).filter(row => row % keepEvery != random.nextInt(keepEvery)).map { row =>
      PixelRect(random.nextInt(3) * 40, row * rowHeight + 20, 1800 - random.nextInt(30), rowHeight - random.nextInt(2))
    }
    val gutter =
      (0 until rows).filter(_ => random.nextInt(8) != 0).map(row => PixelRect(0, row * rowHeight + 20, 64, rowHeight))
    bands.toList ++ gutter ++ List(PixelRect(1890, 20, 14, 900), PixelRect(300, 400, 500, 300))

  "uncoveredWithin" should "give the list it always gave for kept rows and strips of a frame" in {
    val random = Random(5)
    (1 to 80).foreach { round =>
      val holes = random.shuffle(
        keptRows(
          random,
          rows = 10 + random.nextInt(50),
          rowHeight = 14 + random.nextInt(8),
          keepEvery = 2 + random.nextInt(8)
        )
      )
      withClue(s"round $round: ")(
        PixelRect.uncoveredWithin(viewport, holes) shouldBe Reference.uncoveredWithin(viewport, holes)
      )
    }
  }

  it should "give the list it always gave for any overlapping, touching, empty and out of bounds holes" in {
    val random = Random(9)
    (1 to 500).foreach { round =>
      val bounds = PixelRect(random.nextInt(5), random.nextInt(5), 1 + random.nextInt(60), 1 + random.nextInt(60))
      val holes = List.fill(random.nextInt(30)) {
        PixelRect(random.nextInt(80) - 10, random.nextInt(80) - 10, random.nextInt(40) - 3, random.nextInt(40) - 3)
      }
      withClue(s"round $round, $bounds, $holes: ")(
        PixelRect.uncoveredWithin(bounds, holes) shouldBe Reference.uncoveredWithin(bounds, holes)
      )
    }
  }

  it should "give the bounds whole for no holes, and nothing for empty bounds" in {
    PixelRect.uncoveredWithin(viewport, Nil) shouldBe List(viewport)
    PixelRect.uncoveredWithin(PixelRect(3, 3, 0, 9), List(PixelRect(0, 0, 9, 9))) shouldBe
      Reference.uncoveredWithin(PixelRect(3, 3, 0, 9), List(PixelRect(0, 0, 9, 9)))
  }
