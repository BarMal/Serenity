package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The scrollbar's arithmetic: how a position in the document becomes a thumb, and how a pointer along the track
  * becomes a position again.
  */
class ScrollbarGeometrySpec extends AnyFlatSpec with Matchers:

  private val track  = 400
  private val extent = ScrollExtent(firstRow = 0, visibleRows = 40, totalRows = 160)

  "A scroll extent" should "name the last screenful as the furthest the view scrolls" in {
    extent.lastFirstRow shouldBe 120
    extent.clamped(500) shouldBe 120
    extent.clamped(-3) shouldBe 0
  }

  it should "only scroll while the document is longer than the view" in {
    extent.isScrollable shouldBe true
    extent.copy(totalRows = 40).isScrollable shouldBe false
    extent.copy(totalRows = 12).isScrollable shouldBe false
    extent.copy(visibleRows = 0).isScrollable shouldBe false
  }

  it should "page by a screen less one row, and by at least one" in {
    extent.pageRows shouldBe 39
    extent.copy(visibleRows = 1).pageRows shouldBe 1
  }

  "The thumb" should "be the visible share of the track, at the top of it at the start of the document" in {
    ScrollbarGeometry.thumb(extent, track) shouldBe Some(ScrollThumb(offsetPx = 0, lengthPx = 100))
  }

  it should "end at the end of the track at the end of the document" in {
    val atEnd = extent.copy(firstRow = extent.lastFirstRow)

    ScrollbarGeometry.thumb(atEnd, track).map(_.endPx) shouldBe Some(track)
  }

  it should "sit proportionally between the two as the view scrolls" in {
    val halfway = extent.copy(firstRow = 60)

    ScrollbarGeometry.thumb(halfway, track) shouldBe Some(ScrollThumb(offsetPx = 150, lengthPx = 100))
  }

  it should "stay at the end of the track when the view rests beyond the last screenful" in {
    val beyond = extent.copy(firstRow = 150)

    ScrollbarGeometry.thumb(beyond, track).map(_.endPx) shouldBe Some(track)
  }

  it should "never be shorter than the minimum, however long the document" in {
    val huge = ScrollExtent(firstRow = 0, visibleRows = 40, totalRows = 1_000_000)

    ScrollbarGeometry.thumb(huge, 600).map(_.lengthPx) shouldBe Some(ScrollbarGeometry.MinThumbPx)
  }

  it should "never be longer than the track" in {
    ScrollbarGeometry.thumb(extent, 20).map(_.lengthPx) shouldBe Some(20)
  }

  it should "be absent when the whole document is in view" in {
    ScrollbarGeometry.thumb(extent.copy(totalRows = 40), track) shouldBe None
  }

  it should "be absent from a track with no room" in {
    ScrollbarGeometry.thumb(extent, 0) shouldBe None
  }

  "A thumb offset" should "give back the row the thumb was drawn for" in
    (0 to extent.lastFirstRow by 7).foreach { row =>
      val shown  = extent.copy(firstRow = row)
      val offset = ScrollbarGeometry.thumb(shown, track).map(_.offsetPx).getOrElse(fail("expected a thumb"))
      val back   = ScrollbarGeometry.firstRowForThumbOffset(extent, track, offset)

      withClue(s"row $row via offset $offset gave $back: ")(
        ScrollbarGeometry.thumb(extent.copy(firstRow = back), track).map(_.offsetPx) shouldBe Some(offset)
      )
    }

  it should "reach the first and last rows at the ends of the track and no further" in {
    val travel = track - 100

    ScrollbarGeometry.firstRowForThumbOffset(extent, track, 0) shouldBe 0
    ScrollbarGeometry.firstRowForThumbOffset(extent, track, travel) shouldBe extent.lastFirstRow
    ScrollbarGeometry.firstRowForThumbOffset(extent, track, travel + 80) shouldBe extent.lastFirstRow
    ScrollbarGeometry.firstRowForThumbOffset(extent, track, -50) shouldBe 0
  }

  it should "map half the thumb's travel to half the way through the document" in {
    ScrollbarGeometry.firstRowForThumbOffset(extent, track, 150) shouldBe 60
  }

  "Dragging the thumb" should "keep it under the pointer where it was taken hold of" in {
    val grabbed = ScrollbarGeometry.thumb(extent.copy(firstRow = 30), track).getOrElse(fail("expected a thumb"))
    val grab    = 17
    val pointer = grabbed.offsetPx + grab

    ScrollbarGeometry.firstRowForDrag(extent, track, pointer, grab) shouldBe 30
    ScrollbarGeometry.firstRowForDrag(extent, track, pointer + 75, grab) shouldBe 60
  }

  it should "stop at the ends of the document however far the pointer goes" in {
    ScrollbarGeometry.firstRowForDrag(extent, track, pointerPx = 5_000, grabOffsetPx = 10) shouldBe extent.lastFirstRow
    ScrollbarGeometry.firstRowForDrag(extent, track, pointerPx = -5_000, grabOffsetPx = 10) shouldBe 0
  }

  "A pointer along the track" should "land on the thumb, remembering where on it" in {
    val shown = extent.copy(firstRow = 60)

    ScrollbarGeometry.hit(shown, track, pointerPx = 150) shouldBe Some(TrackHit.OnThumb(0))
    ScrollbarGeometry.hit(shown, track, pointerPx = 199) shouldBe Some(TrackHit.OnThumb(49))
    ScrollbarGeometry.hit(shown, track, pointerPx = 249) shouldBe Some(TrackHit.OnThumb(99))
  }

  it should "land above or below the thumb off it" in {
    val shown = extent.copy(firstRow = 60)

    ScrollbarGeometry.hit(shown, track, pointerPx = 149) shouldBe Some(TrackHit.AboveThumb)
    ScrollbarGeometry.hit(shown, track, pointerPx = 250) shouldBe Some(TrackHit.BelowThumb)
  }

  it should "land on nothing when there is nothing to scroll" in {
    ScrollbarGeometry.hit(extent.copy(totalRows = 30), track, pointerPx = 10) shouldBe None
  }

  "A press" should "page towards the pointer from a click in the track" in {
    val shown = extent.copy(firstRow = 60)

    ScrollbarGeometry.firstRowAfterPress(shown, TrackHit.AboveThumb) shouldBe 21
    ScrollbarGeometry.firstRowAfterPress(shown, TrackHit.BelowThumb) shouldBe 99
  }

  it should "not page past either end of the document" in {
    ScrollbarGeometry.firstRowAfterPress(extent.copy(firstRow = 10), TrackHit.AboveThumb) shouldBe 0
    ScrollbarGeometry.firstRowAfterPress(extent.copy(firstRow = 110), TrackHit.BelowThumb) shouldBe 120
  }

  it should "leave the view where it is when it takes hold of the thumb" in {
    ScrollbarGeometry.firstRowAfterPress(extent.copy(firstRow = 60), TrackHit.OnThumb(12)) shouldBe 60
  }
