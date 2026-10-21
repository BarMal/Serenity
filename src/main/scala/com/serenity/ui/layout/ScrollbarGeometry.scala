package com.serenity.ui.layout

/** Where a view sits in its document, counted in visual rows: the first row shown, how many rows fit, and how many
  * there are.
  */
final case class ScrollExtent(firstRow: Int, visibleRows: Int, totalRows: Int):

  /** The first row of the last full screen: scrolling further would only show blank space. */
  def lastFirstRow: Int = (totalRows - visibleRows).max(0)

  def isScrollable: Boolean = visibleRows > 0 && totalRows > visibleRows

  def clamped(row: Int): Int = row.max(0).min(lastFirstRow)

  /** Rows a click in the track moves by: a screen, less one row so the eye keeps a line of context. */
  def pageRows: Int = (visibleRows - 1).max(1)

/** The thumb's place along a track, in pixels from the track's start. */
final case class ScrollThumb(offsetPx: Int, lengthPx: Int):

  def endPx: Int = offsetPx + lengthPx

  def contains(pointerPx: Int): Boolean = pointerPx >= offsetPx && pointerPx < endPx

/** What a pointer along the track lands on. */
enum TrackHit:
  /** `grabOffsetPx` is how far below the thumb's start the pointer is, kept constant while the thumb is dragged. */
  case OnThumb(grabOffsetPx: Int)
  case AboveThumb
  case BelowThumb

/** The scrollbar's arithmetic, apart from any pixels drawn or events received: how a [[ScrollExtent]] becomes a thumb,
  * and how a pointer along the track becomes a scroll position again.
  */
object ScrollbarGeometry:

  /** A thumb shorter than this is too small to grab, however long the document. */
  val MinThumbPx: Int = 24

  def thumb(extent: ScrollExtent, trackPx: Int, minThumbPx: Int = MinThumbPx): Option[ScrollThumb] =
    Option.when(extent.isScrollable && trackPx > 0) {
      val length = math.round(trackPx.toDouble * extent.visibleRows / extent.totalRows).toInt
      val sized  = length.max(minThumbPx).min(trackPx)
      ScrollThumb(offsetPx = math.round(travelShare(extent) * (trackPx - sized)).toInt, lengthPx = sized)
    }

  /** The first row shown when the thumb's start sits `thumbOffsetPx` down the track: the inverse of [[thumb]]. */
  def firstRowForThumbOffset(
    extent: ScrollExtent,
    trackPx: Int,
    thumbOffsetPx: Int,
    minThumbPx: Int = MinThumbPx
  ): Int =
    thumb(extent, trackPx, minThumbPx).fold(0) { current =>
      val travel = trackPx - current.lengthPx
      if travel <= 0 then 0
      else extent.clamped(math.round(thumbOffsetPx.max(0).min(travel).toDouble / travel * extent.lastFirstRow).toInt)
    }

  /** The first row shown while a thumb grabbed `grabOffsetPx` below its start follows a pointer at `pointerPx`. */
  def firstRowForDrag(
    extent: ScrollExtent,
    trackPx: Int,
    pointerPx: Int,
    grabOffsetPx: Int,
    minThumbPx: Int = MinThumbPx
  ): Int =
    firstRowForThumbOffset(extent, trackPx, pointerPx - grabOffsetPx, minThumbPx)

  def hit(extent: ScrollExtent, trackPx: Int, pointerPx: Int, minThumbPx: Int = MinThumbPx): Option[TrackHit] =
    thumb(extent, trackPx, minThumbPx).map { current =>
      if current.contains(pointerPx) then TrackHit.OnThumb(pointerPx - current.offsetPx)
      else if pointerPx < current.offsetPx then TrackHit.AboveThumb
      else TrackHit.BelowThumb
    }

  /** The first row after a press at `target`: a page towards the pointer for a click in the track, and where it already
    * is for one on the thumb.
    */
  def firstRowAfterPress(extent: ScrollExtent, target: TrackHit): Int =
    target match
      case TrackHit.OnThumb(_) => extent.clamped(extent.firstRow)
      case TrackHit.AboveThumb => extent.clamped(extent.firstRow - extent.pageRows)
      case TrackHit.BelowThumb => extent.clamped(extent.firstRow + extent.pageRows)

  private def travelShare(extent: ScrollExtent): Double =
    if extent.lastFirstRow == 0 then 0.0 else extent.clamped(extent.firstRow).toDouble / extent.lastFirstRow
