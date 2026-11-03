package com.serenity.ui.renderer

import java.awt.Color
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import com.serenity.ui.layout.PixelRect

/** What a [[Java2DRenderSurface]] knows about the pixels it has painted since it last cleared the viewport, so a run
  * can leave out a background fill that would repeat the clear.
  *
  * Everything is in the logical pixels the clears and runs are expressed in. Anything painted under a translation is
  * moved back to those before it is compared, and anything the tracking cannot place -- a pixel copy, a layer composite
  * -- forgets the clear altogether, which only ever costs a redundant fill, never a missing one.
  *
  * Every draw reports here and every run asks here, many times a frame, so the calls take plain ints and allocate
  * nothing. A surface is painted by one thread at a time; the edges of the last run and of everything else painted are
  * contained scratch arrays for that reason.
  */
final private[ui] class PaintedPixels:
  import PaintedPixels.*

  private val clean       = AtomicReference[Option[CleanBackground]](None)
  private val translation = AtomicReference(Translation.Zero)
  private val translucent = AtomicBoolean(false)
  private val lastRun     = new Array[Int](4)
  private val touchedBox  = new Array[Int](4)

  def setTranslucent(isTranslucent: Boolean): Unit = translucent.set(isTranslucent)

  def withTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit =
    val outer = translation.get()
    translation.set(Translation(outer.xPx + xPx, outer.yPx + yPx))
    try render
    finally translation.set(outer)

  /** The viewport was cleared to `color` everywhere in `cleared`. A clear that was itself moved, clipped or blended is
    * not a clean slate.
    */
  def cleared(color: Color, cleared: scala.collection.immutable.List[PixelRect], clipped: Boolean): Unit =
    val everythingCleared = translation.get().isZero && !translucent.get() && !clipped
    clean.set(if everythingCleared then CleanBackground.cleared(color, cleared) else None)
    reset(lastRun)
    reset(touchedBox)

  /** Something other than a run painted `xPx, yPx, widthPx, heightPx` (in the current translation). */
  def touched(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int): Unit =
    val moved  = translation.get()
    val left   = moved.left(xPx)
    val top    = moved.top(yPx)
    val right  = moved.right(xPx + widthPx)
    val bottom = moved.bottom(yPx + heightPx)
    if touchedBox(0) >= touchedBox(2) then store(touchedBox, left, top, right, bottom)
    else
      store(
        touchedBox,
        math.min(touchedBox(0), left),
        math.min(touchedBox(1), top),
        math.max(touchedBox(2), right),
        math.max(touchedBox(3), bottom)
      )

  def forgetClear(): Unit = clean.set(None)

  /** A run's fill over `left, top, right, bottom` (in the current translation) is redundant only over pixels still
    * holding the cleared colour: no other draw, nor the run painted just before (whose edge pixels a fractional
    * position can share), has reached them, and nothing translucent is in force to make a second fill differ from the
    * first.
    */
  def needsBackground(left: Int, top: Int, right: Int, bottom: Int, background: Color): Boolean =
    val moved = translation.get()
    val l     = moved.left(left)
    val t     = moved.top(top)
    val r     = moved.right(right)
    val b     = moved.bottom(bottom)
    translucent.get() || overlaps(lastRun, l, t, r, b) || overlaps(touchedBox, l, t, r, b) ||
    (clean.get() match
      case Some(untouched) => !untouched.covers(l, t, r, b, background)
      case None            => true)

  def ranOver(left: Int, top: Int, right: Int, bottom: Int): Unit =
    val moved = translation.get()
    store(lastRun, moved.left(left), moved.top(top), moved.right(right), moved.bottom(bottom))

  private def reset(edges: Array[Int]): Unit = store(edges, 0, 0, 0, 0)

  private def store(edges: Array[Int], left: Int, top: Int, right: Int, bottom: Int): Unit =
    edges(0) = left
    edges(1) = top
    edges(2) = right
    edges(3) = bottom

  private def overlaps(edges: Array[Int], left: Int, top: Int, right: Int, bottom: Int): Boolean =
    edges(0) < right && left < edges(2) && edges(1) < bottom && top < edges(3)

object PaintedPixels:

  /** How far the graphics has been moved from the origin the clears and runs are expressed against. */
  final private case class Translation(xPx: Double, yPx: Double):
    def isZero: Boolean = xPx == 0.0 && yPx == 0.0

    def left(edge: Int): Int   = if isZero then edge else math.floor(edge + xPx).toInt
    def top(edge: Int): Int    = if isZero then edge else math.floor(edge + yPx).toInt
    def right(edge: Int): Int  = if isZero then edge else math.ceil(edge + xPx).toInt
    def bottom(edge: Int): Int = if isZero then edge else math.ceil(edge + yPx).toInt

  private object Translation:
    val Zero: Translation = Translation(0.0, 0.0)
