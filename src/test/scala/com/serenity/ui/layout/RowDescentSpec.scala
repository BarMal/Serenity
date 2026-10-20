package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, LineFontResolver}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RowDescentSpec extends AnyFlatSpec with Matchers:

  private val frc = FontRenderContext(AffineTransform(), true, true)

  // Off-grid sizes keep these fonts distinct from the ones the other specs probe.
  private def font(family: String, style: Int, size: Float): Font = Font(family, style, 1).deriveFont(size + 0.25f)

  private val extents = FontLineExtents()

  private val heading = font(Font.SERIF, Font.BOLD, 28f)
  private val body    = font(Font.MONOSPACED, Font.PLAIN, 12f)

  private def rowOf(fonts: Vector[Font]): (Int, Int) =
    val runs = fonts.zipWithIndex.map((f, index) => ColumnFontRun(index * 10, index * 10 + 10, f))
    LineFontResolver(fonts.head, runs).lineMetrics(frc, extents, 0, fonts.length * 10)

  private def assertRoomForDescenders(fonts: Vector[Font]): Unit =
    val metrics              = fonts.map(_.getLineMetrics("Mgyjpq", frc))
    val maxAscent            = metrics.map(_.getAscent).max
    val maxDescent           = metrics.map(_.getDescent).max
    val (heightPx, ascentPx) = rowOf(fonts)
    withClue(
      s"fonts=${fonts.map(f => s"${f.getName}/${f.getStyle}/${f.getSize2D}")} height=$heightPx ascent=$ascentPx: "
    ) {
      (heightPx - ascentPx).toFloat should be >= maxDescent
      heightPx.toFloat should be >= maxAscent + maxDescent
    }

  "a row mixing a large bold heading run with body runs" should "leave room below the baseline for the deepest descent" in
    assertRoomForDescenders(Vector(heading, body))

  it should "leave room however the runs are ordered" in
    assertRoomForDescenders(Vector(body, heading, font(Font.SANS_SERIF, Font.ITALIC, 14f)))

  "a row of a single font" should "leave room below the baseline for its descent at every size" in {
    for
      family <- List(Font.SERIF, Font.SANS_SERIF, Font.MONOSPACED)
      style  <- List(Font.PLAIN, Font.BOLD)
      size   <- (8 to 40).map(_.toFloat)
    do assertRoomForDescenders(Vector(font(family, style, size)))
  }

  "a row of mixed fonts" should "leave room below the baseline for the deepest descent at every size pair" in {
    val sizes = List(10f, 12f, 14f, 16f, 21f, 28f)
    for
      first  <- sizes.flatMap(size => List(font(Font.SERIF, Font.BOLD, size), font(Font.SANS_SERIF, Font.PLAIN, size)))
      second <- sizes.map(size => font(Font.MONOSPACED, Font.PLAIN, size))
    do assertRoomForDescenders(Vector(first, second))
  }
