package com.serenity

import java.awt.Font

import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CellMetricsSpec extends AnyFlatSpec with Matchers:

  "CellMetrics.fromFont" should "produce charWidth > 0 and lineHeight > 0 for a known system font" in {
    val metrics = CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 12))

    metrics.charWidth should be > 0
    metrics.lineHeight should be > 0
    metrics.ascent should be > 0
  }

  it should "clamp charWidth to at least 1" in {
    val metrics = CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 12))

    metrics.charWidth should be >= 1
    metrics.lineHeight should be >= 1
    metrics.ascent should be >= 1
  }

  "CellMetrics.isValid" should "return true for normal metrics" in {
    CellMetrics(8, 16, 13).isValid shouldBe true
  }

  it should "return false when charWidth is zero" in {
    CellMetrics(0, 16, 13).isValid shouldBe false
  }

  it should "return false when lineHeight is zero" in {
    CellMetrics(8, 0, 13).isValid shouldBe false
  }

  "CellMetrics.toCol" should "not throw ArithmeticException when charWidth is zero" in {
    val metrics = CellMetrics(0, 16, 13)
    noException should be thrownBy metrics.toCol(100)
    metrics.toCol(100) shouldBe 0
  }

  "CellMetrics.toRow" should "not throw ArithmeticException when lineHeight is zero" in {
    val metrics = CellMetrics(8, 0, 13)
    noException should be thrownBy metrics.toRow(100)
    metrics.toRow(100) shouldBe 0
  }

  "CellMetrics.viewportSize" should "not throw when charWidth or lineHeight is zero" in {
    val metrics = CellMetrics(0, 0, 0)
    noException should be thrownBy metrics.viewportSize(800, 600)
    val vp = metrics.viewportSize(800, 600)
    vp.width should be >= 1
    vp.height should be >= 1
  }

  it should "compute correct viewport for valid metrics" in {
    val metrics = CellMetrics(8, 16, 13)
    metrics.viewportSize(800, 640) shouldBe ViewportSize(100, 40)
  }

  it should "convert fractional logical rows without applying device scale" in {
    val metrics = CellMetrics(8, 16, 13)

    metrics.toPixelY(0.25) shouldBe 4.0
    metrics.toPixelX(1.5) shouldBe 12.0
  }
