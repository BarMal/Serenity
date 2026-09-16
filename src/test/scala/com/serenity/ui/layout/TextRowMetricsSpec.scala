package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextRowMetricsSpec extends AnyFlatSpec with Matchers:

  private val pixelGrid = CellMetrics(charWidth = 8, lineHeight = 1, ascent = 1)
  private val rect      = LayoutRect(0, 0, 100, 100)

  "Measured TextRowMetrics with per-line heights" should "stack rows by their own heights" in {
    val metrics = TextRowMetrics(
      rect,
      pixelGrid,
      rowLineHeightPx = 20,
      usesMeasuredLayout = true,
      rowHeightsPx = Vector(20, 40, 20)
    )
    metrics.lineTopPx(0) shouldBe 0
    metrics.lineTopPx(1) shouldBe 20
    metrics.lineTopPx(2) shouldBe 60
    metrics.rowHeightPx(1) shouldBe 40
  }

  it should "fall back to the uniform height for a row past the per-line table" in {
    val metrics =
      TextRowMetrics(rect, pixelGrid, rowLineHeightPx = 20, usesMeasuredLayout = true, rowHeightsPx = Vector(30))
    metrics.lineTopPx(0) shouldBe 0
    metrics.lineTopPx(1) shouldBe 30
    metrics.lineTopPx(2) shouldBe 50
  }

  it should "reproduce uniform stacking when no per-line heights are given" in {
    val metrics = TextRowMetrics(rect, pixelGrid, rowLineHeightPx = 16, usesMeasuredLayout = true)
    metrics.lineTopPx(0) shouldBe 0
    metrics.lineTopPx(1) shouldBe 16
    metrics.lineTopPx(3) shouldBe 48
    metrics.rowHeightPx(2) shouldBe 16
  }

  "visualRowAt" should "reverse a pixel Y to the visual row that owns it, honoring per-line heights" in {
    val metrics = TextRowMetrics(
      rect,
      pixelGrid,
      rowLineHeightPx = 20,
      usesMeasuredLayout = true,
      rowHeightsPx = Vector(20, 40, 20)
    )
    metrics.visualRowAt(0) shouldBe 0
    metrics.visualRowAt(19) shouldBe 0
    metrics.visualRowAt(20) shouldBe 1
    metrics.visualRowAt(59) shouldBe 1 // row 1 spans [20, 60)
    metrics.visualRowAt(60) shouldBe 2
    metrics.visualRowAt(1000) shouldBe 2 // below the last row clamps to it
    metrics.visualRowAt(-5) shouldBe 0
  }

  it should "divide uniformly for a measured layout with no per-line table" in {
    val metrics = TextRowMetrics(rect, pixelGrid, rowLineHeightPx = 16, usesMeasuredLayout = true)
    metrics.visualRowAt(0) shouldBe 0
    metrics.visualRowAt(33) shouldBe 2
  }
