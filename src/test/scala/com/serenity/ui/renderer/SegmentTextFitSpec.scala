package com.serenity.ui.renderer

import com.serenity.config.AppConfig
import com.serenity.testkit.TestMeasure
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.SegmentTitleFit
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: a tab title is cut by pixels at paint time, in every frame, so the advances it is cut by come from the
  * render owner's remembered ones, and a remembered advance cuts the title where a fresh measure does.
  */
class SegmentTextFitSpec extends AnyFlatSpec with Matchers:

  private val font        = FontLoader.previewUiFont(AppConfig.conventional.editorConfig.fontConfig)
  private val cellWidthPx = 9
  private val insetPx     = 4.0
  private val fit         = SegmentTitleFit(1, 6, "…")
  private val titles =
    List("README.md", "Makefile", "MMMMMMMM", "wwwwww", "Buffer 1", "a title that cannot possibly fit")

  private def cut(title: String, advances: Option[MeasuredTextAdvances]): String =
    SegmentTextFit.fit(title, fit, font, advances, cellWidthPx, insetPx)

  private def newAdvances() = MeasuredTextAdvances(MeasuredTextAdvances.DefaultCapacity, TestMeasure.layoutMemos())

  "SegmentTextFit" should "cut a title where a fresh measure of it, remembering nothing, cuts it" in {
    val advances = newAdvances()

    titles.map(cut(_, Some(advances))) shouldBe titles.map(title => cut(title, Some(newAdvances())))
  }

  it should "measure nothing a second time for the same titles" in {
    val advances = newAdvances()
    titles.foreach(cut(_, Some(advances)))
    val afterFirst = advances.measurements

    titles.foreach(cut(_, Some(advances)))

    afterFirst should be > 0L
    advances.measurements shouldBe afterFirst
  }
