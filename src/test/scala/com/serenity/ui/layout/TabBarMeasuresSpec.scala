package com.serenity.ui.layout

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: the tab strip's layout settings need the code font's cell width, which is measured on an image, so a
  * frame that lays the strip out every time must not measure it every time. The remembered width is the one an uncached
  * call measures.
  */
class TabBarMeasuresSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val config = AppConfig.conventional

  "TabBarMeasures" should "measure the cell width once for the same font settings" in {
    val measures = new TabBarMeasures

    val first  = measures.cellWidthPx(config)
    val second = measures.cellWidthPx(config.withUiCornerRadiusPx(4))

    second shouldBe first
    measures.measurements shouldBe 1L
  }

  it should "measure again when the font settings change" in {
    val measures = new TabBarMeasures
    val larger = config.withFontConfig(
      config.editorConfig.fontConfig.copy(fontSize = config.editorConfig.fontConfig.fontSize + 6.0f)
    )

    measures.cellWidthPx(config)
    measures.cellWidthPx(larger)

    measures.measurements shouldBe 2L
  }

  "TabBarLayoutSettings" should "give the same settings from a remembering owner as from a fresh one" in {
    val state    = AppState.initial(config)
    val measures = new TabBarMeasures

    val remembered = (1 to 3).map(_ => TabBarLayoutSettings.of(state, measures = measures))
    val fresh      = TabBarLayoutSettings.of(state)

    remembered.map(_.edgeInsetCells).distinct shouldBe List(fresh.edgeInsetCells)
    remembered.map(_.maxTabWidth).distinct shouldBe List(fresh.maxTabWidth)
    measures.measurements shouldBe 1L
  }

  private val wideTitles = List("README.md", "Makefile", "MMMMMMMM", "wwwwww", "Buffer 1", "i")

  it should "measure each title once across layout passes for the same font settings" in {
    val state    = AppState.initial(config)
    val measures = new TabBarMeasures

    val first  = wideTitles.map(title => TabBarLayoutSettings.of(state, measures = measures).measure.cells(title))
    val second = wideTitles.map(title => TabBarLayoutSettings.of(state, measures = measures).measure.cells(title))

    second shouldBe first
    measures.titleMeasurements shouldBe wideTitles.size.toLong
  }

  it should "count a title in the cells a fresh, uncached measure counts it in" in {
    val state    = AppState.initial(config)
    val measures = new TabBarMeasures
    val fresh    = TabBarLayoutSettings.of(state)

    val remembered = TabBarLayoutSettings.of(state, measures = measures)

    wideTitles.map(remembered.measure.cells) shouldBe wideTitles.map(fresh.measure.cells)
    wideTitles.map(remembered.measure.fit(_, 6)) shouldBe wideTitles.map(fresh.measure.fit(_, 6))
  }

  it should "measure the titles again when the font settings change" in {
    val measures = new TabBarMeasures
    val larger = config.withFontConfig(
      config.editorConfig.fontConfig.copy(fontSize = config.editorConfig.fontConfig.fontSize + 6.0f)
    )

    wideTitles.foreach(title => measures.titleAdvancePx(title, config.editorConfig.fontConfig))
    wideTitles.foreach(title => measures.titleAdvancePx(title, larger.editorConfig.fontConfig))

    measures.titleMeasurements shouldBe 2L * wideTitles.size
  }

  it should "keep a bounded number of title advances" in {
    val measures = new TabBarMeasures
    val fontCfg  = config.editorConfig.fontConfig
    val distinct = (0 until TabBarMeasures.TitleCapacity + 10).map(index => s"Title $index")

    distinct.foreach(title => measures.titleAdvancePx(title, fontCfg))
    val before = measures.titleMeasurements
    measures.titleAdvancePx(distinct.head, fontCfg)

    measures.titleMeasurements shouldBe before + 1L
  }
