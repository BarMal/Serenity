package com.serenity.ui.layout

import com.serenity.config.{AppConfig, AppMode, ConfigRegistry}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProseColumnSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def terminalState(config: AppConfig): AppState =
    val initial = AppState.initial(config.withLineNumbers(false))
    initial.copy(runtime = initial.runtime.copy(capabilities = FrontendCapabilities.tui()))

  private def textColumnWidth(state: AppState, viewportWidth: Int): Int =
    LayoutEngine.calculateLayout(state, ViewportSize(viewportWidth, 40)).editorPanelRect.width

  "A prose measure" should "hold the text column at the same number of characters however wide the window is" in {
    val state = terminalState(AppConfig.default.withAppMode(AppMode.Prose).withProseMeasure(Some(60)))

    List(100, 161, 240).map(textColumnWidth(state, _)) shouldBe List(60, 60, 60)
  }

  it should "centre the column, splitting the spare width between the two sides" in {
    val state  = terminalState(AppConfig.default.withAppMode(AppMode.Prose).withProseMeasure(Some(60)))
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 40))

    layout.leftSpacerRect.width shouldBe 20
    layout.editorPanelRect.x shouldBe 20
  }

  it should "give a window narrower than the measure its whole width" in {
    val state = terminalState(AppConfig.default.withAppMode(AppMode.Prose).withProseMeasure(Some(60)))

    textColumnWidth(state, 50) shouldBe 50
  }

  it should "leave a code workspace at full width" in {
    val state = terminalState(AppConfig.default.withAppMode(AppMode.Code).withProseMeasure(Some(60)))

    textColumnWidth(state, 100) shouldBe 100
  }

  "Grid cells for a measure" should "be the measure in the prose font's ch advance, rounded up to whole cells" in {
    ProseColumn.cells(66, chWidthPx = 9.0, gridCellWidthPx = 10) shouldBe 60
    ProseColumn.cells(66, chWidthPx = 10.0, gridCellWidthPx = 10) shouldBe 66
    ProseColumn.cells(66, chWidthPx = 12.5, gridCellWidthPx = 10) shouldBe 83
  }

  "The prose measure setting" should "read a CSS-style character count, or off" in {
    val field = ConfigRegistry.find("typography.prose.measure").getOrElse(fail("missing typography.prose.measure"))

    field.read(AppConfig.default, "66ch").map(_.surfaceConfig.proseMeasure) shouldBe Some(Some(66))
    field.read(AppConfig.default, "72").map(_.surfaceConfig.proseMeasure) shouldBe Some(Some(72))
    field.read(AppConfig.default.withProseMeasure(Some(66)), "off").map(_.surfaceConfig.proseMeasure) shouldBe
      Some(None)
    field.read(AppConfig.default, "wide") shouldBe None
    field.setting(AppConfig.default.withProseMeasure(Some(66)))._2.config.unwrapped shouldBe "66ch"
    field.setting(AppConfig.default)._2.config.unwrapped shouldBe "off"
  }
