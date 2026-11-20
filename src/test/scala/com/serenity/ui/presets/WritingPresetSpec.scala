package com.serenity.ui.presets

import com.serenity.config.{AppConfig, Gutter, TextAreaInsets, WrapColumn}
import com.serenity.ui.layout.TextArea
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Prose reads best in a column with room around it; the Writing preset asks for one, and no other preset changes. */
class WritingPresetSpec extends AnyFlatSpec with Matchers:

  private val writing = BuiltInUiPresets.builtIn("Writing").map(_.config.surfaceConfig).getOrElse(fail("no Writing"))

  "The Writing preset" should "wrap at 66 characters of the prose font" in {
    writing.wrapColumn shouldBe WrapColumn.Ch(66)
  }

  it should "leave 4% of the height above and below, and two cells at each side" in {
    writing.textAreaInsets shouldBe TextAreaInsets(
      left = Gutter.Cells(2),
      right = Gutter.Cells(2),
      top = Gutter.Percent(0.04),
      bottom = Gutter.Percent(0.04)
    )
  }

  "Every other built-in preset" should "keep the edge-to-edge defaults" in {
    val conventional = AppConfig.conventional.surfaceConfig
    BuiltInUiPresets.builtIns.filterNot(_.name == "Writing").foreach { preset =>
      withClue(s"${preset.name}: ") {
        preset.config.surfaceConfig.wrapColumn shouldBe conventional.wrapColumn
        preset.config.surfaceConfig.textAreaInsets shouldBe conventional.textAreaInsets
      }
    }
  }

  "A bare number in the text area settings" should "be a percentage, so 4 is 4% and 0.04 is not" in {
    Gutter.parse("4", allowAuto = false) shouldBe Some(Gutter.Percent(0.04))
    Gutter.parse("0.04", allowAuto = false) shouldBe Some(Gutter.Percent(0.0004))
    Gutter.parse("2cells", allowAuto = true) shouldBe Some(Gutter.Cells(2))
  }

  "The Writing text area" should "be centred on a wide window and keep its side padding on a narrow one" in {
    val insets = writing.textAreaInsets
    val column = Some(66)

    val wide = TextArea.horizontal(200, insets, 0, 0, column)
    (wide.leftSpacer, wide.rightSpacer, wide.width) shouldBe (67, 67, 66)

    val narrow = TextArea.horizontal(60, insets, 0, 0, column)
    (narrow.leftSpacer, narrow.rightSpacer, narrow.width) shouldBe (2, 2, 56)
  }
