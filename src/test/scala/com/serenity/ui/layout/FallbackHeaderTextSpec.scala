package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the fallback header's one row says at each width: the message, then what dismisses it, cut from the message's
  * end so the hint stays while it can.
  */
class FallbackHeaderTextSpec extends AnyFlatSpec with Matchers:

  private val hint    = FallbackHeaderText.Hint
  private val message = "Skia could not load: no raster surface. Running in the terminal."

  "FallbackHeaderText.fit" should "say the message and then the hint when both fit" in {
    FallbackHeaderText.fit(message, hint, 80) shouldBe s"$message  $hint"
  }

  it should "fit exactly at the width the two need" in {
    val needed = message.length + 2 + hint.length
    FallbackHeaderText.fit(message, hint, needed) shouldBe s"$message  $hint"
  }

  it should "cut the message with an ellipsis, keeping the hint, when one cell is missing" in {
    val needed = message.length + 2 + hint.length
    val fitted = FallbackHeaderText.fit(message, hint, needed - 1)

    fitted should have length (needed - 1)
    fitted should endWith(s"…  $hint")
    fitted should startWith(message.take(message.length - 2))
  }

  it should "drop the hint first when the width is too narrow for both" in {
    val width  = hint.length + 3
    val fitted = FallbackHeaderText.fit(message, hint, width)

    fitted should have length width
    fitted should endWith("…")
    fitted should not include hint
  }

  it should "never be wider than the row, however narrow" in {
    for width <- 0 to 100 do FallbackHeaderText.fit(message, hint, width).length should be <= width
  }

  it should "be empty for a row with no width" in {
    FallbackHeaderText.fit(message, hint, 0) shouldBe ""
    FallbackHeaderText.fit(message, hint, -3) shouldBe ""
  }

  it should "leave a short message whole, and give a hint that fits after it" in {
    FallbackHeaderText.fit("Short.", hint, 40) shouldBe s"Short.  $hint"
    FallbackHeaderText.fit("Short.", hint, 6) shouldBe "Short."
  }

  "FallbackHeaderText.Hint" should "name Escape as what dismisses the header" in {
    hint should include("Esc")
  }

end FallbackHeaderTextSpec
