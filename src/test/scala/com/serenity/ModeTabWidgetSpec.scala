package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The mode + tab-list corner widget's placement is configurable (issue #1307) -- it doesn't have to share
  * `CursorInfoBarPlacement`'s floating/pinned-bottom choice, since it's addressed to a specific window corner.
  */
class ModeTabWidgetSpec extends AnyFlatSpec with Matchers:

  "CornerPosition.fromConfigKey" should "parse the known spellings" in {
    CornerPosition.fromConfigKey("top-left") shouldBe Some(CornerPosition.TopLeft)
    CornerPosition.fromConfigKey("top-right") shouldBe Some(CornerPosition.TopRight)
    CornerPosition.fromConfigKey("bottom-left") shouldBe Some(CornerPosition.BottomLeft)
    CornerPosition.fromConfigKey("bottom-right") shouldBe Some(CornerPosition.BottomRight)
    CornerPosition.fromConfigKey("BOTTOM-RIGHT") shouldBe Some(CornerPosition.BottomRight)
    CornerPosition.fromConfigKey("nonsense") shouldBe None
  }

  "AppConfig.default" should "place the mode/tab widget in the bottom-right corner" in {
    AppConfig.default.modeTabWidgetCornerPosition shouldBe CornerPosition.BottomRight
  }

  "AppConfig.withModeTabWidgetCornerPosition" should "change only the widget's corner" in {
    val moved = AppConfig.default.withModeTabWidgetCornerPosition(CornerPosition.TopLeft)

    moved.modeTabWidgetCornerPosition shouldBe CornerPosition.TopLeft
  }

  "ConfigRegistry" should "read and validate the widget corner position setting" in {
    ConfigRegistry
      .read(AppConfig.default, "widget.mode_tab_corner", "top-left")
      .getOrElse(fail("widget.mode_tab_corner parse"))
      .modeTabWidgetCornerPosition shouldBe CornerPosition.TopLeft

    ConfigRegistry.rejects("widget.mode_tab_corner", "top-left") shouldBe false
    ConfigRegistry.rejects("widget.mode_tab_corner", "unknown") shouldBe true
  }

  "ConfigKeySchema" should "know the widget corner position key" in {
    ConfigKeySchema.isKnownKey("widget.mode_tab_corner") shouldBe true
  }
