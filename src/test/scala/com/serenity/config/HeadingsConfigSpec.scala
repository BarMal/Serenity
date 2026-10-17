package com.serenity.config

import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.theme.{HeadingStyle, RichTextStyling}
import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeadingsConfigSpec extends AnyFlatSpec with Matchers:

  private val levels = 1 to 6

  private val levelKeys =
    for
      level     <- levels.toList
      attribute <- List("size", "bold", "italic", "family", "colour", "align")
    yield s"headings.h$level.$attribute"

  private val globalKeys = List("headings.damping", "headings.damping_threshold")

  private def read(config: AppConfig, settings: (String, String)*): AppConfig =
    settings.foldLeft(config) {
      case (acc, (key, value)) =>
        ConfigRegistry.read(acc, key, value).getOrElse(fail(s"$key = $value was refused"))
    }

  "the heading settings" should "be registered under headings, and written to the config file" in {
    val written = ConfigFileFormat.settings(AppConfig.default).map(_._1)

    (levelKeys ++ globalKeys).foreach { key =>
      withClue(key) {
        ConfigRegistry.find(key) should not be empty
        written should contain(key)
      }
    }
    ConfigFileFormat.missingFromLayout shouldBe Nil
  }

  it should "default every level to auto, with damping on above a 24pt body" in {
    levelKeys.foreach(key => withClue(key)(ConfigRegistry.defaultFor(key).map(_.rendered) shouldBe Some("auto")))
    ConfigRegistry.defaultFor("headings.damping").map(_.rendered) shouldBe Some("true")
    ConfigRegistry.defaultFor("headings.damping_threshold").map(_.rendered) shouldBe Some("24.0")
    AppConfig.default.headingsConfig shouldBe HeadingsConfig()
  }

  it should "resolve to today's heading styles while everything is auto" in {
    AppConfig.default.headingsConfig.styles shouldBe HeadingStyle.defaults
  }

  it should "read a size as a multiple of the body, a flag, a family and an alignment" in {
    val configured = read(
      AppConfig.default,
      "headings.h1.size"   -> "2.5",
      "headings.h1.bold"   -> "false",
      "headings.h2.italic" -> "true",
      "headings.h3.family" -> "Charter",
      "headings.h4.align"  -> "centre"
    ).headingsConfig

    configured.styles(0) shouldBe HeadingStyle.default(1).copy(sizeScale = 2.5f, bold = false)
    configured.styles(1) shouldBe HeadingStyle.default(2).copy(italic = true)
    configured.styles(2) shouldBe HeadingStyle.default(3).copy(family = Some("Charter"))
    configured.styles(3) shouldBe HeadingStyle.default(4).copy(align = Some(ParagraphAlignment.Center))
    configured.styles.drop(4) shouldBe HeadingStyle.defaults.drop(4)
  }

  it should "read a colour as #RRGGBB, and auto or heading as the theme's heading colour" in {
    val red = com.serenity.ui.color.RenderColor.fromRgba(0xcc, 0x33, 0x33)

    read(AppConfig.default, "headings.h1.colour" -> "#CC3333").headingsConfig.styles(0).colour shouldBe Some(red)
    List("auto", "heading", "HEADING").foreach { text =>
      withClue(text) {
        read(AppConfig.default, "headings.h1.colour" -> "#CC3333", "headings.h1.colour" -> text).headingsConfig shouldBe
          HeadingsConfig()
      }
    }
    AppConfig.default.headingsConfig.styles.map(_.colour).distinct shouldBe List(None)
  }

  it should "read auto as the level's own default" in {
    val configured = read(
      AppConfig.default,
      "headings.h1.size" -> "3",
      "headings.h1.size" -> "auto",
      "headings.h2.bold" -> "false",
      "headings.h2.bold" -> "auto"
    )

    configured.headingsConfig shouldBe HeadingsConfig()
  }

  it should "name every alignment, taking centre and center alike" in {
    val expected = Map(
      "left"    -> ParagraphAlignment.Left,
      "centre"  -> ParagraphAlignment.Center,
      "center"  -> ParagraphAlignment.Center,
      "right"   -> ParagraphAlignment.Right,
      "justify" -> ParagraphAlignment.Justify
    )

    expected.foreach {
      case (text, alignment) =>
        withClue(text)(
          read(AppConfig.default, "headings.h1.align" -> text).headingsConfig.styles(0).align shouldBe
            Some(alignment)
        )
    }
  }

  it should "read the damping switch and its threshold" in {
    val configured = read(AppConfig.default, "headings.damping" -> "false", "headings.damping_threshold" -> "30")

    configured.headingsConfig.damping shouldBe false
    configured.headingsConfig.dampingThresholdPt shouldBe 30.0f
  }

  it should "refuse a value it cannot honour" in
    List(
      "headings.h1.size"           -> "0.5",
      "headings.h1.size"           -> "-2",
      "headings.h1.size"           -> "40",
      "headings.h1.size"           -> "huge",
      "headings.h2.bold"           -> "maybe",
      "headings.h3.italic"         -> "2",
      "headings.h4.align"          -> "diagonal",
      "headings.h1.colour"         -> "blue",
      "headings.h2.colour"         -> "#12345",
      "headings.h3.colour"         -> "#GGGGGG",
      "headings.damping"           -> "sometimes",
      "headings.damping_threshold" -> "0",
      "headings.damping_threshold" -> "-3",
      "headings.damping_threshold" -> "5000",
      "headings.damping_threshold" -> "auto"
    ).foreach((key, value) => withClue(s"$key = $value")(ConfigRegistry.rejects(key, value) shouldBe true))

  it should "round-trip through the rendered config file" in {
    val configured = AppConfig.default.withHeadingsConfig(
      HeadingsConfig(
        levels = Vector(
          HeadingLevelConfig(size = Some(2.25f), bold = Some(false)),
          HeadingLevelConfig(italic = Some(true), align = Some(ParagraphAlignment.Right)),
          HeadingLevelConfig(
            family = Some("Charter"),
            colour = Some(com.serenity.ui.color.RenderColor.fromRgba(1, 2, 3))
          ),
          HeadingLevelConfig(),
          HeadingLevelConfig(align = Some(ParagraphAlignment.Justify)),
          HeadingLevelConfig(size = Some(1.5f))
        ),
        damping = false,
        dampingThresholdPt = 36.0f
      )
    )
    val parsed = ConfigFactory.parseString(ConfigFileFormat.render(configured))

    val restored = ConfigRegistry.fields
      .filter(field => field.key.startsWith("headings."))
      .foldLeft(AppConfig.default)((acc, field) =>
        field.readValue(acc, parsed.getValue(field.key)).getOrElse(fail(s"${field.key} did not read back"))
      )

    restored.headingsConfig shouldBe configured.headingsConfig
  }

  "the logical body size" should "be the prose size times a manual multiplier, and never a device scale" in {
    import com.serenity.ui.fonts.FontLoader.{FontConfig, TextScaleMode}

    def logical(config: FontConfig): Float = config.logicalTextFontSize

    logical(FontConfig(textFontSize = 14.0f, textScaleMode = TextScaleMode.Auto, textScaleMultiplier = 2.0)) shouldBe
      14.0f
    logical(FontConfig(textFontSize = 14.0f, textScaleMode = TextScaleMode.Off, textScaleMultiplier = 2.0)) shouldBe
      14.0f
    logical(FontConfig(textFontSize = 14.0f, textScaleMode = TextScaleMode.Manual, textScaleMultiplier = 2.0)) shouldBe
      28.0f
    RichTextStyling.ProseZoomBaselinePx shouldBe 12.0f
  }
