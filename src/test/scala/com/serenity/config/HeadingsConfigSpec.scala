package com.serenity.config

import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.theme.{HeadingColour, HeadingSettings, HeadingStyle, RichTextStyling}
import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeadingsConfigSpec extends AnyFlatSpec with Matchers:

  private val levels = 1 to 6

  private val levelKeys =
    for
      level     <- levels.toList
      attribute <- List("size", "bold", "italic", "family", "colour", "align", "space_before", "space_after", "underline", "rule")
    yield s"headings.h$level.$attribute"

  private val globalKeys = List("headings.colour", "headings.damping", "headings.damping_threshold")

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

  it should "read the space above and below a heading in em of its own font size" in {
    val configured = read(
      AppConfig.default,
      "headings.h1.space_before" -> "1.5",
      "headings.h1.space_after"  -> "0.25",
      "headings.h3.space_after"  -> "2"
    ).headingsConfig

    configured.styles(0) shouldBe HeadingStyle.default(1).copy(spaceBeforeEm = 1.5f, spaceAfterEm = 0.25f)
    configured.styles(2) shouldBe HeadingStyle.default(3).copy(spaceAfterEm = 2.0f)
    configured.styles(1) shouldBe HeadingStyle.default(2)
  }

  it should "read underline and rule as auto, true or false" in {
    val configured = read(
      AppConfig.default,
      "headings.h1.underline" -> "true",
      "headings.h2.underline" -> "false",
      "headings.h1.rule"      -> "true",
      "headings.h3.rule"      -> "false",
      "headings.h4.underline" -> "true",
      "headings.h4.underline" -> "auto"
    ).headingsConfig

    configured.levels(0).underline shouldBe Some(true)
    configured.levels(1).underline shouldBe Some(false)
    configured.levels(3).underline shouldBe None
    configured.levels(0).rule shouldBe Some(true)
    configured.levels(2).rule shouldBe Some(false)
    configured.levels(4) shouldBe HeadingLevelConfig()
    AppConfig.default.headingsConfig.levels.map(level => (level.underline, level.rule)).distinct shouldBe
      List((None, None))
  }

  it should "underline a heading everywhere once the level says so, and leave the look alone while it is auto" in {
    val configured = read(AppConfig.default, "headings.h2.underline" -> "true").headingsConfig

    configured.styles(1) shouldBe HeadingStyle.default(2).copy(underline = true)
    configured.settings(12.0f).textStyle(2).isUnderlined shouldBe true
    configured.settings(12.0f).textStyle(1).isUnderlined shouldBe false
    AppConfig.default.headingsConfig.styles.map(_.underline).distinct shouldBe List(false)
  }

  it should "leave a graphical session's heading settings unmoved by a rule, which only a terminal draws" in {
    val configured = read(AppConfig.default, "headings.h1.rule" -> "true").headingsConfig

    configured.styles shouldBe HeadingStyle.defaults
    configured.settings(12.0f) shouldBe HeadingSettings.default
  }

  it should "name the colours the terminal's ladder uses, accent and muted" in {
    def colourOf(text: String) = read(AppConfig.default, "headings.h1.colour" -> text).headingsConfig.styles(0).colour

    colourOf("accent").map(_.toString) shouldBe Some("Accent")
    colourOf("muted").map(_.toString) shouldBe Some("Muted")
    read(AppConfig.default, "headings.colour" -> "accent").headingsConfig.colour.map(_.toString) shouldBe Some("Accent")
  }

  it should "read auto and zero as no space, leaving the look it has today" in {
    val configured = read(
      AppConfig.default,
      "headings.h2.space_before" -> "1",
      "headings.h2.space_before" -> "auto",
      "headings.h2.space_after"  -> "0"
    ).headingsConfig

    configured.styles(1).spaceBeforeEm shouldBe 0.0f
    configured.styles(1).spaceAfterEm shouldBe 0.0f
    AppConfig.default.headingsConfig.styles.map(style => (style.spaceBeforeEm, style.spaceAfterEm)).distinct shouldBe
      List((0.0f, 0.0f))
  }

  it should "carry the spacing into the settings a layout resolves headings with" in {
    val configured = read(AppConfig.default, "headings.h2.space_before" -> "1.5", "headings.h2.space_after" -> "0.5")

    configured.headingsConfig.settings(12.0f).spacing(2) shouldBe (1.5f, 0.5f)
    configured.headingsConfig.settings(12.0f).spacing(1) shouldBe (0.0f, 0.0f)
    configured.headingsConfig.settings(12.0f) should not be HeadingSettings.default
    AppConfig.default.headingsConfig.settings(12.0f) shouldBe HeadingSettings.default
  }

  it should "read a level's colour as auto, text, heading or #RRGGBB" in {
    val red = HeadingColour.Fixed(com.serenity.ui.color.RenderColor.fromRgba(0xcc, 0x33, 0x33))

    def colourOf(text: String) = read(AppConfig.default, "headings.h1.colour" -> text).headingsConfig.styles(0).colour

    colourOf("#CC3333") shouldBe Some(red)
    colourOf("text") shouldBe Some(HeadingColour.Text)
    colourOf("HEADING") shouldBe Some(HeadingColour.Heading)
    colourOf("auto") shouldBe None
    AppConfig.default.headingsConfig.styles.map(_.colour).distinct shouldBe List(None)
  }

  it should "read the global colour as auto, text or heading" in {
    def globalOf(text: String) = read(AppConfig.default, "headings.colour" -> text).headingsConfig.colour

    globalOf("text") shouldBe Some(HeadingColour.Text)
    globalOf("heading") shouldBe Some(HeadingColour.Heading)
    globalOf("auto") shouldBe None
    AppConfig.default.headingsConfig.colour shouldBe None
    ConfigRegistry.defaultFor("headings.colour").map(_.rendered) shouldBe Some("auto")
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
      "headings.h1.underline"      -> "sometimes",
      "headings.h2.rule"           -> "double",
      "headings.h4.align"          -> "diagonal",
      "headings.h1.space_before"   -> "-0.5",
      "headings.h1.space_before"   -> "9",
      "headings.h1.space_after"    -> "tall",
      "headings.h6.space_after"    -> "-1",
      "headings.h1.colour"         -> "blue",
      "headings.h2.colour"         -> "#12345",
      "headings.h3.colour"         -> "#GGGGGG",
      "headings.colour"            -> "#CC3333",
      "headings.colour"            -> "blue",
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
          HeadingLevelConfig(
            italic = Some(true),
            align = Some(ParagraphAlignment.Right),
            spaceBefore = Some(2.0f)
          ),
          HeadingLevelConfig(
            family = Some("Charter"),
            colour = Some(HeadingColour.Fixed(com.serenity.ui.color.RenderColor.fromRgba(1, 2, 3)))
          ),
          HeadingLevelConfig(),
          HeadingLevelConfig(align = Some(ParagraphAlignment.Justify)),
          HeadingLevelConfig(size = Some(1.5f), spaceBefore = Some(0.75f), spaceAfter = Some(1.25f))
        ),
        colour = Some(HeadingColour.Heading),
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
