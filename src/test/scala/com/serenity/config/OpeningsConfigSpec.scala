package com.serenity.config

import com.serenity.ui.openings.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpeningsConfigSpec extends AnyFlatSpec with Matchers:

  private val keys = List(
    "openings.enabled",
    "openings.letter",
    "openings.raised_row",
    "openings.hybrid_sink_lines",
    "openings.small_caps",
    "openings.epigraph",
    "openings.epigraph_marker",
    "openings.scene_break",
    "openings.hand_drop_cap"
  )

  private def read(settings: (String, String)*): OpeningSettings =
    settings
      .foldLeft(AppConfig.default) {
        case (acc, (key, value)) =>
          ConfigRegistry.read(acc, key, value).getOrElse(fail(s"$key = $value was refused"))
      }
      .openingsConfig

  "the opening settings" should "be registered under openings, and written to the config file" in {
    val written = ConfigFileFormat.settings(AppConfig.default).map(_._1)

    keys.foreach { key =>
      withClue(key) {
        ConfigRegistry.find(key) should not be empty
        written should contain(key)
      }
    }
    ConfigFileFormat.missingFromLayout shouldBe Nil
    ConfigRegistry.writtenKeys.filter(_.startsWith("openings.")) shouldBe keys
  }

  it should "ship with openings off and every other setting at its plainest" in {
    val defaults = keys.map(key => key -> ConfigRegistry.defaultFor(key).map(_.rendered)).toMap

    defaults shouldBe Map(
      "openings.enabled"           -> Some("false"),
      "openings.letter"            -> Some("drop"),
      "openings.raised_row"        -> Some("grow"),
      "openings.hybrid_sink_lines" -> Some("2"),
      "openings.small_caps"        -> Some("false"),
      "openings.epigraph"          -> Some("auto"),
      "openings.epigraph_marker"   -> Some("epigraph"),
      "openings.scene_break"       -> Some("none"),
      "openings.hand_drop_cap"     -> Some("hand")
    )
    AppConfig.default.openingsConfig shouldBe OpeningSettings.default
    AppConfig.default.openingsConfig.enabled shouldBe false
  }

  it should "read every letter mode, raised-row behaviour and rule" in {
    read("openings.letter" -> "raised").letterMode shouldBe LetterMode.Raised
    read("openings.letter" -> "hybrid").letterMode shouldBe LetterMode.Hybrid
    read("openings.letter" -> "drop").letterMode shouldBe LetterMode.Drop
    read("openings.raised_row" -> "clamp").raisedRow shouldBe RaisedRowRule.Clamp
    read("openings.raised_row" -> "grow").raisedRow shouldBe RaisedRowRule.Grow
    read("openings.epigraph" -> "never").rules.epigraph shouldBe EpigraphRule.Never
    read("openings.epigraph" -> "marker").rules.epigraph shouldBe EpigraphRule.ExplicitOnly
    read("openings.epigraph" -> "auto").rules.epigraph shouldBe EpigraphRule.AutoDetect
    read("openings.scene_break" -> "small-caps").rules.sceneBreak shouldBe SceneBreakRule.SmallCapsOnly
    read("openings.scene_break" -> "full").rules.sceneBreak shouldBe SceneBreakRule.FullOpening
    read("openings.scene_break" -> "none").rules.sceneBreak shouldBe SceneBreakRule.NoOpening
    read("openings.hand_drop_cap" -> "hand-small-caps").rules.handDropCap shouldBe HandDropCapRule.HandPlusSmallCaps
    read("openings.hand_drop_cap" -> "opening").rules.handDropCap shouldBe HandDropCapRule.OpeningWins
    read("openings.hand_drop_cap" -> "hand").rules.handDropCap shouldBe HandDropCapRule.HandWins
  }

  it should "read the toggles, the sink lines and the marker text" in {
    val configured = read(
      "openings.enabled"           -> "true",
      "openings.small_caps"        -> "true",
      "openings.hybrid_sink_lines" -> "4",
      "openings.epigraph_marker"   -> "motto"
    )

    configured.enabled shouldBe true
    configured.smallCapsFirstLine shouldBe true
    configured.hybridSinkLines shouldBe 4
    configured.epigraphMarker shouldBe "motto"
  }

  it should "leave the other settings alone when one is read" in {
    read("openings.scene_break" -> "full") shouldBe OpeningSettings.default.copy(rules =
      OpeningRules.default.copy(sceneBreak = SceneBreakRule.FullOpening)
    )
  }

  it should "refuse a value that names nothing" in
    List(
      "openings.letter"            -> "sideways",
      "openings.raised_row"        -> "shrink",
      "openings.epigraph"          -> "sometimes",
      "openings.scene_break"       -> "ornate",
      "openings.hand_drop_cap"     -> "both",
      "openings.hybrid_sink_lines" -> "1",
      "openings.hybrid_sink_lines" -> "99",
      "openings.hybrid_sink_lines" -> "many",
      "openings.epigraph_marker"   -> "",
      "openings.enabled"           -> "maybe"
    ).foreach { case (key, value) => withClue(s"$key = $value")(ConfigRegistry.rejects(key, value) shouldBe true) }

  it should "survive being written to the config file and read back" in {
    val configured = AppConfig.default.copy(openingsConfig =
      OpeningSettings(
        enabled = true,
        letterMode = LetterMode.Hybrid,
        raisedRow = RaisedRowRule.Clamp,
        hybridSinkLines = 5,
        smallCapsFirstLine = true,
        epigraphMarker = "motto",
        rules = OpeningRules(EpigraphRule.ExplicitOnly, SceneBreakRule.SmallCapsOnly, HandDropCapRule.OpeningWins)
      )
    )
    val rendered = ConfigFileFormat.settings(configured).toMap

    val reread = keys.foldLeft(AppConfig.default) { (acc, key) =>
      ConfigRegistry.read(acc, key, rendered(key).rendered).getOrElse(fail(s"$key was refused"))
    }

    reread.openingsConfig shouldBe configured.openingsConfig
  }
