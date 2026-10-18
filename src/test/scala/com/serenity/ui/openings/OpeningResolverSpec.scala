package com.serenity.ui.openings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpeningResolverSpec extends AnyFlatSpec with Matchers:

  private val global = OpeningSettings(
    enabled = true,
    letterMode = LetterMode.Raised,
    raisedRow = RaisedRowRule.Clamp,
    hybridSinkLines = 4,
    smallCapsFirstLine = true,
    epigraphMarker = "epigraph",
    rules = OpeningRules(EpigraphRule.Never, SceneBreakRule.SmallCapsOnly, HandDropCapRule.HandPlusSmallCaps)
  )

  "The default settings" should "leave openings off, with the rules detection starts from" in {
    OpeningSettings.default.enabled shouldBe false
    OpeningSettings.default.rules shouldBe OpeningRules.default
  }

  "An empty override" should "resolve to the global settings themselves" in {
    OpeningOverride.Empty.isEmpty shouldBe true
    OpeningResolver.resolve(global, OpeningOverride.Empty) shouldBe theSameInstanceAs(global)
  }

  "An override" should "replace only the fields it sets" in {
    val resolved = OpeningResolver.resolve(global, OpeningOverride(letterMode = Some(LetterMode.Hybrid)))

    resolved shouldBe global.copy(letterMode = LetterMode.Hybrid)
  }

  it should "be able to set every setting" in {
    val everything = OpeningOverride(
      enabled = Some(false),
      letterMode = Some(LetterMode.Drop),
      raisedRow = Some(RaisedRowRule.Grow),
      hybridSinkLines = Some(2),
      smallCapsFirstLine = Some(false),
      epigraph = Some(EpigraphRule.ExplicitOnly),
      epigraphMarker = Some("motto"),
      sceneBreak = Some(SceneBreakRule.FullOpening),
      handDropCap = Some(HandDropCapRule.OpeningWins)
    )

    everything.isEmpty shouldBe false
    OpeningResolver.resolve(global, everything) shouldBe OpeningSettings(
      enabled = false,
      letterMode = LetterMode.Drop,
      raisedRow = RaisedRowRule.Grow,
      hybridSinkLines = 2,
      smallCapsFirstLine = false,
      epigraphMarker = "motto",
      rules = OpeningRules(EpigraphRule.ExplicitOnly, SceneBreakRule.FullOpening, HandDropCapRule.OpeningWins)
    )
  }

  it should "change each setting on its own" in {
    val single = List(
      OpeningOverride(enabled = Some(false))                -> global.copy(enabled = false),
      OpeningOverride(raisedRow = Some(RaisedRowRule.Grow)) -> global.copy(raisedRow = RaisedRowRule.Grow),
      OpeningOverride(hybridSinkLines = Some(3))            -> global.copy(hybridSinkLines = 3),
      OpeningOverride(smallCapsFirstLine = Some(false))     -> global.copy(smallCapsFirstLine = false),
      OpeningOverride(epigraphMarker = Some("quote"))       -> global.copy(epigraphMarker = "quote"),
      OpeningOverride(epigraph = Some(EpigraphRule.AutoDetect)) ->
        global.copy(rules = global.rules.copy(epigraph = EpigraphRule.AutoDetect)),
      OpeningOverride(sceneBreak = Some(SceneBreakRule.NoOpening)) ->
        global.copy(rules = global.rules.copy(sceneBreak = SceneBreakRule.NoOpening)),
      OpeningOverride(handDropCap = Some(HandDropCapRule.HandWins)) ->
        global.copy(rules = global.rules.copy(handDropCap = HandDropCapRule.HandWins))
    )

    single.foreach {
      case (overrides, expected) =>
        withClue(overrides)(OpeningResolver.resolve(global, overrides) shouldBe expected)
    }
  }

  it should "turn openings on for one document while the global setting is off" in {
    val resolved = OpeningResolver.resolve(OpeningSettings.default, OpeningOverride(enabled = Some(true)))

    resolved shouldBe OpeningSettings.default.copy(enabled = true)
  }

  it should "keep the sink lines and the marker within what they can be" in {
    val resolved = OpeningResolver.resolve(
      global,
      OpeningOverride(hybridSinkLines = Some(99), epigraphMarker = Some("   "))
    )

    resolved.hybridSinkLines shouldBe OpeningSettings.MaxSinkLines
    resolved.epigraphMarker shouldBe global.epigraphMarker
    OpeningResolver
      .resolve(global, OpeningOverride(hybridSinkLines = Some(-3)))
      .hybridSinkLines shouldBe OpeningSettings.MinSinkLines
  }

  "A setting's config key" should "round-trip through its parser" in {
    LetterMode.values.foreach(mode => LetterMode.fromConfigKey(mode.configKey) shouldBe Some(mode))
    RaisedRowRule.values.foreach(rule => RaisedRowRule.fromConfigKey(rule.configKey) shouldBe Some(rule))
    EpigraphRule.values.foreach(rule => EpigraphRule.fromConfigKey(rule.configKey) shouldBe Some(rule))
    SceneBreakRule.values.foreach(rule => SceneBreakRule.fromConfigKey(rule.configKey) shouldBe Some(rule))
    HandDropCapRule.values.foreach(rule => HandDropCapRule.fromConfigKey(rule.configKey) shouldBe Some(rule))
  }

  it should "be refused when it names nothing" in {
    LetterMode.fromConfigKey("sideways") shouldBe None
    EpigraphRule.fromConfigKey("") shouldBe None
  }

  "The settings the layout sees" should "be the same whatever else is set while openings are off" in {
    global.copy(enabled = false).layoutForm shouldBe OpeningSettings.default
    global.layoutForm shouldBe global
  }
