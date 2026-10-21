package com.serenity.config

import java.util.Locale

import com.serenity.ui.openings.{
  EpigraphRule,
  HandDropCapRule,
  LetterMode,
  OpeningRules,
  OpeningSettings,
  RaisedRowRule,
  SceneBreakRule
}

/** How chapter openings are set; a document can override each of these, see
  * [[com.serenity.ui.openings.OpeningOverride]].
  */
private[config] object ConfigFieldsOpenings:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  private def keyed[A](fromKey: String => Option[A], toKey: A => String): FieldCodec[A] =
    enumerated(text => fromKey(text.toLowerCase(Locale.ROOT)), toKey)

  private val sinkLines: FieldCodec[Int] =
    int.filtered(lines => lines >= OpeningSettings.MinSinkLines && lines <= OpeningSettings.MaxSinkLines)

  private val marker: FieldCodec[String] = string.filtered(_.nonEmpty)

  private def openings(config: AppConfig, update: OpeningSettings => OpeningSettings): AppConfig =
    config.withOpeningsConfig(update(config.openingsConfig))

  private def rules(config: AppConfig, update: OpeningRules => OpeningRules): AppConfig =
    openings(config, current => current.copy(rules = update(current.rules)))

  val fields: List[ConfigField[?]] =
    List(
      field("openings.enabled")(boolean)(
        _.openingsConfig.enabled,
        (config, value) => openings(config, _.copy(enabled = value))
      ),
      field("openings.letter")(keyed(LetterMode.fromConfigKey, _.configKey))(
        _.openingsConfig.letterMode,
        (config, value) => openings(config, _.copy(letterMode = value))
      ),
      field("openings.raised_row")(keyed(RaisedRowRule.fromConfigKey, _.configKey))(
        _.openingsConfig.raisedRow,
        (config, value) => openings(config, _.copy(raisedRow = value))
      ),
      field("openings.hybrid_sink_lines")(sinkLines)(
        _.openingsConfig.hybridSinkLines,
        (config, value) => openings(config, _.copy(hybridSinkLines = value))
      ),
      field("openings.small_caps")(boolean)(
        _.openingsConfig.smallCapsFirstLine,
        (config, value) => openings(config, _.copy(smallCapsFirstLine = value))
      ),
      field("openings.epigraph")(keyed(EpigraphRule.fromConfigKey, _.configKey))(
        _.openingsConfig.rules.epigraph,
        (config, value) => rules(config, _.copy(epigraph = value))
      ),
      field("openings.epigraph_marker")(marker)(
        _.openingsConfig.epigraphMarker,
        (config, value) => openings(config, _.copy(epigraphMarker = value))
      ),
      field("openings.scene_break")(keyed(SceneBreakRule.fromConfigKey, _.configKey))(
        _.openingsConfig.rules.sceneBreak,
        (config, value) => rules(config, _.copy(sceneBreak = value))
      ),
      field("openings.hand_drop_cap")(keyed(HandDropCapRule.fromConfigKey, _.configKey))(
        _.openingsConfig.rules.handDropCap,
        (config, value) => rules(config, _.copy(handDropCap = value))
      )
    )
