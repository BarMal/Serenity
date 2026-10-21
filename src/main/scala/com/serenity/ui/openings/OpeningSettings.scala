package com.serenity.ui.openings

/** How the first letter of an opening is set. */
enum LetterMode(val configKey: String):
  case Drop   extends LetterMode("drop")
  case Raised extends LetterMode("raised")
  case Hybrid extends LetterMode("hybrid")

object LetterMode:
  def fromConfigKey(key: String): Option[LetterMode] = values.find(_.configKey == key)

/** What a raised letter does to the row it stands on. */
enum RaisedRowRule(val configKey: String):
  case Grow  extends RaisedRowRule("grow")
  case Clamp extends RaisedRowRule("clamp")

object RaisedRowRule:
  def fromConfigKey(key: String): Option[RaisedRowRule] = values.find(_.configKey == key)

/** Every setting of chapter openings, whether it is the global configuration or what one document resolves to.
  * `epigraphMarker` is read only under [[EpigraphRule.ExplicitOnly]]; `hybridSinkLines` only under
  * [[LetterMode.Hybrid]].
  */
final case class OpeningSettings(
    enabled: Boolean = false,
    letterMode: LetterMode = LetterMode.Drop,
    raisedRow: RaisedRowRule = RaisedRowRule.Grow,
    hybridSinkLines: Int = OpeningSettings.MinSinkLines,
    smallCapsFirstLine: Boolean = false,
    epigraphMarker: String = OpeningSettings.DefaultEpigraphMarker,
    rules: OpeningRules = OpeningRules.default
):

  def normalized: OpeningSettings =
    val sink   = hybridSinkLines.max(OpeningSettings.MinSinkLines).min(OpeningSettings.MaxSinkLines)
    val marker = if epigraphMarker.isBlank then OpeningSettings.DefaultEpigraphMarker else epigraphMarker
    if sink == hybridSinkLines && marker == epigraphMarker then this
    else copy(hybridSinkLines = sink, epigraphMarker = marker)

  /** What layout depends on: while openings are off nothing else they say is read, so every such setting is one value
    * and changing one does not look like a change to layout.
    */
  def layoutForm: OpeningSettings =
    if enabled then this else OpeningSettings.default

object OpeningSettings:
  val MinSinkLines: Int = 2
  val MaxSinkLines: Int = 6

  val DefaultEpigraphMarker: String = "epigraph"

  val default: OpeningSettings = OpeningSettings()

/** What one document sets differently from the global settings. A setting left `None` is the global one. */
final case class OpeningOverride(
    enabled: Option[Boolean] = None,
    letterMode: Option[LetterMode] = None,
    raisedRow: Option[RaisedRowRule] = None,
    hybridSinkLines: Option[Int] = None,
    smallCapsFirstLine: Option[Boolean] = None,
    epigraph: Option[EpigraphRule] = None,
    epigraphMarker: Option[String] = None,
    sceneBreak: Option[SceneBreakRule] = None,
    handDropCap: Option[HandDropCapRule] = None
):

  def isEmpty: Boolean = this == OpeningOverride.Empty

object OpeningOverride:
  val Empty: OpeningOverride = OpeningOverride()

object OpeningResolver:

  /** The settings a document is laid out with: its own overrides over the global `settings`. */
  def resolve(settings: OpeningSettings, overrides: OpeningOverride): OpeningSettings =
    if overrides.isEmpty then settings
    else
      OpeningSettings(
        enabled = overrides.enabled.getOrElse(settings.enabled),
        letterMode = overrides.letterMode.getOrElse(settings.letterMode),
        raisedRow = overrides.raisedRow.getOrElse(settings.raisedRow),
        hybridSinkLines = overrides.hybridSinkLines.getOrElse(settings.hybridSinkLines),
        smallCapsFirstLine = overrides.smallCapsFirstLine.getOrElse(settings.smallCapsFirstLine),
        epigraphMarker = overrides.epigraphMarker.getOrElse(settings.epigraphMarker),
        rules = OpeningRules(
          epigraph = overrides.epigraph.getOrElse(settings.rules.epigraph),
          sceneBreak = overrides.sceneBreak.getOrElse(settings.rules.sceneBreak),
          handDropCap = overrides.handDropCap.getOrElse(settings.rules.handDropCap)
        )
      ).normalized
