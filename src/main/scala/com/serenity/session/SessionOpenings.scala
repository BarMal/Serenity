package com.serenity.session

import com.serenity.ui.openings.*

/** A buffer's chapter-opening overrides as a session stores them: each setting that is set, spelled by its config key.
  * A spelling this version cannot read is left out on restore, so the setting follows the global one.
  */
final case class SessionOpenings(
    enabled: Option[Boolean] = None,
    letter: Option[String] = None,
    raisedRow: Option[String] = None,
    hybridSinkLines: Option[Int] = None,
    smallCaps: Option[Boolean] = None,
    epigraph: Option[String] = None,
    epigraphMarker: Option[String] = None,
    sceneBreak: Option[String] = None,
    handDropCap: Option[String] = None
)

object SessionOpenings:

  def fromOverride(overrides: OpeningOverride): Option[SessionOpenings] =
    Option.unless(overrides.isEmpty)(
      SessionOpenings(
        enabled = overrides.enabled,
        letter = overrides.letterMode.map(_.configKey),
        raisedRow = overrides.raisedRow.map(_.configKey),
        hybridSinkLines = overrides.hybridSinkLines,
        smallCaps = overrides.smallCapsFirstLine,
        epigraph = overrides.epigraph.map(_.configKey),
        epigraphMarker = overrides.epigraphMarker,
        sceneBreak = overrides.sceneBreak.map(_.configKey),
        handDropCap = overrides.handDropCap.map(_.configKey)
      )
    )

  def toOverride(stored: SessionOpenings): OpeningOverride =
    OpeningOverride(
      enabled = stored.enabled,
      letterMode = stored.letter.flatMap(LetterMode.fromConfigKey),
      raisedRow = stored.raisedRow.flatMap(RaisedRowRule.fromConfigKey),
      hybridSinkLines = stored.hybridSinkLines,
      smallCapsFirstLine = stored.smallCaps,
      epigraph = stored.epigraph.flatMap(EpigraphRule.fromConfigKey),
      epigraphMarker = stored.epigraphMarker,
      sceneBreak = stored.sceneBreak.flatMap(SceneBreakRule.fromConfigKey),
      handDropCap = stored.handDropCap.flatMap(HandDropCapRule.fromConfigKey)
    )
