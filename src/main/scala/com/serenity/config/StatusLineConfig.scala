package com.serenity.config

import java.awt.Color

/** One piece of text the status line can show, in the order the user has chosen to include them. */
enum StatusSegment(val configKey: String):
  case Position    extends StatusSegment("position")
  case Title       extends StatusSegment("title")
  case Language    extends StatusSegment("language")
  case Mode        extends StatusSegment("mode")
  case WordCount   extends StatusSegment("word_count")
  case CharCount   extends StatusSegment("char_count")
  case ReadingTime extends StatusSegment("reading_time")

object StatusSegment:

  def fromConfigKey(value: String): Option[StatusSegment] =
    values.find(_.configKey == value.trim.toLowerCase)

  /** Parses `status.segments`: a comma-separated segment list, or `off`/empty for none. */
  def parseList(value: String): Option[List[StatusSegment]] =
    value.trim.toLowerCase match
      case "" | "off" | "none" | "false" | "disabled" => Some(Nil)
      // The pre-segment info bar presets, still accepted on the way in.
      case "minimal"           => Some(List(Position))
      case "detailed" | "full" => Some(List(Position, Title))
      case trimmed =>
        val parsed = trimmed.split(",").toList.map(_.trim).filter(_.nonEmpty).map(fromConfigKey)
        Option.when(parsed.nonEmpty && parsed.forall(_.isDefined))(parsed.flatten)

  def renderList(segments: List[StatusSegment]): String =
    if segments.isEmpty then "off" else segments.map(_.configKey).mkString(", ")

  /** Adds `segment` at its slot in the canonical definition order relative to the already-included segments, rather
    * than appending to the end -- so toggling a segment off then on restores its position instead of shunting it to the
    * tail (#1533), while a manual reordering of the existing segments is preserved. A no-op when already present.
    */
  def include(segments: List[StatusSegment], segment: StatusSegment): List[StatusSegment] =
    if segments.contains(segment) then segments
    else
      val canonicalIndex = values.toList.indexOf(_: StatusSegment)
      segments.indexWhere(existing => canonicalIndex(existing) > canonicalIndex(segment)) match
        case -1       => segments :+ segment
        case insertAt => segments.patch(insertAt, List(segment), 0)

  def move(segments: List[StatusSegment], segment: StatusSegment, delta: Int): List[StatusSegment] =
    val index  = segments.indexOf(segment)
    val target = index + delta
    if index < 0 || target < 0 || target >= segments.length then segments
    else
      segments.zipWithIndex.map {
        case (_, `index`)  => segments(target)
        case (_, `target`) => segments(index)
        case (other, _)    => other
      }

/** Where the status line lives: a full-width row pinned under the workspace, a quiet row that follows the caret, or
  * nowhere.
  */
enum StatusLinePlacement(val configKey: String):
  case Pinned   extends StatusLinePlacement("pinned")
  case Floating extends StatusLinePlacement("floating")
  case Off      extends StatusLinePlacement("off")

object StatusLinePlacement:

  def fromConfigKey(value: String): Option[StatusLinePlacement] =
    value.trim.toLowerCase match
      case "pinned" | "pinned-bottom" | "pinnedbottom" | "bottom" | "bar" => Some(Pinned)
      case "floating" | "float" | "cursor"                                => Some(Floating)
      case "off" | "none" | "hidden" | "false"                            => Some(Off)
      case _                                                              => None

/** `None` keeps the active theme's own panel colour and alpha, matching every other panel; `Some` overrides just the
  * status line, independent of theme (#1295).
  */
final case class StatusLineColors(
    foreground: Option[Color] = None,
    background: Option[Color] = None,
    backgroundAlpha: Option[Double] = None
):
  def foregroundOr(default: Color): Color = foreground.getOrElse(default)
  def backgroundOr(default: Color): Color = background.getOrElse(default)

final case class StatusLineConfig(
    segments: List[StatusSegment] = StatusLineConfig.defaultSegments,
    placement: StatusLinePlacement = StatusLinePlacement.Pinned,
    colors: StatusLineColors = StatusLineColors()
):
  def isShown: Boolean    = placement != StatusLinePlacement.Off && segments.nonEmpty
  def isPinned: Boolean   = isShown && placement == StatusLinePlacement.Pinned
  def isFloating: Boolean = isShown && placement == StatusLinePlacement.Floating

object StatusLineConfig:

  val MinBackgroundAlpha: Double = 0.0
  val MaxBackgroundAlpha: Double = 1.0

  val defaultSegments: List[StatusSegment] =
    List(StatusSegment.Position, StatusSegment.Language, StatusSegment.Title, StatusSegment.Mode)

  val default: StatusLineConfig = StatusLineConfig()

  def clampBackgroundAlpha(alpha: Double): Double =
    if alpha.isFinite then alpha.max(MinBackgroundAlpha).min(MaxBackgroundAlpha) else MinBackgroundAlpha
