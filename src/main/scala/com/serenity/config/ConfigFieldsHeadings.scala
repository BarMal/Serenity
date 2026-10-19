package com.serenity.config

import java.util.Locale

import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.color.RenderColor
import com.serenity.ui.theme.{HeadingColour, HeadingStyle}

/** How headings are styled, per level and overall. */
private[config] object ConfigFieldsHeadings:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  private val size: FieldCodec[Option[Float]] =
    float.filtered(value => value >= HeadingsConfig.SizeRange._1 && value <= HeadingsConfig.SizeRange._2).orAuto

  private val space: FieldCodec[Option[Float]] =
    float.filtered(value => value >= HeadingsConfig.SpaceRange._1 && value <= HeadingsConfig.SpaceRange._2).orAuto

  private val flag: FieldCodec[Option[Boolean]] = boolean.orAuto

  private val family: FieldCodec[Option[String]] = string.filtered(_.nonEmpty).orAuto

  private val alignment: FieldCodec[Option[ParagraphAlignment]] =
    enumerated[ParagraphAlignment](
      text =>
        text.toLowerCase(Locale.ROOT) match
          case "left"              => Some(ParagraphAlignment.Left)
          case "centre" | "center" => Some(ParagraphAlignment.Center)
          case "right"             => Some(ParagraphAlignment.Right)
          case "justify"           => Some(ParagraphAlignment.Justify)
          case _                   => None
      ,
      {
        case ParagraphAlignment.Left    => "left"
        case ParagraphAlignment.Center  => "centre"
        case ParagraphAlignment.Right   => "right"
        case ParagraphAlignment.Justify => "justify"
      }
    ).orAuto

  private def parseColour(text: String): Option[HeadingColour] =
    text.trim.toLowerCase(Locale.ROOT) match
      case "text"      => Some(HeadingColour.Text)
      case "heading"   => Some(HeadingColour.Heading)
      case "accent"    => Some(HeadingColour.Accent)
      case "secondary" => Some(HeadingColour.Secondary)
      case "muted"     => Some(HeadingColour.Muted)
      case other       => colorFromHex(other).map(color => HeadingColour.Fixed(RenderColor.fromAwt(color)))

  private def colourKey(colour: HeadingColour): String =
    colour match
      case HeadingColour.Text         => "text"
      case HeadingColour.Heading      => "heading"
      case HeadingColour.Accent       => "accent"
      case HeadingColour.Secondary    => "secondary"
      case HeadingColour.Muted        => "muted"
      case HeadingColour.Fixed(color) => colorToHex(color.toAwt)

  /** `auto` follows the global colour; `text`, `heading`, `accent`, `secondary`, `muted` or `#RRGGBB` say what this
    * level is.
    */
  private val levelColour: FieldCodec[Option[HeadingColour]] = enumerated(parseColour, colourKey).orAuto

  /** `auto` keeps each kind of document's own look; `text`, `heading`, `accent`, `secondary` or `muted` applies to
    * both.
    */
  private val globalColour: FieldCodec[Option[HeadingColour]] =
    enumerated[HeadingColour](
      text =>
        parseColour(text).flatMap {
          case HeadingColour.Fixed(_) => None
          case choice                 => Some(choice)
        },
      colourKey
    ).orAuto

  private val threshold: FieldCodec[Float] =
    float.filtered(value => value >= HeadingsConfig.ThresholdRange._1 && value <= HeadingsConfig.ThresholdRange._2)

  private def levelField[A](level: Int, attribute: String, codec: FieldCodec[Option[A]])(
    get: HeadingLevelConfig => Option[A],
    set: (HeadingLevelConfig, Option[A]) => HeadingLevelConfig
  ): ConfigField[Option[A]] =
    field(s"headings.h$level.$attribute")(codec)(
      config => get(config.headingsConfig.levels(level - 1)),
      (config, value) =>
        config.withHeadingsConfig(config.headingsConfig.withLevel(level)(current => set(current, value)))
    )

  private def levelFields(level: Int): List[ConfigField[?]] =
    List(
      levelField(level, "size", size)(_.size, (current, value) => current.copy(size = value)),
      levelField(level, "bold", flag)(_.bold, (current, value) => current.copy(bold = value)),
      levelField(level, "italic", flag)(_.italic, (current, value) => current.copy(italic = value)),
      levelField(level, "family", family)(_.family, (current, value) => current.copy(family = value)),
      levelField(level, "colour", levelColour)(_.colour, (current, value) => current.copy(colour = value)),
      levelField(level, "align", alignment)(_.align, (current, value) => current.copy(align = value)),
      levelField(level, "space_before", space)(
        _.spaceBefore,
        (current, value) => current.copy(spaceBefore = value)
      ),
      levelField(level, "space_after", space)(_.spaceAfter, (current, value) => current.copy(spaceAfter = value)),
      levelField(level, "underline", flag)(_.underline, (current, value) => current.copy(underline = value)),
      levelField(level, "rule", flag)(_.rule, (current, value) => current.copy(rule = value))
    )

  val fields: List[ConfigField[?]] =
    (1 to HeadingStyle.LevelCount).toList.flatMap(levelFields) ++ List(
      field("headings.colour")(globalColour)(
        _.headingsConfig.colour,
        (config, value) => config.withHeadingsConfig(config.headingsConfig.copy(colour = value))
      ),
      field("headings.damping")(boolean)(
        _.headingsConfig.damping,
        (config, value) => config.withHeadingsConfig(config.headingsConfig.copy(damping = value))
      ),
      field("headings.damping_threshold")(threshold)(
        _.headingsConfig.dampingThresholdPt,
        (config, value) => config.withHeadingsConfig(config.headingsConfig.copy(dampingThresholdPt = value))
      )
    )
