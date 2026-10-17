package com.serenity.config

import java.util.Locale

import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.theme.HeadingStyle

/** How headings are styled, per level and overall. */
private[config] object ConfigFieldsHeadings:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  private val size: FieldCodec[Option[Float]] =
    float.filtered(value => value >= HeadingsConfig.SizeRange._1 && value <= HeadingsConfig.SizeRange._2).orAuto

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
      levelField(level, "align", alignment)(_.align, (current, value) => current.copy(align = value))
    )

  val fields: List[ConfigField[?]] =
    (1 to HeadingStyle.LevelCount).toList.flatMap(levelFields) ++ List(
      field("headings.damping")(boolean)(
        _.headingsConfig.damping,
        (config, value) => config.withHeadingsConfig(config.headingsConfig.copy(damping = value))
      ),
      field("headings.damping_threshold")(threshold)(
        _.headingsConfig.dampingThresholdPt,
        (config, value) => config.withHeadingsConfig(config.headingsConfig.copy(dampingThresholdPt = value))
      )
    )
