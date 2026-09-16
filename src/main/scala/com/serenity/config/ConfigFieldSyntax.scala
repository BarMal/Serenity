package com.serenity.config

import java.awt.Color
import java.util.Locale

import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.serenity.ui.theme.ColorFormat

/** The codecs and constructors the per-domain `ConfigFields*` objects declare their settings with, so every field is
  * spelled the same way whichever file it lives in.
  */
private[config] object ConfigFieldSyntax:

  import FieldCodec.*

  /** `#RRGGBB` or `#RRGGBBAA`, which is what [[ColorFormat.toHex]] writes. */
  private[config] def colorFromHex(value: String): Option[Color] =
    val hex = value.trim.stripPrefix("#")
    Option
      .when(hex.length == 6 || hex.length == 8)(hex)
      .filter(_.forall(character => Character.digit(character, 16) >= 0))
      .flatMap { normalized =>
        scala.util.Try {
          val red   = Integer.parseInt(normalized.substring(0, 2), 16)
          val green = Integer.parseInt(normalized.substring(2, 4), 16)
          val blue  = Integer.parseInt(normalized.substring(4, 6), 16)
          val alpha = if normalized.length == 8 then Integer.parseInt(normalized.substring(6, 8), 16) else 255
          Color(red, green, blue, alpha)
        }.toOption
      }

  private[config] def colorToHex(value: Color): String = ColorFormat.toHex(value, withAlpha = true)

  private[config] val color: FieldCodec[Color] =
    given io.circe.Encoder[Color] = io.circe.Encoder.encodeString.contramap(colorToHex)
    given io.circe.Decoder[Color] =
      io.circe.Decoder.decodeString.emap(text => colorFromHex(text).toRight(s"Not a colour: $text"))
    FieldCodec.of(colorFromHex, value => HoconValue.string(colorToHex(value)))

  /** The segment list, which also accepts the older single-word presets (`minimal`, `detailed`) it replaced. */
  private[config] val infoBarSegments: FieldCodec[List[CursorInfoBarSegment]] =
    given io.circe.Encoder[List[CursorInfoBarSegment]] =
      io.circe.Encoder.encodeList(using io.circe.Encoder.encodeString.contramap(_.configKey))
    given io.circe.Decoder[List[CursorInfoBarSegment]] =
      io.circe.Decoder.decodeList(using
        io.circe.Decoder.decodeString.emap(key =>
          CursorInfoBarSegment
            .fromConfigKey(key)
            .orElse(CursorInfoBarSegment.values.find(_.toString == key))
            .toRight(s"Unknown cursor info bar segment: $key")
        )
      )
    FieldCodec.of(
      CursorInfoBarSegment.parseList,
      values => HoconValue.string(if values.isEmpty then "off" else values.map(_.configKey).mkString(","))
    )

  /** Font sizes are clamped rather than refused: a file asking for 400pt is a file that means "as big as you allow". */
  private[config] val fontSize: FieldCodec[Float] =
    FieldCodec.of(text => text.trim.toFloatOption.map(size => size.max(8.0f).min(48.0f)), HoconValue.number)

  private[config] val textScaleMode: FieldCodec[TextScaleMode] =
    enumerated(
      text =>
        text.toLowerCase(Locale.ROOT) match
          case "auto"                      => Some(TextScaleMode.Auto)
          case "manual" | "custom"         => Some(TextScaleMode.Manual)
          case "off" | "none" | "disabled" => Some(TextScaleMode.Off)
          case _                           => None
      ,
      _.configKey
    )

  /** A percentage in the file, a fraction in the config. The rounding matters: reading `17.3` back as a raw division
    * gives 0.17299999999999996, which then writes out as a different number than the one that was saved.
    */
  private[config] def fractionOfPercent(value: Double): Double =
    BigDecimal(value / 100.0).setScale(9, BigDecimal.RoundingMode.HALF_UP).toDouble

  private[config] val insetPercent: FieldCodec[Double] =
    double.filtered(percent => percent >= 0.0 && percent <= TextAreaInsets.MaxInset * 100.0)

  private[config] val viewportPercent: FieldCodec[Double] =
    double.filtered(percent =>
      percent >= ViewportAxisSizing.MinPercent * 100.0 && percent <= ViewportAxisSizing.MaxPercent * 100.0
    )

  private[config] def lowercased[A](values: Array[A]): FieldCodec[A] =
    enumerated(
      text => values.find(_.toString.equalsIgnoreCase(text.replace("-", ""))),
      value => value.toString.toLowerCase(Locale.ROOT)
    )

  private[config] val materialPreset: FieldCodec[MaterialPreset] =
    enumerated(
      text =>
        text.toLowerCase(Locale.ROOT) match
          case "solid" | "opaque"      => Some(MaterialPreset.Solid)
          case "clear" | "transparent" => Some(MaterialPreset.Clear)
          case "frosted" | "soft"      => Some(MaterialPreset.Frosted)
          case "crystal" | "glass"     => Some(MaterialPreset.Crystal)
          case "custom"                => Some(MaterialPreset.Custom)
          case _                       => None
      ,
      _.configKey
    )

  private[config] def field[A](key: String, aliases: String*)(codec: FieldCodec[A])(
    get: AppConfig => A,
    set: (AppConfig, A) => AppConfig
  ): ConfigField[A] = ConfigField(key, aliases.toSet, codec, get, set)

  private[config] def named[A](key: String, jsonKey: String, aliases: String*)(codec: FieldCodec[A])(
    get: AppConfig => A,
    set: (AppConfig, A) => AppConfig
  ): ConfigField[A] = ConfigField(key, aliases.toSet, codec, get, set, Some(jsonKey))

  /** For a setting whose setter adjusts a neighbour: putting back what was saved should touch only the field itself. */
  extension [A](configField: ConfigField[A])
    private[config] def restoredBy(assign: (AppConfig, A) => AppConfig): ConfigField[A] =
      configField.copy(restore = Some(assign))
