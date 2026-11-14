package com.serenity.config

import java.awt.Color
import java.util.Locale

import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.serenity.ui.fonts.FontSizes
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
  private[config] val statusSegments: FieldCodec[List[StatusSegment]] =
    given io.circe.Encoder[List[StatusSegment]] =
      io.circe.Encoder.encodeList(using io.circe.Encoder.encodeString.contramap(_.configKey))
    given io.circe.Decoder[List[StatusSegment]] =
      io.circe.Decoder.decodeList(using
        io.circe.Decoder.decodeString.emap(key =>
          StatusSegment
            .fromConfigKey(key)
            .orElse(StatusSegment.values.find(_.toString == key))
            .toRight(s"Unknown status segment: $key")
        )
      )
    FieldCodec.of(StatusSegment.parseList, segments => HoconValue.string(StatusSegment.renderList(segments)))

  private[config] val fontSize: FieldCodec[Float] =
    FieldCodec.of(text => text.trim.toFloatOption.map(size => size.max(8.0f).min(48.0f)), HoconValue.number)

  /** The sizes the last size change replaced, as `prose, code, ui`; `off` when there is nothing to undo. */
  private[config] val previousFontSizes: FieldCodec[Option[FontSizes]] =
    given io.circe.Encoder[Option[FontSizes]] =
      io.circe.Encoder.encodeOption(using io.circe.Encoder.encodeString.contramap(FontSizes.render))
    given io.circe.Decoder[Option[FontSizes]] =
      io.circe.Decoder.decodeOption(using
        io.circe.Decoder.decodeString.emap(text => FontSizes.parse(text).toRight(s"Not three font sizes: $text"))
      )
    FieldCodec
      .of(
        text => if parseBoolean(text).contains(false) then Some(None) else FontSizes.parse(text).map(Some.apply),
        sizes => HoconValue.string(sizes.fold("off")(FontSizes.render))
      )
      .copy(readList = Some(items => FontSizes.parse(items.mkString(",")).map(Some.apply)))

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

  /** A gutter: a percentage (a bare number as ever, or `N%`), `Ncells`, `Nch` and, where allowed, `auto`. */
  private[config] def gutter(allowAuto: Boolean, allowCh: Boolean): FieldCodec[Gutter] =
    given io.circe.Encoder[Gutter] = Gutter.fieldEncoder
    given io.circe.Decoder[Gutter] = Gutter.fieldDecoder(allowAuto, allowCh)
    FieldCodec.of(
      text => Gutter.parse(text, allowAuto, allowCh),
      {
        case Gutter.Percent(share) => HoconValue.number(Gutter.percentOf(share))
        case other                 => HoconValue.string(Gutter.render(other))
      }
    )

  /** Where a line wraps: `edge`, a count of cells, or `Nch`. */
  private[config] val wrapColumn: FieldCodec[WrapColumn] =
    FieldCodec.of(
      WrapColumn.parse,
      {
        case WrapColumn.Cells(count) => HoconValue.number(count)
        case other                   => HoconValue.string(WrapColumn.render(other))
      }
    )

  private[config] def lowercased[A](values: Array[A]): FieldCodec[A] =
    enumerated(
      text => values.find(_.toString.equalsIgnoreCase(text.replace("-", ""))),
      value => value.toString.toLowerCase(Locale.ROOT)
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
