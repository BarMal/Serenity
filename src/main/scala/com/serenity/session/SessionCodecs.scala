package com.serenity.session

import java.awt.{Color, Font}

import scala.concurrent.duration.FiniteDuration

import cats.syntax.all.*
import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope}
import com.serenity.config.*
import com.serenity.lsp.config.{LspServerOverride, LspUserConfig}
import com.serenity.richtext.*
import com.serenity.ui.fonts.FontLoader.{FontConfig, TextScaleMode}
import com.serenity.ui.theme.ColorFormat
import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.given

// Circe codecs for all types
// First encode the basic dependencies
given Encoder[FiniteDuration] = Encoder.encodeLong.contramap(_.toNanos)
given Decoder[FiniteDuration] = Decoder.decodeLong.map(scala.concurrent.duration.Duration.fromNanos)

given Encoder[AnimationConfig] = deriveEncoder
given Decoder[AnimationConfig] = deriveDecoder

/** Builds the codec for an enum that carries a `configKey` -- the same spelling `ConfigManager` already writes to the
  * config file. The encoder always writes `configKey`, so a value looks identical whether it came from a session file
  * or the config file. The decoder accepts both `configKey` and the enum's `toString` name, because earlier releases of
  * `SessionState` wrote `toString`: this keeps every session file written by the current release loading unchanged.
  * There is no plan to stop accepting the legacy spelling -- it costs nothing to keep reading, and dropping it would
  * risk breaking someone's saved session for no benefit.
  */
private def configKeyEncoder[A](configKey: A => String): Encoder[A] =
  Encoder.encodeString.contramap(configKey)

private def configKeyDecoder[A](typeName: String, values: Array[A], configKey: A => String): Decoder[A] =
  Decoder.decodeString.emap { value =>
    values
      .find(a => configKey(a) == value || a.toString == value)
      .toRight(s"Unknown $typeName: $value")
  }

given Encoder[TextScaleMode] = Encoder.encodeString.contramap(_.configKey)

given Decoder[TextScaleMode] = Decoder.decodeString.emap { value =>
  value.toLowerCase match
    case "auto"                      => Right(TextScaleMode.Auto)
    case "manual" | "custom"         => Right(TextScaleMode.Manual)
    case "off" | "none" | "disabled" => Right(TextScaleMode.Off)
    case other                       => Left(s"Unknown text scale mode: $other")
}

given Encoder[FontConfig] = deriveEncoder

given Decoder[FontConfig] = Decoder.instance { cursor =>
  for
    codeFontFamily  <- cursor.getOrElse[String]("codeFontFamily")(FontConfig().codeFontFamily)
    textFontFamily  <- cursor.getOrElse[String]("textFontFamily")(FontConfig().textFontFamily)
    uiFontFamily    <- cursor.getOrElse[String]("uiFontFamily")(Font.SANS_SERIF)
    legacyFontSize  <- cursor.getOrElse[Float]("fontSize")(FontConfig().fontSize)
    codeFontSize    <- cursor.getOrElse[Float]("codeFontSize")(legacyFontSize)
    textFontSize    <- cursor.getOrElse[Float]("textFontSize")(legacyFontSize)
    uiFontSize      <- cursor.getOrElse[Float]("uiFontSize")(FontConfig().uiFontSize)
    textScaleMode   <- cursor.getOrElse[TextScaleMode]("textScaleMode")(FontConfig().textScaleMode)
    textScale       <- cursor.getOrElse[Double]("textScaleMultiplier")(FontConfig().textScaleMultiplier)
    legacyLigatures <- cursor.getOrElse[Boolean]("enableLigatures")(FontConfig().enableLigatures)
    codeLigatures   <- cursor.getOrElse[Boolean]("codeLigatures")(legacyLigatures)
    textLigatures   <- cursor.getOrElse[Boolean]("textLigatures")(legacyLigatures)
    uiLigatures     <- cursor.getOrElse[Boolean]("uiLigatures")(FontConfig().uiLigatures)
  yield FontConfig(
    codeFontFamily = codeFontFamily,
    textFontFamily = textFontFamily,
    uiFontFamily = uiFontFamily,
    fontSize = codeFontSize,
    textFontSize = textFontSize,
    uiFontSize = uiFontSize,
    textScaleMode = textScaleMode,
    textScaleMultiplier = FontConfig.clampTextScale(textScale),
    enableLigatures = codeLigatures,
    textLigatures = textLigatures,
    uiLigatures = uiLigatures
  )
}

given Encoder[CursorMode] = configKeyEncoder(_.configKey)
given Decoder[CursorMode] = configKeyDecoder("CursorMode", CursorMode.values, _.configKey)

given Encoder[CursorInfoBarSegment] = configKeyEncoder(_.configKey)
given Decoder[CursorInfoBarSegment] =
  configKeyDecoder("CursorInfoBarSegment", CursorInfoBarSegment.values, _.configKey)

given Encoder[CursorInfoBarPlacement] = configKeyEncoder(_.configKey)

given Decoder[CursorInfoBarPlacement] =
  configKeyDecoder("CursorInfoBarPlacement", CursorInfoBarPlacement.values, _.configKey)

given Encoder[WindowChromeMode] = configKeyEncoder(_.configKey)
given Decoder[WindowChromeMode] = configKeyDecoder("WindowChromeMode", WindowChromeMode.values, _.configKey)

given Encoder[MarkdownViewMode] = configKeyEncoder(_.configKey)
given Decoder[MarkdownViewMode] = configKeyDecoder("MarkdownViewMode", MarkdownViewMode.values, _.configKey)

given Encoder[DefaultDocumentMode] = configKeyEncoder(_.configKey)

given Decoder[DefaultDocumentMode] = configKeyDecoder("DefaultDocumentMode", DefaultDocumentMode.values, _.configKey)

given Encoder[RenderFpsTarget] = Encoder.encodeString.contramap(_.configKey)

given Decoder[RenderFpsTarget] =
  Decoder.decodeString.emap(value => RenderFpsTarget.fromConfigKey(value).toRight(s"Unknown RenderFpsTarget: $value"))

given Encoder[RenderDamageGranularity] = Encoder.encodeString.contramap(_.configKey)

given Decoder[RenderDamageGranularity] =
  Decoder.decodeString.emap(value =>
    RenderDamageGranularity.fromConfigKey(value).toRight(s"Unknown RenderDamageGranularity: $value")
  )

given Encoder[InterfaceDensity] = configKeyEncoder(_.configKey)
given Decoder[InterfaceDensity] = configKeyDecoder("InterfaceDensity", InterfaceDensity.values, _.configKey)

given Encoder[PreferredWindowSize] = deriveEncoder
given Decoder[PreferredWindowSize] = deriveDecoder
given Encoder[WindowConfig]        = deriveEncoder
given Decoder[WindowConfig]        = deriveDecoder
given Encoder[CursorConfig]        = deriveEncoder
given Decoder[CursorConfig]        = deriveDecoder
given Encoder[DocumentConfig]      = deriveEncoder
given Decoder[DocumentConfig]      = deriveDecoder
given Encoder[InterfaceConfig]     = deriveEncoder
given Decoder[InterfaceConfig]     = deriveDecoder

given Encoder[TextAreaInsets] = deriveEncoder
given Decoder[TextAreaInsets] = deriveDecoder

// BackgroundStyle has no configKey: it is never written to the config file on its own (ConfigManager derives it
// from MaterialPreset), so there is no config-file spelling to converge on. Left on toString deliberately.
given Encoder[BackgroundStyle] = Encoder.encodeString.contramap(_.toString)

given Decoder[BackgroundStyle] = Decoder.decodeString.emap {
  case "Solid"       => Right(BackgroundStyle.Solid)
  case "Transparent" => Right(BackgroundStyle.Transparent)
  case "Frosted"     => Right(BackgroundStyle.Frosted)
  case "GlassLike"   => Right(BackgroundStyle.GlassLike)
  case other         => Left(s"Unknown BackgroundStyle: $other")
}

given Encoder[MaterialPreset] = configKeyEncoder(_.configKey)
given Decoder[MaterialPreset] = configKeyDecoder("MaterialPreset", MaterialPreset.values, _.configKey)

given Encoder[MotionPreset] = configKeyEncoder(_.configKey)
given Decoder[MotionPreset] = configKeyDecoder("MotionPreset", MotionPreset.values, _.configKey)

given Encoder[MotionAccessibility] = configKeyEncoder(_.configKey)
given Decoder[MotionAccessibility] = configKeyDecoder("MotionAccessibility", MotionAccessibility.values, _.configKey)

given Encoder[MotionFamily] = configKeyEncoder(_.configKey)
given Decoder[MotionFamily] = configKeyDecoder("MotionFamily", MotionFamily.values, _.configKey)

// TransitionKind has no configKey of its own -- ConfigManager keeps a separate ad hoc string mapping
// (`transitionKindConfigKey`) rather than a field on the enum, so there is nothing here to generalize onto. Left
// on toString deliberately.
given Encoder[TransitionKind] = Encoder.encodeString.contramap(_.toString)

given Decoder[TransitionKind] = Decoder.decodeString.emap {
  case "Disabled"               => Right(TransitionKind.Disabled)
  case "Fade"                   => Right(TransitionKind.Fade)
  case "TypedText"              => Right(TransitionKind.TypedText)
  case "DirectionalSweep"       => Right(TransitionKind.DirectionalSweep)
  case "OutlineThenContent"     => Right(TransitionKind.OutlineThenContent)
  case "LineAndCharacterTandem" => Right(TransitionKind.LineAndCharacterTandem)
  case other                    => Left(s"Unknown TransitionKind: $other")
}

given Encoder[MotionFamilyConfig] = Encoder.instance { config =>
  Json.obj(
    "enabled"        -> config.enabled.asJson,
    "transitionKind" -> config.transitionKind.asJson,
    "animation"      -> config.animation.asJson,
    "speedScale"     -> config.speedScale.asJson,
    // TransitionScope has no configKey (see the TransitionKind note above), so its toString spelling is the only
    // one that has ever existed here -- no format divergence to fix for this map's keys.
    "transitionOverrides" -> config.transitionOverrides.map { case (scope, kind) => scope.toString -> kind }.asJson
  )
}

given Decoder[MotionFamilyConfig] = Decoder.instance { cursor =>
  for
    enabled        <- cursor.get[Boolean]("enabled")
    transitionKind <- cursor.get[TransitionKind]("transitionKind")
    animation      <- cursor.get[Option[AnimationConfig]]("animation")
    speedScale     <- cursor.get[Double]("speedScale")
    encoded        <- cursor.getOrElse[Map[String, TransitionKind]]("transitionOverrides")(Map.empty)
    transitionOverrides <- encoded.toList.traverse {
      case (name, kind) =>
        TransitionScope.values
          .find(_.toString == name)
          .toRight(DecodingFailure(s"Unknown TransitionScope: $name", cursor.history))
          .map(_ -> kind)
    }
  yield MotionFamilyConfig(enabled, transitionKind, animation, speedScale, transitionOverrides.toMap)
}

given Encoder[MotionConfig] = Encoder.instance { config =>
  Json.obj(
    "accessibility" -> config.accessibility.asJson,
    "baseline"      -> config.baseline.asJson,
    "families"      -> config.families.map { case (family, settings) => family.configKey -> settings }.asJson
  )
}

given Decoder[MotionConfig] = Decoder.instance { cursor =>
  for
    accessibility <- cursor.get[MotionAccessibility]("accessibility")
    baseline      <- cursor.get[MotionPreset]("baseline")
    encoded       <- cursor.get[Map[String, MotionFamilyConfig]]("families")
    families <- encoded.toList.traverse {
      case (name, settings) =>
        MotionFamily.values
          .find(family => family.configKey == name || family.toString == name)
          .toRight(DecodingFailure(s"Unknown MotionFamily: $name", cursor.history))
          .map(_ -> settings)
    }
  yield MotionConfig(accessibility, baseline, families.toMap)
}

given Encoder[Color] = Encoder.encodeString.contramap(formatColor)

given Decoder[Color] = Decoder.decodeString.emap(value => parseColor(value).toRight(s"Invalid colour value: $value"))

given Encoder[CursorColorConfig] = deriveEncoder
given Decoder[CursorColorConfig] = deriveDecoder

given Encoder[CursorInfoBarColorConfig] = deriveEncoder
given Decoder[CursorInfoBarColorConfig] = deriveDecoder

given Encoder[LspServerOverride] = deriveEncoder
given Decoder[LspServerOverride] = deriveDecoder

given Encoder[LspUserConfig] = deriveEncoder
given Decoder[LspUserConfig] = deriveDecoder

given Encoder[SpellCheckConfig] = Encoder.instance { config =>
  io.circe.Json.obj(
    "enabled"         -> config.enabled.asJson,
    "languages"       -> config.languages.asJson,
    "dictionaryPaths" -> config.dictionaryPaths.asJson,
    "additionalWords" -> config.additionalWords.asJson
  )
}

given Decoder[SpellCheckConfig] = Decoder.instance { cursor =>
  for
    enabled         <- cursor.getOrElse[Boolean]("enabled")(false)
    languages       <- cursor.getOrElse[List[String]]("languages")(List("en"))
    dictionaryPaths <- cursor.getOrElse[List[String]]("dictionaryPaths")(Nil)
    additionalWords <- cursor.getOrElse[List[String]]("additionalWords")(Nil)
  yield SpellCheckConfig(
    enabled = enabled,
    languages = languages,
    dictionaryPaths = dictionaryPaths,
    additionalWords = additionalWords
  ).normalized
}

// InlineMark and ParagraphAlignment describe rich-text document content, not app configuration -- they have no
// configKey and are never written to the config file, so there is no spelling to converge on here. Left on
// toString deliberately.
given Encoder[InlineMark] = Encoder.encodeString.contramap(_.toString)

given Decoder[InlineMark] = Decoder.decodeString.emap {
  case "Bold"      => Right(InlineMark.Bold)
  case "Italic"    => Right(InlineMark.Italic)
  case "Underline" => Right(InlineMark.Underline)
  case other       => Left(s"Unknown InlineMark: $other")
}

given Encoder[ParagraphAlignment] = Encoder.encodeString.contramap(_.toString)

given Decoder[ParagraphAlignment] = Decoder.decodeString.emap {
  case "Left"    => Right(ParagraphAlignment.Left)
  case "Center"  => Right(ParagraphAlignment.Center)
  case "Right"   => Right(ParagraphAlignment.Right)
  case "Justify" => Right(ParagraphAlignment.Justify)
  case other     => Left(s"Unknown ParagraphAlignment: $other")
}

given Encoder[RichTextStyle] = deriveEncoder
given Decoder[RichTextStyle] = deriveDecoder

given Encoder[RichTextRun] = deriveEncoder
given Decoder[RichTextRun] = deriveDecoder

given Encoder[ParagraphRole] = Encoder.instance {
  case ParagraphRole.Body =>
    io.circe.Json.obj("type" -> io.circe.Json.fromString("body"))
  case ParagraphRole.Heading(level) =>
    io.circe.Json.obj(
      "type"  -> io.circe.Json.fromString("heading"),
      "level" -> io.circe.Json.fromInt(level.max(1))
    )
}

given Decoder[ParagraphRole] = Decoder.instance { cursor =>
  cursor.downField("type").as[Option[String]].flatMap {
    case Some("heading") =>
      cursor.downField("level").as[Option[Int]].map(level => ParagraphRole.Heading(level.getOrElse(1).max(1)))
    case Some("body") | None =>
      Right(ParagraphRole.Body)
    case Some(other) =>
      Left(io.circe.DecodingFailure(s"Unknown paragraph role: $other", cursor.history))
  }
}

given Encoder[RichTextParagraph] = deriveEncoder
given Decoder[RichTextParagraph] = deriveDecoder

given Encoder[RichTextDocument] = deriveEncoder
given Decoder[RichTextDocument] = deriveDecoder

given Encoder[AppConfig] = Encoder.instance(SessionConfigCodec.encode)

given Decoder[AppConfig] = Decoder.instance(cursor => Right(SessionConfigCodec.decode(cursor)))

private def parseColor(value: String): Option[Color] =
  val hex = value.stripPrefix("#")
  Option
    .when(hex.length == 6 || hex.length == 8)(hex)
    .filter(_.forall(ch => Character.digit(ch, 16) >= 0))
    .flatMap { normalized =>
      scala.util.Try {
        val red   = Integer.parseInt(normalized.substring(0, 2), 16)
        val green = Integer.parseInt(normalized.substring(2, 4), 16)
        val blue  = Integer.parseInt(normalized.substring(4, 6), 16)
        val alpha = if normalized.length == 8 then Integer.parseInt(normalized.substring(6, 8), 16) else 255
        Color(red, green, blue, alpha)
      }.toOption
    }

private def formatColor(color: Color): String =
  ColorFormat.toHex(color, withAlpha = true)
