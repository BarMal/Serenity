package com.serenity.session

import java.awt.{Color, Font}

import scala.concurrent.duration.FiniteDuration

import cats.syntax.all.*
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
given Decoder[CursorMode] =
  Decoder.decodeString.emap(value => CursorMode.fromConfigKey(value).toRight(s"Unknown CursorMode: $value"))

given Encoder[StatusSegment] = configKeyEncoder(_.configKey)
given Decoder[StatusSegment] = configKeyDecoder("StatusSegment", StatusSegment.values, _.configKey)

given Encoder[StatusLinePlacement] = configKeyEncoder(_.configKey)

given Decoder[StatusLinePlacement] =
  configKeyDecoder("StatusLinePlacement", StatusLinePlacement.values, _.configKey)

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

given Encoder[Color] = Encoder.encodeString.contramap(formatColor)

given Decoder[Color] = Decoder.decodeString.emap(value => parseColor(value).toRight(s"Invalid colour value: $value"))

given Encoder[CursorColorConfig] = deriveEncoder
given Decoder[CursorColorConfig] = deriveDecoder

given Encoder[StatusLineColors] = deriveEncoder
given Decoder[StatusLineColors] = deriveDecoder

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

given Encoder[InlineAtom] = Encoder.instance {
  case InlineAtom.SoftBreak => Json.fromString("SoftBreak")
  case InlineAtom.Opaque(raw, visible) =>
    Json.obj("opaque" -> Json.fromString(raw), "visible" -> Json.fromBoolean(visible))
  case InlineAtom.Block(raw, feature) =>
    Json.obj("block" -> Json.fromString(raw), "feature" -> Json.fromString(DocumentFeature.key(feature)))
}

given Decoder[InlineAtom] = Decoder.instance { cursor =>
  cursor.as[String] match
    case Right("SoftBreak") => Right(InlineAtom.SoftBreak)
    case Right(other)       => Left(DecodingFailure(s"Unknown InlineAtom: $other", cursor.history))
    case Left(_) if cursor.downField("block").succeeded =>
      for
        raw <- cursor.get[String]("block")
        feature <- cursor
          .get[String]("feature")
          .flatMap(key =>
            DocumentFeature.fromKey(key).toRight(DecodingFailure(s"Unknown DocumentFeature: $key", cursor.history))
          )
      yield InlineAtom.Block(raw, feature)
    case Left(_) =>
      for
        raw     <- cursor.get[String]("opaque")
        visible <- cursor.getOrElse[Boolean]("visible")(true)
      yield InlineAtom.Opaque(raw, visible)
}

given Encoder[FidelityItem] = Encoder.instance(item =>
  Json.obj(
    "feature"   -> Json.fromString(DocumentFeature.key(item.feature)),
    "treatment" -> Json.fromString(Treatment.key(item.treatment)),
    "count"     -> Json.fromInt(item.count)
  )
)

given Decoder[FidelityItem] = Decoder.instance { cursor =>
  for
    feature <- cursor
      .get[String]("feature")
      .flatMap(key =>
        DocumentFeature.fromKey(key).toRight(DecodingFailure(s"Unknown DocumentFeature: $key", cursor.history))
      )
    treatment <- cursor
      .get[String]("treatment")
      .flatMap(key => Treatment.fromKey(key).toRight(DecodingFailure(s"Unknown Treatment: $key", cursor.history)))
    count <- cursor.get[Int]("count")
  yield FidelityItem(feature, treatment, count)
}

given Encoder[FidelityReport] = Encoder.instance(report => Json.obj("items" -> report.items.asJson))
given Decoder[FidelityReport] = Decoder.instance(_.get[List[FidelityItem]]("items").map(FidelityReport(_)))

given Encoder[RawProperty] = deriveEncoder
given Decoder[RawProperty] = deriveDecoder

given Encoder[RichTextStyle] =
  deriveEncoder[RichTextStyle].mapJsonObject(fields =>
    if fields("extras").exists(_.asArray.exists(_.isEmpty)) then fields.remove("extras") else fields
  )

given Decoder[RichTextStyle] = Decoder.instance { cursor =>
  for
    marks      <- cursor.get[Set[InlineMark]]("marks")
    fontFamily <- cursor.get[Option[String]]("fontFamily")
    fontSize   <- cursor.get[Option[Float]]("fontSize")
    color      <- cursor.get[Option[String]]("color")
    link       <- cursor.get[Option[String]]("link")
    extras     <- cursor.getOrElse[List[RawProperty]]("extras")(Nil)
  yield RichTextStyle(marks, fontFamily, fontSize, color, link, extras)
}

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
  case ParagraphRole.DropCap(lines) =>
    io.circe.Json.obj(
      "type"  -> io.circe.Json.fromString("drop_cap"),
      "lines" -> io.circe.Json.fromInt(lines.max(1))
    )
}

given Decoder[ParagraphRole] = Decoder.instance { cursor =>
  cursor.downField("type").as[Option[String]].flatMap {
    case Some("heading") =>
      cursor.downField("level").as[Option[Int]].map(level => ParagraphRole.Heading(level.getOrElse(1).max(1)))
    case Some("drop_cap") =>
      cursor
        .downField("lines")
        .as[Option[Int]]
        .map(lines => ParagraphRole.dropCap(lines.getOrElse(ParagraphRole.DefaultDropCapLines)))
    case Some("body") | None =>
      Right(ParagraphRole.Body)
    case Some(other) =>
      Left(io.circe.DecodingFailure(s"Unknown paragraph role: $other", cursor.history))
  }
}

// Manual because a paragraph's provenance is not saved as it is: only which body block it came from (and whether it was
// split off that block), which is what restoring needs to link it to the package read again from disk.
given Encoder[RichTextParagraph] = Encoder.instance { paragraph =>
  val content = Json.obj(
    "runs"      -> paragraph.runs.asJson,
    "alignment" -> paragraph.alignment.asJson,
    "role"      -> paragraph.role.asJson
  )
  paragraph.source.flatMap(_.originBlock).fold(content) { block =>
    val isDerived = paragraph.source.exists(_.blockIndex == ParagraphSource.NoBlock)
    content.deepMerge(
      Json
        .obj("block" -> Json.fromInt(block))
        .deepMerge(if isDerived then Json.obj("derived" -> Json.True) else Json.obj())
    )
  }
}

given Decoder[RichTextParagraph] = Decoder.instance { cursor =>
  for
    runs      <- cursor.get[List[RichTextRun]]("runs")
    alignment <- cursor.get[ParagraphAlignment]("alignment")
    role      <- cursor.get[ParagraphRole]("role")
    block     <- cursor.get[Option[Int]]("block")
    derived   <- cursor.getOrElse[Boolean]("derived")(false)
  yield RichTextParagraph(
    runs,
    alignment,
    role,
    block.map(index =>
      if derived then ParagraphSource(ParagraphSource.NoBlock, "", Nil, None, Some(index))
      else ParagraphSource(index, "", Nil, None)
    )
  )
}

// Manual, not derived: RichTextDocument is backed by a ParagraphTree (#1663), not a case class, so
// deriveEncoder/deriveDecoder no longer apply -- and even if they did, deriving over the tree would leak its
// internal node shape into the session file format instead of the flat paragraph list every prior save used.
given Encoder[RichTextDocument] = Encoder.instance(document => Json.obj("paragraphs" -> document.paragraphs.asJson))
given Decoder[RichTextDocument] =
  Decoder.instance(_.get[List[RichTextParagraph]]("paragraphs").map(RichTextDocument.apply))

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
