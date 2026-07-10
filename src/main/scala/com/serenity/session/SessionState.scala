package com.serenity.session

import java.awt.{Color, Font}
import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.{FontConfig, TextScaleMode}
import com.serenity.ui.layout.{Layout, PaneSplitDirection}
import com.serenity.ui.theme.Theme
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.given
import io.circe.{Decoder, DecodingFailure, Encoder}

/** Represents the persistent session state that survives application restarts. This is a subset of AppState containing
  * only the information needed to restore the user's workspace.
  */
case class SessionState(
    buffers: List[SessionBuffer],
    layout: SessionLayout,
    focus: Option[SessionFocus],
    bufferOrder: List[Int], // Use Int IDs instead of BufferId for serialization
    config: AppConfig,
    themeName: String, // Store theme name instead of full theme object
    recentFiles: List[String] = Nil,
    schemaVersion: Int = 1
)

/** Persistent representation of a buffer
  */
case class SessionBuffer(
    id: Int,
    filePath: Option[String], // Use String instead of Path for JSON serialization
    isDirty: Boolean,
    language: Option[String],
    isNewEmpty: Boolean,
    cursors: List[SessionCursorPosition],
    viewport: SessionViewport,
    // Persist buffer text so restore does not depend on disk reads
    unsavedContent: Option[String] = None,
    richTextDocument: Option[RichTextDocument] = None,
    findState: Option[SessionFindState] = None,
    bookmarks: List[SessionCursorPosition] = Nil,
    documentComments: List[SessionDocumentComment] = Nil
)

/** Persistent layout information
  */
case class SessionLayout(
    editorPanes: List[SessionEditorPane],
    activeEditorPaneId: Option[Int],
    paneOrder: List[Int] = Nil,
    splitDirection: String = PaneSplitDirection.Horizontal.toString
)

case class SessionEditorPane(
    id: Int,
    bufferId: Option[Int]
)

/** Persistent focus state
  */
enum SessionFocus:
  case EditorPane(paneId: Int)
  // Note: We don't persist Surface focus as UI surfaces are not persistent

case class SessionCursorPosition(
    line: Int,
    column: Int
)

case class SessionViewport(
    leftColumn: Int,
    topLine: Int,
    visibleColumns: Int,
    visibleLines: Int,
    topVisualLine: Int = 0
)

case class SessionFindState(
    query: String,
    results: List[SessionFindResult],
    currentIndex: Int
)

case class SessionFindResult(
    line: Int,
    column: Int
)

case class SessionDocumentComment(
    anchor: SessionCursorPosition,
    focus: SessionCursorPosition,
    text: String
)

object SessionState:

  val CurrentSchemaVersion: Int = 1

  /** Convert AppState to SessionState for persistence
    */
  def fromAppState(appState: AppState, persistUnsaved: Boolean = true): SessionState =
    SessionState(
      buffers = orderedBuffers(appState).map(SessionBuffer.fromBuffer(_, persistUnsaved)),
      layout = SessionLayout.fromLayout(appState.layout),
      focus = SessionFocus.fromFocus(appState.focus),
      bufferOrder = appState.bufferOrder.map(_.value),
      config = appState.config,
      themeName = appState.theme.name,
      recentFiles = appState.recentFiles.map(_.toString)
    )

  private def orderedBuffers(appState: AppState): List[Buffer] =
    val orderedIds = appState.bufferOrder.filter(appState.buffers.contains)
    val missingIds = appState.buffers.keys.toList
      .filterNot(orderedIds.toSet)
      .sortBy(_.value)

    (orderedIds ++ missingIds).flatMap(appState.buffers.get)

  /** Convert SessionState back to AppState for restoration
    */
  def toAppState(sessionState: SessionState, theme: Theme)(using balance: com.serenity.rope.Balance): AppState =
    // Convert session buffers back to app buffers
    val bufferMap = sessionState.buffers.map { sessionBuffer =>
      val buffer = SessionBuffer.toBuffer(sessionBuffer)
      BufferId(sessionBuffer.id) -> buffer
    }.toMap

    // Convert session layout back to app layout
    val layout = SessionLayout.toLayout(sessionState.layout)

    // Convert focus
    val focus = sessionState.focus
      .map(SessionFocus.toFocus)
      .getOrElse(
        layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(Focus.EditorPane(PaneId(0)))
      )

    AppState(
      layout = layout,
      buffers = bufferMap,
      bufferOrder = sessionState.bufferOrder.map(BufferId.apply),
      focus = focus,
      uiSurfaces = List.empty, // Never restore UI surfaces
      actionStack = Nil,       // Never restore action stack
      viewportSize = None,     // Will be set when app starts
      theme = theme,
      config = sessionState.config,
      recentFiles = sessionState.recentFiles.map(Path.of(_)),
      nextBufferId = BufferId(bufferMap.keys.map(_.value).maxOption.getOrElse(-1) + 1),
      nextPaneId = PaneId(layout.editorPanes.keys.map(_.value).maxOption.getOrElse(-1) + 1),
      nextSurfaceId = 0
    )

  /** Convert SessionState back to AppState for restoration, reading file-backed buffers from disk when older or
    * size-conscious session files do not contain persisted text.
    */
  def toAppStateIO(sessionState: SessionState, theme: Theme)(using balance: com.serenity.rope.Balance): IO[AppState] =
    for buffers <- sessionState.buffers.traverse { sessionBuffer =>
          SessionBuffer.toBufferIO(sessionBuffer).map(buffer => BufferId(sessionBuffer.id) -> buffer)
        }
    yield toAppStateWithBuffers(sessionState, theme, buffers.toMap)

  private def toAppStateWithBuffers(
    sessionState: SessionState,
    theme: Theme,
    bufferMap: Map[BufferId, Buffer]
  ): AppState =
    val layout = SessionLayout.toLayout(sessionState.layout)
    val focus = sessionState.focus
      .map(SessionFocus.toFocus)
      .getOrElse(
        layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(Focus.EditorPane(PaneId(0)))
      )

    AppState(
      layout = layout,
      buffers = bufferMap,
      bufferOrder = sessionState.bufferOrder.map(BufferId.apply),
      focus = focus,
      uiSurfaces = List.empty,
      actionStack = Nil,
      viewportSize = None,
      theme = theme,
      config = sessionState.config,
      recentFiles = sessionState.recentFiles.map(Path.of(_)),
      nextBufferId = BufferId(bufferMap.keys.map(_.value).maxOption.getOrElse(-1) + 1),
      nextPaneId = PaneId(layout.editorPanes.keys.map(_.value).maxOption.getOrElse(-1) + 1),
      nextSurfaceId = 0
    )

object SessionBuffer:

  def fromBuffer(buffer: Buffer, persistUnsaved: Boolean = true): SessionBuffer =
    val text = buffer.content.toString
    SessionBuffer(
      id = buffer.id.value,
      filePath = buffer.filePath.map(_.toString),
      isDirty = buffer.isDirty,
      language = buffer.language.map(_.id),
      isNewEmpty = buffer.isNewEmpty,
      cursors = buffer.cursors.map(SessionCursorPosition.fromCursorPosition),
      viewport = SessionViewport.fromViewport(buffer.viewport),
      unsavedContent =
        if persistUnsaved || (!buffer.isDirty && !buffer.isNewEmpty) then Some(text)
        else None,
      richTextDocument = buffer.richTextDocument.filter(_.matchesPlainText(text)),
      findState = buffer.findState.map(SessionFindState.fromFindState),
      bookmarks = buffer.bookmarks.map(SessionCursorPosition.fromCursorPosition),
      documentComments = buffer.documentComments.map(SessionDocumentComment.fromDocumentComment)
    )

  def toBuffer(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): Buffer =
    import com.serenity.rope.Rope
    import java.nio.file.Paths

    Buffer(
      id = BufferId(sessionBuffer.id),
      content = sessionBuffer.unsavedContent.map(Rope.apply).getOrElse(Rope.empty),
      filePath = sessionBuffer.filePath.map(path => Paths.get(path)),
      isDirty = sessionBuffer.isDirty,
      language = sessionBuffer.language.flatMap(LanguageId.fromString),
      isNewEmpty = sessionBuffer.isNewEmpty,
      cursors = sessionBuffer.cursors.map(SessionCursorPosition.toCursorPosition),
      viewport = SessionViewport.toViewport(sessionBuffer.viewport),
      findState = sessionBuffer.findState.map(SessionFindState.toFindState),
      bookmarks = sessionBuffer.bookmarks.map(SessionCursorPosition.toCursorPosition),
      documentComments = sessionBuffer.documentComments.map(SessionDocumentComment.toDocumentComment),
      richTextDocument = sessionBuffer.richTextDocument
    )

  def toBufferIO(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): IO[Buffer] =
    import com.serenity.rope.Rope
    import java.nio.file.{Files, Paths}

    sessionBuffer.unsavedContent match
      case Some(_) =>
        IO.pure(toBuffer(sessionBuffer))
      case None =>
        sessionBuffer.richTextDocument match
          case Some(document) =>
            IO.pure(toBuffer(sessionBuffer).copy(content = Rope(document.plainText), isDirty = false))
          case None =>
            sessionBuffer.filePath match
              case Some(pathText) =>
                val path = Paths.get(pathText)
                IO.blocking(Files.readString(path))
                  .map(diskContent => toBuffer(sessionBuffer).copy(content = Rope(diskContent), isDirty = false))
                  .handleError(_ => toBuffer(sessionBuffer))
              case None =>
                IO.pure(toBuffer(sessionBuffer))

object SessionLayout:

  def fromLayout(layout: Layout): SessionLayout =
    SessionLayout(
      editorPanes = orderedPanes(layout).map(SessionEditorPane.fromEditorPane),
      activeEditorPaneId = layout.activeEditorPaneId.map(_.value),
      paneOrder = layout.paneOrder.map(_.value),
      splitDirection = layout.splitDirection.toString
    )

  private def orderedPanes(layout: Layout): List[EditorPane] =
    val orderedIds = layout.orderedPaneIds.filter(layout.editorPanes.contains)
    val missingIds = layout.editorPanes.keys.toList
      .filterNot(orderedIds.toSet)
      .sortBy(_.value)

    (orderedIds ++ missingIds).flatMap(layout.editorPanes.get)

  def toLayout(sessionLayout: SessionLayout): Layout =
    val editorPanes = sessionLayout.editorPanes.map { sessionPane =>
      val pane = SessionEditorPane.toEditorPane(sessionPane)
      PaneId(sessionPane.id) -> pane
    }.toMap

    Layout(
      editorPanes = editorPanes,
      activeEditorPaneId = sessionLayout.activeEditorPaneId.map(PaneId.apply),
      paneOrder = sessionLayout.paneOrder.map(PaneId.apply),
      splitDirection = PaneSplitDirection.fromString(sessionLayout.splitDirection)
    )

object SessionEditorPane:

  def fromEditorPane(pane: EditorPane): SessionEditorPane =
    SessionEditorPane(
      id = pane.id.value,
      bufferId = pane.bufferId.map(_.value)
    )

  def toEditorPane(sessionPane: SessionEditorPane): EditorPane =
    EditorPane(
      id = PaneId(sessionPane.id),
      bufferId = sessionPane.bufferId.map(BufferId.apply),
      viewport = Viewport.default,
      cursors = List(CursorPosition(0, 0)),
      centerLine = 0
    )

object SessionFocus:

  def fromFocus(focus: Focus): Option[SessionFocus] =
    focus match
      case Focus.EditorPane(paneId) => Some(SessionFocus.EditorPane(paneId.value))
      case Focus.Surface(_)         => None // Don't persist surface focus

  def toFocus(sessionFocus: SessionFocus): Focus =
    sessionFocus match
      case SessionFocus.EditorPane(paneId) => Focus.EditorPane(PaneId(paneId))

object SessionCursorPosition:
  def fromCursorPosition(cursor: CursorPosition): SessionCursorPosition =
    SessionCursorPosition(cursor.line, cursor.column)

  def toCursorPosition(sessionCursor: SessionCursorPosition): CursorPosition =
    CursorPosition(sessionCursor.line, sessionCursor.column)

object SessionViewport:

  def fromViewport(viewport: Viewport): SessionViewport =
    SessionViewport(
      leftColumn = viewport.leftColumn,
      topLine = viewport.topLine,
      visibleColumns = viewport.visibleColumns,
      visibleLines = viewport.visibleLines,
      topVisualLine = viewport.topVisualLine
    )

  def toViewport(sessionViewport: SessionViewport): Viewport =
    Viewport(
      leftColumn = sessionViewport.leftColumn,
      topLine = sessionViewport.topLine,
      visibleColumns = sessionViewport.visibleColumns,
      visibleLines = sessionViewport.visibleLines,
      topVisualLine = sessionViewport.topVisualLine
    )

object SessionFindState:

  def fromFindState(findState: FindState): SessionFindState =
    SessionFindState(
      query = findState.query,
      results = findState.results.map(SessionFindResult.fromFindResult),
      currentIndex = findState.currentIndex
    )

  def toFindState(sessionFindState: SessionFindState): FindState =
    FindState(
      query = sessionFindState.query,
      results = sessionFindState.results.map(SessionFindResult.toFindResult),
      currentIndex = sessionFindState.currentIndex
    )

object SessionFindResult:

  def fromFindResult(findResult: FindResult): SessionFindResult =
    SessionFindResult(findResult.line, findResult.column)

  def toFindResult(sessionFindResult: SessionFindResult): FindResult =
    FindResult(sessionFindResult.line, sessionFindResult.column)

object SessionDocumentComment:

  def fromDocumentComment(comment: DocumentComment): SessionDocumentComment =
    SessionDocumentComment(
      anchor = SessionCursorPosition.fromCursorPosition(comment.anchor),
      focus = SessionCursorPosition.fromCursorPosition(comment.focus),
      text = comment.text
    )

  def toDocumentComment(sessionComment: SessionDocumentComment): DocumentComment =
    DocumentComment(
      anchor = SessionCursorPosition.toCursorPosition(sessionComment.anchor),
      focus = SessionCursorPosition.toCursorPosition(sessionComment.focus),
      text = sessionComment.text
    )

// Circe codecs for all types
// First encode the basic dependencies
given Encoder[FiniteDuration] = Encoder.encodeLong.contramap(_.toNanos)
given Decoder[FiniteDuration] = Decoder.decodeLong.map(scala.concurrent.duration.Duration.fromNanos)

given Encoder[AnimationConfig] = deriveEncoder
given Decoder[AnimationConfig] = deriveDecoder

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

given Encoder[CursorMode] = Encoder.encodeString.contramap(_.toString)

given Decoder[CursorMode] = Decoder.decodeString.emap {
  case "Blink"   => Right(CursorMode.Blink)
  case "Breathe" => Right(CursorMode.Breathe)
  case other     => Left(s"Unknown CursorMode: $other")
}

given Encoder[CursorInfoBarMode] = Encoder.encodeString.contramap(_.toString)

given Decoder[CursorInfoBarMode] = Decoder.decodeString.emap {
  case "Off"      => Right(CursorInfoBarMode.Off)
  case "Position" => Right(CursorInfoBarMode.Position)
  case "Detailed" => Right(CursorInfoBarMode.Detailed)
  case other      => Left(s"Unknown CursorInfoBarMode: $other")
}

given Encoder[CursorInfoBarPlacement] = Encoder.encodeString.contramap(_.toString)

given Decoder[CursorInfoBarPlacement] = Decoder.decodeString.emap {
  case "Floating"     => Right(CursorInfoBarPlacement.Floating)
  case "PinnedBottom" => Right(CursorInfoBarPlacement.PinnedBottom)
  case other          => Left(s"Unknown CursorInfoBarPlacement: $other")
}

given Encoder[WindowChromeMode] = Encoder.encodeString.contramap(_.toString)

given Decoder[WindowChromeMode] = Decoder.decodeString.emap {
  case "Native" => Right(WindowChromeMode.Native)
  case "Custom" => Right(WindowChromeMode.Custom)
  case other    => Left(s"Unknown WindowChromeMode: $other")
}

given Encoder[MarkdownViewMode] = Encoder.encodeString.contramap(_.toString)

given Decoder[MarkdownViewMode] = Decoder.decodeString.emap {
  case "Source"       => Right(MarkdownViewMode.Source)
  case "SplitPreview" => Right(MarkdownViewMode.SplitPreview)
  case "InlineLens"   => Right(MarkdownViewMode.InlineLens)
  case other          => Left(s"Unknown MarkdownViewMode: $other")
}

given Encoder[DefaultDocumentMode] = Encoder.encodeString.contramap(_.toString)

given Decoder[DefaultDocumentMode] = Decoder.decodeString.emap {
  case "PlainText" => Right(DefaultDocumentMode.PlainText)
  case "Markdown"  => Right(DefaultDocumentMode.Markdown)
  case "RichText"  => Right(DefaultDocumentMode.RichText)
  case other       => Left(s"Unknown DefaultDocumentMode: $other")
}

given Encoder[RenderFpsTarget] = Encoder.encodeString.contramap(_.configKey)

given Decoder[RenderFpsTarget] =
  Decoder.decodeString.emap(value => RenderFpsTarget.fromConfigKey(value).toRight(s"Unknown RenderFpsTarget: $value"))

given Encoder[InterfaceDensity] = Encoder.encodeString.contramap(_.toString)

given Decoder[InterfaceDensity] = Decoder.decodeString.emap {
  case "Compact"     => Right(InterfaceDensity.Compact)
  case "Comfortable" => Right(InterfaceDensity.Comfortable)
  case "Spacious"    => Right(InterfaceDensity.Spacious)
  case other         => Left(s"Unknown InterfaceDensity: $other")
}

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

given Encoder[BackgroundStyle] = Encoder.encodeString.contramap(_.toString)

given Decoder[BackgroundStyle] = Decoder.decodeString.emap {
  case "Solid"       => Right(BackgroundStyle.Solid)
  case "Transparent" => Right(BackgroundStyle.Transparent)
  case "Frosted"     => Right(BackgroundStyle.Frosted)
  case "GlassLike"   => Right(BackgroundStyle.GlassLike)
  case other         => Left(s"Unknown BackgroundStyle: $other")
}

given Encoder[MaterialPreset] = Encoder.encodeString.contramap(_.toString)

given Decoder[MaterialPreset] = Decoder.decodeString.emap {
  case "Solid"   => Right(MaterialPreset.Solid)
  case "Clear"   => Right(MaterialPreset.Clear)
  case "Frosted" => Right(MaterialPreset.Frosted)
  case "Crystal" => Right(MaterialPreset.Crystal)
  case "Custom"  => Right(MaterialPreset.Custom)
  case other     => Left(s"Unknown MaterialPreset: $other")
}

given Encoder[MotionPreset] = Encoder.encodeString.contramap(_.toString)

given Decoder[MotionPreset] = Decoder.decodeString.emap {
  case "Reduced"    => Right(MotionPreset.Reduced)
  case "Subtle"     => Right(MotionPreset.Subtle)
  case "Smooth"     => Right(MotionPreset.Smooth)
  case "Expressive" => Right(MotionPreset.Expressive)
  case "Custom"     => Right(MotionPreset.Custom)
  case other        => Left(s"Unknown MotionPreset: $other")
}

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

given Encoder[Color] = Encoder.encodeString.contramap(formatColor)

given Decoder[Color] = Decoder.decodeString.emap(value => parseColor(value).toRight(s"Invalid colour value: $value"))

given Encoder[CursorColorConfig] = deriveEncoder
given Decoder[CursorColorConfig] = deriveDecoder

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

given Encoder[AppConfig] = Encoder.instance { config =>
  io.circe.Json.obj(
    "characterAnimation"                -> config.characterAnimation.asJson,
    "syntaxHighlightingEnabled"         -> config.syntaxHighlightingEnabled.asJson,
    "hotkeyConfig"                      -> config.hotkeyConfig.asJson,
    "focusedKeymapConfig"               -> config.focusedKeymapConfig.asJson,
    "fontConfig"                        -> config.fontConfig.asJson,
    "minimumPaneWidth"                  -> config.minimumPaneWidth.asJson,
    "showLineNumbers"                   -> config.showLineNumbers.asJson,
    "showGutter"                        -> config.showGutter.asJson,
    "wordWrapEnabled"                   -> config.wordWrapEnabled.asJson,
    "blurRadius"                        -> config.blurRadius.asJson,
    "backgroundStyle"                   -> config.backgroundStyle.asJson,
    "materialPreset"                    -> config.materialPreset.asJson,
    "motionPreset"                      -> config.motionPreset.asJson,
    "elementTransitionSpeedScale"       -> config.elementTransitionSpeedScale.asJson,
    "editorTextTransitionSpeedScale"    -> config.editorTextTransitionSpeedScale.asJson,
    "commandRunnerTransitionSpeedScale" -> config.commandRunnerTransitionSpeedScale.asJson,
    "uiTransitionSpeedScale"            -> config.uiTransitionSpeedScale.asJson,
    "cursorTransitionSpeedScale"        -> config.cursorTransitionSpeedScale.asJson,
    "commandRunnerAnimation"            -> config.commandRunnerAnimation.asJson,
    "uiAnimation"                       -> config.uiAnimation.asJson,
    "commandRunnerVisibleRows"          -> config.commandRunnerVisibleRows.asJson,
    "renderFpsTarget"                   -> config.renderFpsTarget.asJson,
    "editorInsertionTransitionKind"     -> config.editorInsertionTransitionKind.asJson,
    "commandRunnerTransitionKind"       -> config.commandRunnerTransitionKind.asJson,
    "panelOpenTransitionKind"           -> config.panelOpenTransitionKind.asJson,
    "panelCloseTransitionKind"          -> config.panelCloseTransitionKind.asJson,
    "cursorMode"                        -> config.cursorMode.asJson,
    "cursorColors"                      -> config.cursorColors.asJson,
    "cursorInfoBarMode"                 -> config.cursorInfoBarMode.asJson,
    "cursorInfoBarPlacement"            -> config.cursorInfoBarPlacement.asJson,
    "windowChromeMode"                  -> config.windowChromeMode.asJson,
    "markdownViewMode"                  -> config.markdownViewMode.asJson,
    "defaultDocumentMode"               -> config.defaultDocumentMode.asJson,
    "interfaceDensity"                  -> config.interfaceDensity.asJson,
    "uiElementGap"                      -> config.uiElementGap.asJson,
    "uiCornerRadiusPx"                  -> config.uiCornerRadiusPx.asJson,
    "uiOutlineThicknessPx"              -> config.uiOutlineThicknessPx.asJson,
    "textAreaInsets"                    -> config.textAreaInsets.asJson,
    "preferredWindowSize"               -> config.preferredWindowSize.asJson,
    "lspUserConfig"                     -> config.lspUserConfig.asJson,
    "spellCheck"                        -> config.spellCheck.asJson
  )
}

given Decoder[AppConfig] = Decoder.instance { cursor =>
  val defaultConfig = AppConfig.default

  for
    characterAnimation <- cursor.getOrElse[Option[AnimationConfig]]("characterAnimation")(
      defaultConfig.characterAnimation
    )
    syntaxHighlightingEnabled <- cursor.getOrElse[Boolean]("syntaxHighlightingEnabled")(
      defaultConfig.syntaxHighlightingEnabled
    )
    hotkeyConfig        <- cursor.getOrElse[HotkeyConfig]("hotkeyConfig")(HotkeyConfig())
    focusedKeymapConfig <- cursor.getOrElse[FocusedKeymapConfig]("focusedKeymapConfig")(FocusedKeymapConfig())
    fontConfig          <- cursor.getOrElse[FontConfig]("fontConfig")(defaultConfig.fontConfig)
    minimumPaneWidth    <- cursor.getOrElse[Int]("minimumPaneWidth")(defaultConfig.minimumPaneWidth)
    showLineNumbers     <- cursor.getOrElse[Boolean]("showLineNumbers")(defaultConfig.showLineNumbers)
    showGutter          <- cursor.getOrElse[Boolean]("showGutter")(defaultConfig.showGutter)
    wordWrapEnabled     <- cursor.getOrElse[Boolean]("wordWrapEnabled")(true)
    blurRadius          <- cursor.getOrElse[Float]("blurRadius")(0.0f)
    backgroundStyle     <- cursor.getOrElse[BackgroundStyle]("backgroundStyle")(BackgroundStyle.Frosted)
    materialPreset      <- cursor.getOrElse[MaterialPreset]("materialPreset")(MaterialPreset.Frosted)
    motionPreset        <- cursor.getOrElse[MotionPreset]("motionPreset")(MotionPreset.Smooth)
    elementTransitionSpeedScale <- cursor
      .getOrElse[Double]("elementTransitionSpeedScale")(1.0)
      .map(AppConfig.clampElementTransitionSpeedScale)
    editorTextTransitionSpeedScale <- cursor
      .getOrElse[Option[Double]]("editorTextTransitionSpeedScale")(None)
      .map(_.map(AppConfig.clampElementTransitionSpeedScale))
    commandRunnerTransitionSpeedScale <- cursor
      .getOrElse[Option[Double]]("commandRunnerTransitionSpeedScale")(None)
      .map(_.map(AppConfig.clampElementTransitionSpeedScale))
    uiTransitionSpeedScale <- cursor
      .getOrElse[Option[Double]]("uiTransitionSpeedScale")(None)
      .map(_.map(AppConfig.clampElementTransitionSpeedScale))
    cursorTransitionSpeedScale <- cursor
      .getOrElse[Option[Double]]("cursorTransitionSpeedScale")(None)
      .map(_.map(AppConfig.clampElementTransitionSpeedScale))
    commandRunnerAnimation <- cursor.getOrElse[Option[AnimationConfig]]("commandRunnerAnimation")(
      AnimationConfig.smooth
    )
    uiAnimation <- cursor.getOrElse[Option[AnimationConfig]]("uiAnimation")(defaultConfig.uiAnimation)
    commandRunnerVisibleRows <- cursor
      .getOrElse[Option[Int]]("commandRunnerVisibleRows")(None)
      .map(_.map(AppConfig.clampCommandRunnerVisibleRows))
    renderFpsTarget <- cursor.getOrElse[RenderFpsTarget]("renderFpsTarget")(RenderFpsTarget.Fps60)
    editorInsertionTransitionKind <- cursor.getOrElse[TransitionKind]("editorInsertionTransitionKind")(
      TransitionKind.Fade
    )
    commandRunnerTransitionKind <- cursor.getOrElse[Option[TransitionKind]]("commandRunnerTransitionKind")(None)
    panelOpenTransitionKind     <- cursor.getOrElse[Option[TransitionKind]]("panelOpenTransitionKind")(None)
    panelCloseTransitionKind    <- cursor.getOrElse[Option[TransitionKind]]("panelCloseTransitionKind")(None)
    cursorMode                  <- cursor.getOrElse[CursorMode]("cursorMode")(CursorMode.Blink)
    cursorColors                <- cursor.getOrElse[CursorColorConfig]("cursorColors")(CursorColorConfig())
    cursorInfoBarMode           <- cursor.getOrElse[CursorInfoBarMode]("cursorInfoBarMode")(CursorInfoBarMode.Off)
    cursorInfoBarPlacement <- cursor.getOrElse[CursorInfoBarPlacement]("cursorInfoBarPlacement")(
      CursorInfoBarPlacement.Floating
    )
    windowChromeMode     <- cursor.getOrElse[WindowChromeMode]("windowChromeMode")(defaultConfig.windowChromeMode)
    markdownViewMode     <- cursor.getOrElse[MarkdownViewMode]("markdownViewMode")(MarkdownViewMode.Source)
    defaultDocumentMode  <- cursor.getOrElse[DefaultDocumentMode]("defaultDocumentMode")(DefaultDocumentMode.PlainText)
    interfaceDensity     <- cursor.getOrElse[InterfaceDensity]("interfaceDensity")(InterfaceDensity.Comfortable)
    uiElementGap         <- cursor.getOrElse[Int]("uiElementGap")(0)
    uiCornerRadiusPx     <- cursor.getOrElse[Int]("uiCornerRadiusPx")(8)
    uiOutlineThicknessPx <- cursor.getOrElse[Int]("uiOutlineThicknessPx")(defaultConfig.uiOutlineThicknessPx)
    textAreaInsets       <- cursor.getOrElse[TextAreaInsets]("textAreaInsets")(TextAreaInsets())
    preferredWindowSize  <- cursor.getOrElse[Option[PreferredWindowSize]]("preferredWindowSize")(None)
    lspUserConfig        <- cursor.getOrElse[LspUserConfig]("lspUserConfig")(LspUserConfig.empty)
    spellCheck           <- cursor.getOrElse[SpellCheckConfig]("spellCheck")(SpellCheckConfig())
  yield AppConfig(
    characterAnimation = characterAnimation,
    syntaxHighlightingEnabled = syntaxHighlightingEnabled,
    hotkeyConfig = hotkeyConfig,
    focusedKeymapConfig = focusedKeymapConfig,
    fontConfig = fontConfig,
    minimumPaneWidth = minimumPaneWidth,
    showLineNumbers = showLineNumbers,
    showGutter = showGutter,
    wordWrapEnabled = wordWrapEnabled,
    blurRadius = blurRadius,
    backgroundStyle = backgroundStyle,
    materialPreset = materialPreset,
    motionPreset = motionPreset,
    elementTransitionSpeedScale = elementTransitionSpeedScale,
    editorTextTransitionSpeedScale = editorTextTransitionSpeedScale,
    commandRunnerTransitionSpeedScale = commandRunnerTransitionSpeedScale,
    uiTransitionSpeedScale = uiTransitionSpeedScale,
    cursorTransitionSpeedScale = cursorTransitionSpeedScale,
    commandRunnerAnimation = commandRunnerAnimation,
    uiAnimation = uiAnimation,
    commandRunnerVisibleRows = commandRunnerVisibleRows,
    renderFpsTarget = renderFpsTarget,
    editorInsertionTransitionKind = editorInsertionTransitionKind,
    commandRunnerTransitionKind = commandRunnerTransitionKind,
    panelOpenTransitionKind = panelOpenTransitionKind,
    panelCloseTransitionKind = panelCloseTransitionKind,
    cursorConfig = CursorConfig(
      mode = cursorMode,
      colors = cursorColors,
      infoBarMode = cursorInfoBarMode,
      infoBarPlacement = cursorInfoBarPlacement
    ),
    windowConfig = WindowConfig(
      chromeMode = windowChromeMode,
      preferredSize = preferredWindowSize
    ),
    documentConfig = DocumentConfig(
      markdownViewMode = markdownViewMode,
      defaultMode = defaultDocumentMode
    ),
    interfaceConfig = InterfaceConfig(
      density = interfaceDensity,
      elementGap = uiElementGap,
      cornerRadiusPx = uiCornerRadiusPx,
      outlineThicknessPx = uiOutlineThicknessPx
    ),
    textAreaInsets = textAreaInsets,
    lspUserConfig = lspUserConfig,
    spellCheck = spellCheck.normalized
  )
}

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
  val rgb = f"#${color.getRed}%02X${color.getGreen}%02X${color.getBlue}%02X"
  if color.getAlpha == 255 then rgb else f"$rgb${color.getAlpha}%02X"

given Encoder[SessionState] = deriveEncoder

given Encoder[SessionBuffer] = deriveEncoder
given Decoder[SessionBuffer] = deriveDecoder

given Encoder[SessionLayout] = deriveEncoder
given Decoder[SessionLayout] = deriveDecoder

given Encoder[SessionEditorPane] = deriveEncoder
given Decoder[SessionEditorPane] = deriveDecoder

given Encoder[SessionFocus] = deriveEncoder
given Decoder[SessionFocus] = deriveDecoder

given Encoder[SessionCursorPosition] = deriveEncoder
given Decoder[SessionCursorPosition] = deriveDecoder

given Encoder[SessionViewport] = deriveEncoder

given Decoder[SessionViewport] = Decoder.instance { cursor =>
  for
    leftColumn    <- cursor.downField("leftColumn").as[Int]
    topLine       <- cursor.downField("topLine").as[Int]
    visibleCols   <- cursor.downField("visibleColumns").as[Int]
    visibleLines  <- cursor.downField("visibleLines").as[Int]
    topVisualLine <- cursor.downField("topVisualLine").as[Option[Int]]
  yield SessionViewport(leftColumn, topLine, visibleCols, visibleLines, topVisualLine.getOrElse(0))
}

given Encoder[SessionFindResult] = deriveEncoder
given Decoder[SessionFindResult] = deriveDecoder

given Encoder[SessionDocumentComment] = deriveEncoder
given Decoder[SessionDocumentComment] = deriveDecoder

given Encoder[SessionFindState] = deriveEncoder

given Decoder[SessionFindState] = Decoder.instance { cursor =>
  for
    query        <- cursor.downField("query").as[String]
    currentIndex <- cursor.downField("currentIndex").as[Int]
    results      <- cursor.downField("results").as[Option[List[SessionFindResult]]]
    resultLines  <- cursor.downField("resultLines").as[Option[List[Int]]]
  yield SessionFindState(
    query = query,
    results = results.getOrElse(resultLines.getOrElse(Nil).map(line => SessionFindResult(line, 0))),
    currentIndex = currentIndex
  )
}

given Decoder[SessionState] = Decoder.instance { cursor =>
  for
    schemaVersion <- cursor.getOrElse[Int]("schemaVersion")(SessionState.CurrentSchemaVersion)
    _ <- Either.cond(
      schemaVersion <= SessionState.CurrentSchemaVersion,
      (),
      DecodingFailure(
        s"Unsupported session schema version: $schemaVersion (current: ${SessionState.CurrentSchemaVersion})",
        cursor.history
      )
    )
    buffers     <- cursor.get[List[SessionBuffer]]("buffers")
    layout      <- cursor.get[SessionLayout]("layout")
    focus       <- cursor.get[Option[SessionFocus]]("focus")
    bufferOrder <- cursor.get[List[Int]]("bufferOrder")
    config      <- cursor.get[AppConfig]("config")
    themeName   <- cursor.get[String]("themeName")
    recentFiles <- cursor.getOrElse[List[String]]("recentFiles")(Nil)
  yield SessionState(
    buffers = buffers,
    layout = layout,
    focus = focus,
    bufferOrder = bufferOrder,
    config = config,
    themeName = themeName,
    recentFiles = recentFiles,
    schemaVersion = schemaVersion
  )
}
