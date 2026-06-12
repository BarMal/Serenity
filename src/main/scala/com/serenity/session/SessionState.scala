package com.serenity.session

import java.awt.{Color, Font}
import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.animation.AnimationConfig
import com.serenity.config.*
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.Layout
import com.serenity.ui.theme.Theme
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

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
    recentFiles: List[String] = Nil
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
    findState: Option[SessionFindState] = None
)

/** Persistent layout information
  */
case class SessionLayout(
    editorPanes: List[SessionEditorPane],
    activeEditorPaneId: Option[Int],
    paneOrder: List[Int] = Nil
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

object SessionState:

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
    SessionBuffer(
      id = buffer.id.value,
      filePath = buffer.filePath.map(_.toString),
      isDirty = buffer.isDirty,
      language = buffer.language.map(_.id),
      isNewEmpty = buffer.isNewEmpty,
      cursors = buffer.cursors.map(SessionCursorPosition.fromCursorPosition),
      viewport = SessionViewport.fromViewport(buffer.viewport),
      unsavedContent =
        if persistUnsaved || (!buffer.isDirty && !buffer.isNewEmpty) then Some(buffer.content.toString)
        else None,
      findState = buffer.findState.map(SessionFindState.fromFindState)
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
      findState = sessionBuffer.findState.map(SessionFindState.toFindState)
    )

  def toBufferIO(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): IO[Buffer] =
    import com.serenity.rope.Rope
    import java.nio.file.{Files, Paths}

    sessionBuffer.unsavedContent match
      case Some(_) =>
        IO.pure(toBuffer(sessionBuffer))
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
      paneOrder = layout.paneOrder.map(_.value)
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
      paneOrder = sessionLayout.paneOrder.map(PaneId.apply)
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

// Circe codecs for all types
// First encode the basic dependencies
given Encoder[FiniteDuration] = Encoder.encodeLong.contramap(_.toNanos)
given Decoder[FiniteDuration] = Decoder.decodeLong.map(scala.concurrent.duration.Duration.fromNanos)

given Encoder[AnimationConfig] = deriveEncoder
given Decoder[AnimationConfig] = deriveDecoder

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

given Encoder[InterfaceDensity] = Encoder.encodeString.contramap(_.toString)

given Decoder[InterfaceDensity] = Decoder.decodeString.emap {
  case "Compact"     => Right(InterfaceDensity.Compact)
  case "Comfortable" => Right(InterfaceDensity.Comfortable)
  case "Spacious"    => Right(InterfaceDensity.Spacious)
  case other         => Left(s"Unknown InterfaceDensity: $other")
}

given Encoder[PreferredWindowSize] = deriveEncoder
given Decoder[PreferredWindowSize] = deriveDecoder

given Encoder[BackgroundStyle] = Encoder.encodeString.contramap(_.toString)

given Decoder[BackgroundStyle] = Decoder.decodeString.emap {
  case "Solid"       => Right(BackgroundStyle.Solid)
  case "Transparent" => Right(BackgroundStyle.Transparent)
  case "Frosted"     => Right(BackgroundStyle.Frosted)
  case "GlassLike"   => Right(BackgroundStyle.GlassLike)
  case other         => Left(s"Unknown BackgroundStyle: $other")
}

given Encoder[Color] = Encoder.encodeString.contramap(formatColor)

given Decoder[Color] = Decoder.decodeString.emap(value => parseColor(value).toRight(s"Invalid colour value: $value"))

given Encoder[CursorColorConfig] = deriveEncoder
given Decoder[CursorColorConfig] = deriveDecoder

given Encoder[AppConfig] = deriveEncoder

given Decoder[AppConfig] = Decoder.instance { cursor =>
  for
    characterAnimation        <- cursor.get[Option[AnimationConfig]]("characterAnimation")
    syntaxHighlightingEnabled <- cursor.get[Boolean]("syntaxHighlightingEnabled")
    hotkeyConfig              <- cursor.getOrElse[HotkeyConfig]("hotkeyConfig")(HotkeyConfig())
    focusedKeymapConfig       <- cursor.getOrElse[FocusedKeymapConfig]("focusedKeymapConfig")(FocusedKeymapConfig())
    fontConfig                <- cursor.get[FontConfig]("fontConfig")
    minimumPaneWidth          <- cursor.get[Int]("minimumPaneWidth")
    showLineNumbers           <- cursor.get[Boolean]("showLineNumbers")
    showGutter                <- cursor.get[Boolean]("showGutter")
    blurRadius                <- cursor.getOrElse[Float]("blurRadius")(0.0f)
    backgroundStyle           <- cursor.getOrElse[BackgroundStyle]("backgroundStyle")(BackgroundStyle.Frosted)
    cursorMode                <- cursor.getOrElse[CursorMode]("cursorMode")(CursorMode.Blink)
    cursorColors              <- cursor.getOrElse[CursorColorConfig]("cursorColors")(CursorColorConfig())
    windowChromeMode          <- cursor.getOrElse[WindowChromeMode]("windowChromeMode")(WindowChromeMode.Native)
    markdownViewMode          <- cursor.getOrElse[MarkdownViewMode]("markdownViewMode")(MarkdownViewMode.Source)
    interfaceDensity          <- cursor.getOrElse[InterfaceDensity]("interfaceDensity")(InterfaceDensity.Comfortable)
    preferredWindowSize       <- cursor.getOrElse[Option[PreferredWindowSize]]("preferredWindowSize")(None)
  yield AppConfig(
    characterAnimation = characterAnimation,
    syntaxHighlightingEnabled = syntaxHighlightingEnabled,
    hotkeyConfig = hotkeyConfig,
    focusedKeymapConfig = focusedKeymapConfig,
    fontConfig = fontConfig,
    minimumPaneWidth = minimumPaneWidth,
    showLineNumbers = showLineNumbers,
    showGutter = showGutter,
    blurRadius = blurRadius,
    backgroundStyle = backgroundStyle,
    cursorMode = cursorMode,
    cursorColors = cursorColors,
    windowChromeMode = windowChromeMode,
    markdownViewMode = markdownViewMode,
    interfaceDensity = interfaceDensity,
    preferredWindowSize = preferredWindowSize
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
given Decoder[SessionState] = deriveDecoder

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
