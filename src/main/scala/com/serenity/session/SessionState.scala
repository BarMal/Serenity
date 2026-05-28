package com.serenity.session

import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.theme.Theme
import com.serenity.ui.layout.Layout
import com.serenity.animation.AnimationConfig
import com.serenity.ui.fonts.FontLoader.FontConfig
import scala.concurrent.duration.FiniteDuration
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

/**
 * Represents the persistent session state that survives application restarts.
 * This is a subset of AppState containing only the information needed to
 * restore the user's workspace.
 */
case class SessionState(
    buffers: List[SessionBuffer],
    layout: SessionLayout,
    focus: Option[SessionFocus],
    bufferOrder: List[Int], // Use Int IDs instead of BufferId for serialization
    config: AppConfig,
    themeName: String, // Store theme name instead of full theme object
    findState: Option[SessionFindState] = None
)

/**
 * Persistent representation of a buffer
 */
case class SessionBuffer(
    id: Int,
    filePath: Option[String], // Use String instead of Path for JSON serialization
    isDirty: Boolean,
    language: Option[String],
    isNewEmpty: Boolean,
    cursors: List[SessionCursorPosition],
    viewport: SessionViewport,
    // Only persist unsaved content if configured to do so
    unsavedContent: Option[String] = None
)

/**
 * Persistent layout information
 */
case class SessionLayout(
    editorPanes: List[SessionEditorPane],
    activeEditorPaneId: Option[Int]
)

case class SessionEditorPane(
    id: Int,
    bufferId: Option[Int]
)

/**
 * Persistent focus state
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
    visibleLines: Int
)

case class SessionFindState(
    query: String,
    resultLines: List[Int],
    currentIndex: Int
)

object SessionState:
  
  /**
   * Convert AppState to SessionState for persistence
   */
  def fromAppState(appState: AppState): SessionState =
    SessionState(
      buffers = appState.buffers.values.map(SessionBuffer.fromBuffer).toList,
      layout = SessionLayout.fromLayout(appState.layout),
      focus = SessionFocus.fromFocus(appState.focus),
      bufferOrder = appState.bufferOrder.map(_.value),
      config = appState.config,
      themeName = appState.theme.name,
      findState = appState.findState.map(SessionFindState.fromFindState)
    )
  
  /**
   * Convert SessionState back to AppState for restoration
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
    val focus = sessionState.focus.map(SessionFocus.toFocus).getOrElse(
      layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(Focus.EditorPane(PaneId(0)))
    )
    
    AppState(
      layout = layout,
      buffers = bufferMap,
      bufferOrder = sessionState.bufferOrder.map(BufferId.apply),
      focus = focus,
      uiSurfaces = List.empty, // Never restore UI surfaces
      actionStack = Nil, // Never restore action stack
      findState = sessionState.findState.map(SessionFindState.toFindState),
      terminalSize = None, // Will be set when app starts
      theme = theme,
      config = sessionState.config,
      nextBufferId = BufferId(bufferMap.keys.map(_.value).maxOption.getOrElse(-1) + 1),
      nextPaneId = PaneId(layout.editorPanes.keys.map(_.value).maxOption.getOrElse(-1) + 1),
      nextSurfaceId = 0
    )

object SessionBuffer:
  def fromBuffer(buffer: Buffer): SessionBuffer =
    SessionBuffer(
      id = buffer.id.value,
      filePath = buffer.filePath.map(_.toString),
      isDirty = buffer.isDirty,
      language = buffer.language,
      isNewEmpty = buffer.isNewEmpty,
      cursors = buffer.cursors.map(SessionCursorPosition.fromCursorPosition),
      viewport = SessionViewport.fromViewport(buffer.viewport),
      // TODO: Add policy for whether to persist unsaved content
      unsavedContent = if buffer.isDirty && buffer.filePath.isEmpty then Some(buffer.content.toString) else None
    )
  
  def toBuffer(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): Buffer =
    import com.serenity.rope.Rope
    import java.nio.file.Paths
    
    Buffer(
      id = BufferId(sessionBuffer.id),
      content = sessionBuffer.unsavedContent.map(Rope.apply).getOrElse(Rope.empty),
      filePath = sessionBuffer.filePath.map(path => Paths.get(path)),
      isDirty = sessionBuffer.isDirty,
      language = sessionBuffer.language,
      isNewEmpty = sessionBuffer.isNewEmpty,
      cursors = sessionBuffer.cursors.map(SessionCursorPosition.toCursorPosition),
      viewport = SessionViewport.toViewport(sessionBuffer.viewport)
    )

object SessionLayout:
  def fromLayout(layout: Layout): SessionLayout =
    SessionLayout(
      editorPanes = layout.editorPanes.values.map(SessionEditorPane.fromEditorPane).toList,
      activeEditorPaneId = layout.activeEditorPaneId.map(_.value)
    )
  
  def toLayout(sessionLayout: SessionLayout): Layout =
    val editorPanes = sessionLayout.editorPanes.map { sessionPane =>
      val pane = SessionEditorPane.toEditorPane(sessionPane)
      PaneId(sessionPane.id) -> pane
    }.toMap
    
    Layout(
      editorPanes = editorPanes,
      activeEditorPaneId = sessionLayout.activeEditorPaneId.map(PaneId.apply)
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
      case Focus.Surface(_) => None // Don't persist surface focus
  
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
      visibleLines = viewport.visibleLines
    )
  
  def toViewport(sessionViewport: SessionViewport): Viewport =
    Viewport(
      leftColumn = sessionViewport.leftColumn,
      topLine = sessionViewport.topLine,
      visibleColumns = sessionViewport.visibleColumns,
      visibleLines = sessionViewport.visibleLines
    )

object SessionFindState:
  def fromFindState(findState: FindState): SessionFindState =
    SessionFindState(
      query = findState.query,
      resultLines = findState.resultLines,
      currentIndex = findState.currentIndex
    )
  
  def toFindState(sessionFindState: SessionFindState): FindState =
    FindState(
      query = sessionFindState.query,
      resultLines = sessionFindState.resultLines,
      currentIndex = sessionFindState.currentIndex
    )

// Circe codecs for all types
// First encode the basic dependencies
given Encoder[FiniteDuration] = Encoder.encodeLong.contramap(_.toNanos)
given Decoder[FiniteDuration] = Decoder.decodeLong.map(scala.concurrent.duration.Duration.fromNanos)

given Encoder[AnimationConfig] = deriveEncoder
given Decoder[AnimationConfig] = deriveDecoder

given Encoder[FontConfig] = deriveEncoder
given Decoder[FontConfig] = deriveDecoder

given Encoder[AppConfig] = deriveEncoder
given Decoder[AppConfig] = deriveDecoder

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
given Decoder[SessionViewport] = deriveDecoder

given Encoder[SessionFindState] = deriveEncoder
given Decoder[SessionFindState] = deriveDecoder