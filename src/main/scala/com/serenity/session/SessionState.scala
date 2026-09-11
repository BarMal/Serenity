package com.serenity.session

import java.nio.file.Path

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.config.*
import com.serenity.state.models.*
import com.serenity.ui.theme.Theme

/** The persisted session file's own schema version -- distinct from [[com.serenity.config.ConfigVersion]], which
  * versions the separate config file format.
  */
opaque type SchemaVersion = Int

object SchemaVersion:
  def apply(value: Int): SchemaVersion = value

  extension (version: SchemaVersion)
    def value: Int = version
    def <=(other: SchemaVersion): Boolean = version <= other

/** Represents the persistent session state that survives application restarts. This is a subset of AppState containing
  * only the information needed to restore the user's workspace.
  */
final case class SessionState(
    buffers: List[SessionBuffer],
    layout: SessionLayout,
    focus: Option[SessionFocus],
    bufferOrder: List[Int], // Use Int IDs instead of BufferId for serialization
    config: AppConfig,
    themeName: String, // Store theme name instead of full theme object
    recentFiles: List[String] = Nil,
    // Keyed by `AppMode.configKey` rather than the enum itself: circe's semi-automatic derivation needs an explicit
    // KeyEncoder/KeyDecoder for a non-string map key, and this reuses the string form the config file already has.
    recentFilesByMode: Map[String, List[String]] = Map.empty,
    schemaVersion: SchemaVersion = SessionState.CurrentSchemaVersion
)

object SessionState:

  /** Schema version 2 adds workspace trees, docked panel snapshots, and maximised-node identity. A version-1 session's
    * `editorPanes` array order (or, if present, its legacy `paneOrder` key -- see `SessionJsonCodecs`) seeds a simple
    * left-to-right split tree at restore time. Invalid version-2 trees fall back to that same seed while preserving
    * buffers and supported panel content.
    */
  val CurrentSchemaVersion: SchemaVersion = SchemaVersion(2)

  def fromAppState(appState: AppState, persistUnsaved: Boolean = true): SessionState =
    SessionState(
      buffers = orderedBuffers(appState).map(SessionBuffer.fromBuffer(_, persistUnsaved)),
      layout = SessionLayout.fromAppState(appState),
      focus = SessionFocus.fromFocus(appState.persisted.focus),
      bufferOrder = appState.persisted.bufferOrder.map(_.value),
      config = appState.persisted.config,
      themeName = appState.persisted.theme.name,
      recentFiles = appState.persisted.recentFiles.map(_.toString),
      recentFilesByMode = appState.persisted.recentFilesByMode.map {
        case (mode, paths) => mode.configKey -> paths.map(_.toString)
      }
    )

  private def orderedBuffers(appState: AppState): List[Buffer] =
    val orderedIds = appState.persisted.bufferOrder.filter(appState.persisted.buffers.contains)
    val missingIds = appState.persisted.buffers.keys.toList
      .filterNot(orderedIds.toSet)
      .sortBy(_.value)

    (orderedIds ++ missingIds).flatMap(appState.persisted.buffers.get)

  def toAppState(sessionState: SessionState, theme: Theme)(using balance: com.serenity.rope.Balance): AppState =
    val bufferMap = sessionState.buffers.map { sessionBuffer =>
      val buffer = SessionBuffer.toBuffer(sessionBuffer)
      BufferId(sessionBuffer.id) -> buffer
    }.toMap

    toAppStateWithBuffers(sessionState, theme, bufferMap)

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
    val restoredLayout = SessionLayout.restore(sessionState.layout, Some(bufferMap.keySet))
    val layout         = restoredLayout.layout
    val focus = sessionState.focus
      .map(SessionFocus.toFocus)
      .filter {
        case Focus.EditorPane(paneId) => layout.editorPanes.contains(paneId)
        case Focus.Surface(surfaceId) => restoredLayout.surfaces.exists(_.id == surfaceId)
        // Unreachable in practice: SessionFocus.toFocus never produces Focus.Modal (modal dialogs aren't persisted,
        // see SessionFocus.fromFocus), but no modal is ever open immediately after a session restore regardless.
        case Focus.Modal => false
      }
      .getOrElse(
        layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(Focus.EditorPane(PaneId(0)))
      )
    val requestedBufferOrder = sessionState.bufferOrder.map(BufferId.apply).filter(bufferMap.contains).distinct
    val bufferOrder =
      requestedBufferOrder ++ bufferMap.keys.toList.filterNot(requestedBufferOrder.contains).sortBy(_.value)

    AppState(
      persisted = Persisted(
        layout = layout,
        buffers = bufferMap,
        bufferOrder = bufferOrder,
        focus = focus,
        theme = theme,
        config = sessionState.config,
        recentFiles = sessionState.recentFiles.map(Path.of(_)),
        recentFilesByMode = sessionState.recentFilesByMode.flatMap {
          case (key, paths) => AppMode.fromConfigKey(key).map(mode => mode -> paths.map(Path.of(_)))
        }
      ),
      runtime = Runtime(
        uiSurfaces = restoredLayout.surfaces,
        nextBufferId = BufferId(bufferMap.keys.map(_.value).maxOption.getOrElse(-1) + 1),
        nextPaneId = PaneId(layout.editorPanes.keys.map(_.value).maxOption.getOrElse(-1) + 1),
        nextSurfaceId = restoredLayout.nextSurfaceId
      )
    )
