package com.serenity.session

import java.awt.{Color, Font}
import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope}
import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.{FontConfig, TextScaleMode}
import com.serenity.ui.layout.{
  Layout,
  PaneSplitDirection,
  PanelPosition,
  SplitAxis,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.presets.UiPreset.given
import com.serenity.ui.theme.Theme
import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.given

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
    schemaVersion: Int = 2
)

/** Persistent representation of a buffer
  */
final case class SessionBuffer(
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
    richTextFidelity: Option[RichTextFidelity] = None,
    findState: Option[SessionFindState] = None,
    bookmarks: List[SessionCursorPosition] = Nil,
    documentComments: List[SessionDocumentComment] = Nil
)

/** Persistent layout information
  */
final case class SessionLayout(
    editorPanes: List[SessionEditorPane],
    activeEditorPaneId: Option[Int],
    paneOrder: List[Int] = Nil,
    splitDirection: String = PaneSplitDirection.Horizontal.toString,
    workspaceTree: Option[SessionWorkspaceNode] = None,
    maximizedWorkspaceNodeId: Option[String] = None,
    dockedPanels: List[SessionDockedPanel] = Nil
)

/** Versioned session representation of one workspace-tree node. */
enum SessionWorkspaceNode:
  case EditorLeaf(id: String, paneId: Int)
  case DockedSurface(id: String, surfaceId: String, position: String)

  case Split(
      id: String,
      axis: String,
      ratio: Double,
      first: SessionWorkspaceNode,
      second: SessionWorkspaceNode
  )

/** Persistable docked panel content keyed by the surface identity referenced from the workspace tree. */
final case class SessionDockedPanel(surfaceId: String, panel: UiPreset.PinnedPanel)

final case class SessionEditorPane(
    id: Int,
    bufferId: Option[Int]
)

/** Persistent focus state
  */
enum SessionFocus:
  case EditorPane(paneId: Int)
  // Note: We don't persist Surface focus as UI surfaces are not persistent

final case class SessionCursorPosition(
    line: Int,
    column: Int
)

final case class SessionViewport(
    leftColumn: Int,
    topLine: Int,
    visibleColumns: Int,
    visibleLines: Int,
    topVisualLine: Int = 0
)

final case class SessionFindState(
    query: String,
    results: List[SessionFindResult],
    currentIndex: Int
)

final case class SessionFindResult(
    line: Int,
    column: Int
)

final case class SessionDocumentComment(
    anchor: SessionCursorPosition,
    focus: SessionCursorPosition,
    text: String
)

object SessionState:

  /** Schema version 2 adds workspace trees, docked panel snapshots, and maximised-node identity. Version-1 sessions
    * continue to decode through `paneOrder` and `splitDirection`, which are converted to an equivalent tree in memory.
    * Invalid version-2 trees fall back to that legacy topology while preserving buffers and supported panel content.
    */
  val CurrentSchemaVersion: Int = 2

  /** Convert AppState to SessionState for persistence
    */
  def fromAppState(appState: AppState, persistUnsaved: Boolean = true): SessionState =
    SessionState(
      buffers = orderedBuffers(appState).map(SessionBuffer.fromBuffer(_, persistUnsaved)),
      layout = SessionLayout.fromAppState(appState),
      focus = SessionFocus.fromFocus(appState.persisted.focus),
      bufferOrder = appState.persisted.bufferOrder.map(_.value),
      config = appState.persisted.config,
      themeName = appState.persisted.theme.name,
      recentFiles = appState.persisted.recentFiles.map(_.toString)
    )

  private def orderedBuffers(appState: AppState): List[Buffer] =
    val orderedIds = appState.persisted.bufferOrder.filter(appState.persisted.buffers.contains)
    val missingIds = appState.persisted.buffers.keys.toList
      .filterNot(orderedIds.toSet)
      .sortBy(_.value)

    (orderedIds ++ missingIds).flatMap(appState.persisted.buffers.get)

  /** Convert SessionState back to AppState for restoration
    */
  def toAppState(sessionState: SessionState, theme: Theme)(using balance: com.serenity.rope.Balance): AppState =
    // Convert session buffers back to app buffers
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
        recentFiles = sessionState.recentFiles.map(Path.of(_))
      ),
      runtime = Runtime(
        uiSurfaces = restoredLayout.surfaces,
        nextBufferId = BufferId(bufferMap.keys.map(_.value).maxOption.getOrElse(-1) + 1),
        nextPaneId = PaneId(layout.editorPanes.keys.map(_.value).maxOption.getOrElse(-1) + 1),
        nextSurfaceId = restoredLayout.nextSurfaceId
      )
    )

object SessionBuffer:

  def fromBuffer(buffer: Buffer, persistUnsaved: Boolean = true): SessionBuffer =
    val text = buffer.document.content.toString
    SessionBuffer(
      id = buffer.id.value,
      filePath = buffer.document.filePath.map(_.toString),
      isDirty = buffer.document.isDirty,
      language = buffer.document.language.map(_.id),
      isNewEmpty = buffer.document.isNewEmpty,
      cursors = buffer.editing.cursors.map(SessionCursorPosition.fromCursorPosition),
      viewport = SessionViewport.fromViewport(buffer.viewport),
      unsavedContent =
        if persistUnsaved || (!buffer.document.isDirty && !buffer.document.isNewEmpty) then Some(text)
        else None,
      richTextDocument = buffer.richText.richTextDocument.filter(_.matchesPlainText(text)),
      richTextFidelity = buffer.richText.richTextFidelity,
      findState = buffer.findState.map(SessionFindState.fromFindState),
      bookmarks = buffer.annotations.bookmarks.map(SessionCursorPosition.fromCursorPosition),
      documentComments = buffer.annotations.documentComments.map(SessionDocumentComment.fromDocumentComment)
    )

  def toBuffer(sessionBuffer: SessionBuffer)(using balance: com.serenity.rope.Balance): Buffer =
    import com.serenity.rope.Rope
    import java.nio.file.Paths

    Buffer(
      id = BufferId(sessionBuffer.id),
      document = Document(
        content = sessionBuffer.unsavedContent.map(Rope.apply).getOrElse(Rope.empty),
        filePath = sessionBuffer.filePath.map(path => Paths.get(path)),
        isDirty = sessionBuffer.isDirty,
        language = sessionBuffer.language.flatMap(LanguageId.fromString),
        isNewEmpty = sessionBuffer.isNewEmpty
      ),
      editing = EditingState(cursors = sessionBuffer.cursors.map(SessionCursorPosition.toCursorPosition)),
      viewport = SessionViewport.toViewport(sessionBuffer.viewport),
      findState = sessionBuffer.findState.map(SessionFindState.toFindState),
      annotations = Annotations(
        bookmarks = sessionBuffer.bookmarks.map(SessionCursorPosition.toCursorPosition),
        documentComments = sessionBuffer.documentComments.map(SessionDocumentComment.toDocumentComment)
      ),
      richText = RichTextState(
        richTextDocument = sessionBuffer.richTextDocument,
        richTextFidelity = sessionBuffer.richTextFidelity
      )
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
            val buffer = toBuffer(sessionBuffer)
            IO.pure(buffer.copy(document = buffer.document.copy(content = Rope(document.plainText), isDirty = false)))
          case None =>
            sessionBuffer.filePath match
              case Some(pathText) =>
                val path = Paths.get(pathText)
                IO.blocking(Files.readString(path))
                  .map { diskContent =>
                    val buffer = toBuffer(sessionBuffer)
                    buffer.copy(document = buffer.document.copy(content = Rope(diskContent), isDirty = false))
                  }
                  .handleError(_ => toBuffer(sessionBuffer))
              case None =>
                IO.pure(toBuffer(sessionBuffer))

object SessionLayout:

  final private[session] case class Restored(layout: Layout, surfaces: List[UiSurface], nextSurfaceId: Int)

  def fromAppState(state: AppState): SessionLayout =
    val dockedPanels = state.pinnedSurfaces.flatMap { surface =>
      UiPreset.PinnedPanel.fromSurface(surface).map(SessionDockedPanel(surface.id.value, _))
    }
    val persistedSurfaceIds = dockedPanels.map(panel => SurfaceId(panel.surfaceId)).toSet
    val workspaceTree = state.persisted.layout.workspaceTree
      .filter(_.dockedSurfaceIds.toSet.subsetOf(persistedSurfaceIds))
      .map(tree => fromWorkspaceNode(tree.root))

    fromLayout(state.persisted.layout).copy(
      workspaceTree = workspaceTree,
      maximizedWorkspaceNodeId = state.persisted.layout.maximizedWorkspaceNodeId
        .filter(nodeId => workspaceTree.exists(_ => treeContainsNode(state, nodeId)))
        .map(_.value),
      dockedPanels = dockedPanels
    )

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
    restore(sessionLayout).layout

  private[session] def restore(
    sessionLayout: SessionLayout,
    validBufferIds: Option[Set[BufferId]] = None
  ): Restored =
    val decodedEditorPanes = sessionLayout.editorPanes.map { sessionPane =>
      val pane = SessionEditorPane
        .toEditorPane(sessionPane)
        .copy(bufferId = sessionPane.bufferId.map(BufferId.apply).filter(id => validBufferIds.forall(_.contains(id))))
      PaneId(sessionPane.id) -> pane
    }.toMap
    val editorPanes =
      if decodedEditorPanes.nonEmpty then decodedEditorPanes
      else Map(PaneId(0) -> EditorPane.empty(PaneId(0)))
    val splitDirection = PaneSplitDirection.fromString(sessionLayout.splitDirection)
    val orderedPaneIds =
      val requested = sessionLayout.paneOrder.map(PaneId.apply).filter(editorPanes.contains)
      requested ++ editorPanes.keys.toList.filterNot(requested.contains).sortBy(_.value)
    val surfaces = sessionLayout.dockedPanels.foldLeft(List.empty[UiSurface]) { (restored, persisted) =>
      val surface = persisted.panel.toUiSurface(SurfaceId(persisted.surfaceId))
      if restored.exists(_.id == surface.id) then restored else restored :+ surface
    }
    val pinnedSurfaceIds = surfaces.map(_.id).toSet
    val decodedTree = sessionLayout.workspaceTree
      .flatMap(toWorkspaceNode)
      .map(WorkspaceTree.apply)
      .filter(_.validationErrors(editorPanes.keySet, pinnedSurfaceIds).isEmpty)
    val fallbackTree  = fallbackWorkspaceTree(orderedPaneIds, splitDirection, surfaces)
    val workspaceTree = decodedTree.orElse(fallbackTree)
    val maximized = sessionLayout.maximizedWorkspaceNodeId
      .map(WorkspaceNodeId.apply)
      .filter(nodeId => workspaceTree.exists(_.surfaceIdForNode(nodeId).nonEmpty))
    val activeEditorPaneId = sessionLayout.activeEditorPaneId
      .map(PaneId.apply)
      .filter(editorPanes.contains)
      .orElse(orderedPaneIds.headOption)
    val layout = Layout(
      editorPanes = editorPanes,
      activeEditorPaneId = activeEditorPaneId,
      paneOrder = orderedPaneIds,
      splitDirection = splitDirection,
      workspaceTree = workspaceTree,
      maximizedWorkspaceNodeId = maximized
    )
    Restored(layout, surfaces, nextSurfaceId(surfaces))

  private def fromWorkspaceNode(node: WorkspaceNode): SessionWorkspaceNode =
    node match
      case WorkspaceNode.Leaf(id, paneId) =>
        SessionWorkspaceNode.EditorLeaf(id.value, paneId.value)
      case WorkspaceNode.DockedSurface(id, surfaceId, position) =>
        SessionWorkspaceNode.DockedSurface(id.value, surfaceId.value, position.toString)
      case WorkspaceNode.Split(id, axis, ratio, first, second) =>
        SessionWorkspaceNode.Split(
          id.value,
          axis.toString,
          ratio,
          fromWorkspaceNode(first),
          fromWorkspaceNode(second)
        )

  private def toWorkspaceNode(node: SessionWorkspaceNode): Option[WorkspaceNode] =
    node match
      case SessionWorkspaceNode.EditorLeaf(id, paneId) =>
        Some(WorkspaceNode.Leaf(WorkspaceNodeId(id), PaneId(paneId)))
      case SessionWorkspaceNode.DockedSurface(id, surfaceId, position) =>
        panelPosition(position).map(WorkspaceNode.DockedSurface(WorkspaceNodeId(id), SurfaceId(surfaceId), _))
      case SessionWorkspaceNode.Split(id, axis, ratio, first, second) =>
        for
          splitAxis  <- splitAxis(axis)
          _          <- Option.when(ratio.isFinite)(())
          firstNode  <- toWorkspaceNode(first)
          secondNode <- toWorkspaceNode(second)
        yield WorkspaceNode.Split(
          WorkspaceNodeId(id),
          splitAxis,
          ratio.max(WorkspaceTree.MinimumSplitRatio).min(WorkspaceTree.MaximumSplitRatio),
          firstNode,
          secondNode
        )

  private def fallbackWorkspaceTree(
    paneIds: List[PaneId],
    splitDirection: PaneSplitDirection,
    surfaces: List[UiSurface]
  ): Option[WorkspaceTree] =
    surfaces.zipWithIndex.foldLeft(WorkspaceTree.fromLegacy(paneIds, splitDirection)) {
      case (Some(tree), (surface, index)) =>
        surface.presentation match
          case SurfacePresentation.Pinned(position, _) =>
            tree.dock(
              surface.id,
              position,
              WorkspaceNodeId(s"restored-dock-split-$index"),
              WorkspaceNodeId(s"restored-dock-${surface.id.value}")
            )
          case _ =>
            Some(tree)
      case (None, _) =>
        None
    }

  private def panelPosition(value: String): Option[PanelPosition] =
    value match
      case "Left"   => Some(PanelPosition.Left)
      case "Right"  => Some(PanelPosition.Right)
      case "Top"    => Some(PanelPosition.Top)
      case "Bottom" => Some(PanelPosition.Bottom)
      case _        => None

  private def splitAxis(value: String): Option[SplitAxis] =
    value match
      case "Horizontal" => Some(SplitAxis.Horizontal)
      case "Vertical"   => Some(SplitAxis.Vertical)
      case _            => None

  private def nextSurfaceId(surfaces: List[UiSurface]): Int =
    surfaces
      .flatMap { surface =>
        Option
          .when(surface.id.value.startsWith("surface-"))(surface.id.value.stripPrefix("surface-"))
          .flatMap(_.toIntOption)
      }
      .maxOption
      .getOrElse(-1) + 1

  private def treeContainsNode(state: AppState, nodeId: WorkspaceNodeId): Boolean =
    state.persisted.layout.workspaceTree.exists(_.nodeIds.contains(nodeId))

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

given Encoder[CursorInfoBarMode] = configKeyEncoder(_.configKey)
given Decoder[CursorInfoBarMode] = configKeyDecoder("CursorInfoBarMode", CursorInfoBarMode.values, _.configKey)

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

private def encodeCursorConfig(config: AppConfig): List[(String, Json)] =
  List(
    "cursorMode"             -> config.cursorConfig.mode.asJson,
    "cursorColors"           -> config.cursorConfig.colors.asJson,
    "cursorInfoBarMode"      -> config.cursorConfig.infoBarMode.asJson,
    "cursorInfoBarPlacement" -> config.cursorConfig.infoBarPlacement.asJson
  )

private def encodeWindowConfig(config: AppConfig): List[(String, Json)] =
  List(
    "windowChromeMode"    -> config.windowConfig.chromeMode.asJson,
    "preferredWindowSize" -> config.windowConfig.preferredSize.asJson
  )

private def encodeDocumentConfig(config: AppConfig): List[(String, Json)] =
  List(
    "markdownViewMode"    -> config.documentConfig.markdownViewMode.asJson,
    "defaultDocumentMode" -> config.documentConfig.defaultMode.asJson
  )

private def encodeInterfaceConfig(config: AppConfig): List[(String, Json)] =
  List(
    "interfaceDensity"     -> config.interfaceConfig.density.asJson,
    "uiElementGap"         -> config.interfaceConfig.elementGap.asJson,
    "uiCornerRadiusPx"     -> config.interfaceConfig.cornerRadiusPx.asJson,
    "uiOutlineThicknessPx" -> config.interfaceConfig.outlineThicknessPx.asJson
  )

private def decodeCursorConfig(cursor: HCursor, defaultConfig: AppConfig): Decoder.Result[CursorConfig] =
  for
    mode        <- cursor.getOrElse[CursorMode]("cursorMode")(defaultConfig.cursorConfig.mode)
    colors      <- cursor.getOrElse[CursorColorConfig]("cursorColors")(defaultConfig.cursorConfig.colors)
    infoBarMode <- cursor.getOrElse[CursorInfoBarMode]("cursorInfoBarMode")(defaultConfig.cursorConfig.infoBarMode)
    infoBarPlacement <- cursor.getOrElse[CursorInfoBarPlacement]("cursorInfoBarPlacement")(
      defaultConfig.cursorConfig.infoBarPlacement
    )
  yield CursorConfig(
    mode = mode,
    colors = colors,
    infoBarMode = infoBarMode,
    infoBarPlacement = infoBarPlacement
  )

private def decodeWindowConfig(cursor: HCursor, defaultConfig: AppConfig): Decoder.Result[WindowConfig] =
  for
    chromeMode <- cursor.getOrElse[WindowChromeMode]("windowChromeMode")(defaultConfig.windowConfig.chromeMode)
    preferredSize <- cursor.getOrElse[Option[PreferredWindowSize]]("preferredWindowSize")(
      defaultConfig.windowConfig.preferredSize
    )
  yield WindowConfig(
    chromeMode = chromeMode,
    preferredSize = preferredSize
  )

private def decodeDocumentConfig(cursor: HCursor, defaultConfig: AppConfig): Decoder.Result[DocumentConfig] =
  for
    markdownViewMode <- cursor.getOrElse[MarkdownViewMode]("markdownViewMode")(
      defaultConfig.documentConfig.markdownViewMode
    )
    defaultMode <- cursor.getOrElse[DefaultDocumentMode]("defaultDocumentMode")(
      defaultConfig.documentConfig.defaultMode
    )
  yield DocumentConfig(
    markdownViewMode = markdownViewMode,
    defaultMode = defaultMode
  )

private def decodeInterfaceConfig(cursor: HCursor, defaultConfig: AppConfig): Decoder.Result[InterfaceConfig] =
  for
    density        <- cursor.getOrElse[InterfaceDensity]("interfaceDensity")(defaultConfig.interfaceConfig.density)
    elementGap     <- cursor.getOrElse[Double]("uiElementGap")(defaultConfig.interfaceConfig.elementGap)
    cornerRadiusPx <- cursor.getOrElse[Int]("uiCornerRadiusPx")(defaultConfig.interfaceConfig.cornerRadiusPx)
    outlineThicknessPx <- cursor.getOrElse[Int]("uiOutlineThicknessPx")(
      defaultConfig.interfaceConfig.outlineThicknessPx
    )
  yield InterfaceConfig(
    density = density,
    elementGap = elementGap,
    cornerRadiusPx = cornerRadiusPx,
    outlineThicknessPx = outlineThicknessPx
  )

given Encoder[AppConfig] = Encoder.instance { config =>
  Json.obj(
    (
      List(
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
        "commandRunnerItemGapRows"          -> config.commandRunnerItemGapRows.asJson,
        "commandRunnerCursorGapRows"        -> config.commandRunnerCursorGapRows.asJson,
        "renderFpsTarget"                   -> config.renderFpsTarget.asJson,
        "renderDamageGranularity"           -> config.renderDamageGranularity.asJson,
        "editorInsertionTransitionKind"     -> config.editorInsertionTransitionKind.asJson,
        "commandRunnerTransitionKind"       -> config.commandRunnerTransitionKind.asJson,
        "panelOpenTransitionKind"           -> config.panelOpenTransitionKind.asJson,
        "panelCloseTransitionKind"          -> config.panelCloseTransitionKind.asJson,
        "motionConfiguration"               -> config.motionConfiguration.asJson
      ) ++
        encodeCursorConfig(config) ++
        encodeWindowConfig(config) ++
        encodeDocumentConfig(config) ++
        encodeInterfaceConfig(config) ++
        List(
          "textAreaInsets" -> config.textAreaInsets.asJson,
          "lspUserConfig"  -> config.lspUserConfig.asJson,
          "spellCheck"     -> config.spellCheck.asJson
        )
    )*
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
    commandRunnerItemGapRows <- cursor
      .getOrElse[Double]("commandRunnerItemGapRows")(defaultConfig.commandRunnerItemGapRows)
      .map(AppConfig.clampCommandRunnerItemGapRows)
    commandRunnerCursorGapRows <- cursor
      .getOrElse[Option[Double]]("commandRunnerCursorGapRows")(defaultConfig.commandRunnerCursorGapRows)
      .map(_.map(AppConfig.clampCommandRunnerCursorGapRows))
    renderFpsTarget <- cursor.getOrElse[RenderFpsTarget]("renderFpsTarget")(RenderFpsTarget.Fps60)
    renderDamageGranularity <- cursor
      .getOrElse[RenderDamageGranularity]("renderDamageGranularity")(RenderDamageGranularity.Rows)
    editorInsertionTransitionKind <- cursor.getOrElse[TransitionKind]("editorInsertionTransitionKind")(
      TransitionKind.Fade
    )
    commandRunnerTransitionKind <- cursor.getOrElse[Option[TransitionKind]]("commandRunnerTransitionKind")(None)
    panelOpenTransitionKind     <- cursor.getOrElse[Option[TransitionKind]]("panelOpenTransitionKind")(None)
    panelCloseTransitionKind    <- cursor.getOrElse[Option[TransitionKind]]("panelCloseTransitionKind")(None)
    motionConfiguration         <- cursor.getOrElse[Option[MotionConfig]]("motionConfiguration")(None)
    cursorConfig                <- decodeCursorConfig(cursor, defaultConfig)
    windowConfig                <- decodeWindowConfig(cursor, defaultConfig)
    documentConfig              <- decodeDocumentConfig(cursor, defaultConfig)
    interfaceConfig             <- decodeInterfaceConfig(cursor, defaultConfig)
    textAreaInsets              <- cursor.getOrElse[TextAreaInsets]("textAreaInsets")(TextAreaInsets())
    lspUserConfig               <- cursor.getOrElse[LspUserConfig]("lspUserConfig")(LspUserConfig.empty)
    spellCheck                  <- cursor.getOrElse[SpellCheckConfig]("spellCheck")(SpellCheckConfig())
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
    commandRunnerItemGapRows = commandRunnerItemGapRows,
    commandRunnerCursorGapRows = commandRunnerCursorGapRows,
    renderFpsTarget = renderFpsTarget,
    renderDamageGranularity = renderDamageGranularity,
    editorInsertionTransitionKind = editorInsertionTransitionKind,
    commandRunnerTransitionKind = commandRunnerTransitionKind,
    panelOpenTransitionKind = panelOpenTransitionKind,
    panelCloseTransitionKind = panelCloseTransitionKind,
    motionConfiguration = motionConfiguration,
    cursorConfig = cursorConfig,
    windowConfig = windowConfig,
    documentConfig = documentConfig,
    interfaceConfig = interfaceConfig,
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

given Encoder[SessionLayout] = deriveEncoder

given Encoder[SessionWorkspaceNode] = deriveEncoder
given Decoder[SessionWorkspaceNode] = deriveDecoder

given Encoder[SessionDockedPanel] = deriveEncoder
given Decoder[SessionDockedPanel] = deriveDecoder

given Encoder[SessionEditorPane] = deriveEncoder
given Decoder[SessionEditorPane] = deriveDecoder

given Decoder[SessionLayout] = Decoder.instance { cursor =>
  for
    editorPanes              <- cursor.get[List[SessionEditorPane]]("editorPanes")
    activeEditorPaneId       <- cursor.get[Option[Int]]("activeEditorPaneId")
    paneOrder                <- cursor.getOrElse[List[Int]]("paneOrder")(Nil)
    splitDirection           <- cursor.getOrElse[String]("splitDirection")(PaneSplitDirection.Horizontal.toString)
    workspaceTree            <- cursor.getOrElse[Option[SessionWorkspaceNode]]("workspaceTree")(None)
    maximizedWorkspaceNodeId <- cursor.getOrElse[Option[String]]("maximizedWorkspaceNodeId")(None)
    persistedPanels          <- cursor.getOrElse[List[Json]]("dockedPanels")(Nil)
  yield SessionLayout(
    editorPanes,
    activeEditorPaneId,
    paneOrder,
    splitDirection,
    workspaceTree,
    maximizedWorkspaceNodeId,
    persistedPanels.flatMap(_.as[SessionDockedPanel].toOption)
  )
}

given Encoder[SessionFocus] = deriveEncoder
given Decoder[SessionFocus] = deriveDecoder

given Encoder[SessionBuffer]    = deriveEncoder
given Decoder[SessionBuffer]    = deriveDecoder
given Encoder[RichTextFidelity] = deriveEncoder
given Decoder[RichTextFidelity] = deriveDecoder

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
    schemaVersion <- cursor.getOrElse[Int]("schemaVersion")(1)
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
