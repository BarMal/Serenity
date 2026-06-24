package com.serenity.state.models

import com.serenity.animation.AnimationState
import com.serenity.config.*
import com.serenity.lsp.model.Diagnostic
import com.serenity.ui.layout.{Layout, ViewportSize}
import com.serenity.ui.theme.Theme

enum SurfacePhase:
  case BufferFadingOut // surface not rendered; buffer chars in overlay area fading out
  case Visible         // surface rendered (may have fade-in animation)
  case Exiting         // ghost surface fading out; focus already restored

case class SurfaceAnimationState(
    phase: SurfacePhase = SurfacePhase.Visible,
    animationState: AnimationState = AnimationState.empty,
    overlayHeight: Int = 0,    // rows in overlay (excluding border) for building fade-in
    bufferFadeLength: Int = 0, // ticks to stay in BufferFadingOut before transitioning
    phaseTick: Int = 0         // ticks elapsed in current phase
)

case class FindResult(line: Int, column: Int)

case class FindResultSet private (
    query: String,
    results: List[FindResult],
    currentIndex: Int
):
  def selectedResult: Option[FindResult] =
    if results.isEmpty then None else results.lift(currentIndex)

  def move(delta: Int): FindResultSet =
    FindResultSet.normalized(query, results, currentIndex + delta)

  def selectionSummary: String =
    selectedResult match
      case Some(result) =>
        s"$matchCountLabel, ${currentIndex + 1}/${results.length} at ${result.line + 1}:${result.column + 1}"
      case None =>
        matchCountLabel

  def visibleResults(maxResults: Int): List[(FindResult, Int)] =
    if maxResults <= 0 || results.isEmpty then Nil
    else
      val windowSize = math.min(maxResults, results.length)
      val halfWindow = windowSize / 2
      val maxStart   = results.length - windowSize
      val start      = math.max(0, math.min(currentIndex - halfWindow, maxStart))
      results.zipWithIndex.slice(start, start + windowSize)

  private def matchCountLabel: String =
    results.length match
      case 1     => "1 match"
      case count => s"$count matches"

object FindResultSet:
  val empty: FindResultSet = FindResultSet("", Nil, 0)

  def normalized(query: String, results: List[FindResult], requestedIndex: Int): FindResultSet =
    if query.isEmpty then empty
    else FindResultSet(query, results, wrapIndex(requestedIndex, results.length))

  private def wrapIndex(index: Int, resultCount: Int): Int =
    if resultCount <= 0 then 0
    else
      val raw = index % resultCount
      if raw < 0 then raw + resultCount else raw

case class FindState(
    query: String,
    results: List[FindResult],
    currentIndex: Int
):
  def resultSet: FindResultSet =
    FindResultSet.normalized(query, results, currentIndex)

object FindState:
  def fromResultSet(resultSet: FindResultSet): FindState =
    FindState(resultSet.query, resultSet.results, resultSet.currentIndex)

case class ThemeTransition(previousTheme: Theme, currentStep: Int, totalSteps: Int):
  def progress: Double         = if totalSteps <= 0 then 1.0 else currentStep.toDouble / totalSteps
  def advance: ThemeTransition = copy(currentStep = currentStep + 1)
  def isComplete: Boolean      = currentStep >= totalSteps

case class NavigationPoint(
    paneId: PaneId,
    bufferId: BufferId,
    cursor: CursorPosition
)

case class HoveredEditorTarget(
    paneId: PaneId,
    bufferId: BufferId,
    cursor: CursorPosition
)

case class SpellCheckFingerprint(
    contentIdentity: Int,
    contentWeight: Int,
    contentNewlineCount: Int,
    contentLastLineLength: Int,
    usesTextFont: Boolean,
    config: SpellCheckConfig
)

object SpellCheckFingerprint:

  def from(buffer: Buffer, config: SpellCheckConfig): SpellCheckFingerprint =
    SpellCheckFingerprint(
      contentIdentity = System.identityHashCode(buffer.content),
      contentWeight = buffer.content.weight,
      contentNewlineCount = buffer.content.newlineCount,
      contentLastLineLength = buffer.content.lastLineLength,
      usesTextFont = buffer.usesTextFont,
      config = config.normalized
    )

case class SpellCheckCacheEntry(
    fingerprint: SpellCheckFingerprint,
    diagnostics: List[Diagnostic]
)

case class AppState(
    layout: Layout,
    buffers: Map[BufferId, Buffer],
    bufferOrder: List[BufferId] = List.empty, // Tracks buffer creation and navigation order
    focus: Focus,
    uiSurfaces: List[UiSurface] = List.empty,
    actionStack: List[AppAction] = Nil,
    viewportSize: Option[ViewportSize] = None,
    theme: Theme = Theme.default,
    config: AppConfig = AppConfig.default,
    nextBufferId: BufferId = BufferId(0),
    nextPaneId: PaneId = PaneId(0),
    nextSurfaceId: Int = 0,
    themeTransition: Option[ThemeTransition] = None,
    surfaceAnimations: Map[SurfaceId, SurfaceAnimationState] = Map.empty,
    clipboard: Option[String] = None, // not persisted between sessions
    recentFiles: List[java.nio.file.Path] = Nil,
    diagnostics: Map[String, List[Diagnostic]] = Map.empty,
    spellCheckCache: Map[String, SpellCheckCacheEntry] = Map.empty,
    focusHistory: List[Focus] = List.empty,
    navigationBackStack: List[NavigationPoint] = Nil,
    navigationForwardStack: List[NavigationPoint] = Nil,
    hoveredEditorTarget: Option[HoveredEditorTarget] = None
):
  /** Convenience accessor for syntax highlighting setting */
  def syntaxHighlightingEnabled: Boolean = config.syntaxHighlightingEnabled
  def isValid: Boolean                   = validationErrors.isEmpty

  /** Cursor position for the currently active editor pane, if any. */
  def activeCursorPosition: Option[CursorPosition] =
    layout.activeEditorPaneId
      .flatMap(layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(buffers.get)
      .flatMap(_.cursors.headOption)

  def cursorInfoBarSurface: Option[UiSurface] =
    config.cursorInfoBarMode match
      case CursorInfoBarMode.Off => None
      case mode =>
        config.cursorInfoBarPlacement match
          case CursorInfoBarPlacement.Floating =>
            for
              paneId   <- layout.activeEditorPaneId
              pane     <- layout.editorPanes.get(paneId)
              bufferId <- pane.bufferId
              buffer   <- buffers.get(bufferId)
              cursor   <- buffer.cursors.headOption
            yield UiSurface(
              id = SurfaceId("cursor-info-bar"),
              content = SurfaceContent.CursorInfoBar(formatCursorInfoBarText(mode, cursor, buffer)),
              presentation = SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
            )
          case CursorInfoBarPlacement.PinnedBottom =>
            None

  def cursorInfoBarText: Option[String] =
    config.cursorInfoBarMode match
      case CursorInfoBarMode.Off => None
      case mode =>
        for
          paneId   <- layout.activeEditorPaneId
          pane     <- layout.editorPanes.get(paneId)
          bufferId <- pane.bufferId
          buffer   <- buffers.get(bufferId)
          cursor   <- buffer.cursors.headOption
        yield formatCursorInfoBarText(mode, cursor, buffer)

  private def formatCursorInfoBarText(mode: CursorInfoBarMode, cursor: CursorPosition, buffer: Buffer): String =
    val position = s"Line ${cursor.line + 1}, Col ${cursor.column + 1}"
    mode match
      case CursorInfoBarMode.Off =>
        ""
      case CursorInfoBarMode.Position =>
        position
      case CursorInfoBarMode.Detailed =>
        val language = buffer.language.fold("Plain Text")(_.displayName)
        val fileName = buffer.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Unsaved")
        s"$position | $language | $fileName"

  def floatingSurfaces: List[UiSurface] =
    uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, _) => true
        case _                                  => false
    }

  def pinnedSurfaces: List[UiSurface] =
    val storedPinned = uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Pinned(_, _) => true
        case _                                => false
    }
    storedPinned ++ cursorInfoBarSurface.filter {
      _.presentation match
        case SurfacePresentation.Pinned(_, _) => true
        case _                                => false
    }

  def expandedPanelSurface: Option[UiSurface] =
    uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }

  def surfaceById(surfaceId: SurfaceId): Option[UiSurface] =
    uiSurfaces.find(_.id == surfaceId).orElse(cursorInfoBarSurface.filter(_.id == surfaceId))

  def activeSurface: Option[UiSurface] =
    focus match
      case Focus.Surface(surfaceId) => surfaceById(surfaceId)
      case _                        => None

  def commandRunnerSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.CommandPalette(_) => true
      case _                                => false
    }

  def commandRunnerSubmenuSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.CommandPaletteSubmenu(_, _, _) => true
      case _                                             => false
    }

  def commandRunnerDomainSurfaceIds: Set[SurfaceId] =
    Set.from(List(commandRunnerSurface.map(_.id), commandRunnerSubmenuSurface.map(_.id)).flatten)

  def hasCommandRunnerDomain: Boolean =
    commandRunnerDomainSurfaceIds.nonEmpty

  def isCommandRunnerDomainFocus(currentFocus: Focus = focus): Boolean =
    currentFocus match
      case Focus.Surface(surfaceId) => commandRunnerDomainSurfaceIds.contains(surfaceId)
      case _                        => false

  def preferredCommandRunnerFocus: Option[Focus] =
    commandRunnerSubmenuSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPaletteSubmenu(_, _, previewOnly) if !previewOnly =>
            commandRunnerSubmenuSurface.map(surface => Focus.Surface(surface.id))
          case _ =>
            None
      }
      .orElse(commandRunnerSurface.map(surface => Focus.Surface(surface.id)))

  def themePickerSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.ThemePicker(_) => true
      case _                             => false
    }

  def fileSearchSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.FileSearch(_) => true
      case _                            => false
    }

  def contextMenuSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.ContextMenu(_) => true
      case _                             => false
    }

  def commentLensSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.CommentLens(_) => true
      case _                             => false
    }

  def startPageSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.StartPage(_) => true
      case _                           => false
    }

  def modalSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false
    }

  def peekSurface: Option[UiSurface] =
    uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) => true
        case _                                                             => false
    }

  private def findSurface(matches: SurfaceContent => Boolean): Option[UiSurface] =
    uiSurfaces.find(surface => matches(surface.content))

  def allocateSurfaceId: (AppState, SurfaceId) =
    val surfaceId = SurfaceId(s"surface-$nextSurfaceId")
    (copy(nextSurfaceId = nextSurfaceId + 1), surfaceId)

  def pushFocus(newFocus: Focus): AppState =
    val deduplicated = focusHistory.filterNot(_ == focus)
    copy(focus = newFocus, focusHistory = focus :: deduplicated)

  def popFocus: AppState =
    focusHistory match
      case head :: tail =>
        head match
          case Focus.Surface(sid) if surfaceById(sid).isEmpty =>
            copy(focusHistory = tail).popFocus
          case validFocus =>
            copy(focus = validFocus, focusHistory = tail)
      case Nil =>
        val fallback = layout.activeEditorPaneId
          .map(Focus.EditorPane(_))
          .getOrElse(Focus.EditorPane(PaneId(0)))
        copy(focus = fallback)

  /** Get the currently focused buffer ID, if any */
  def focusedBufferId: Option[BufferId] =
    focus match
      case Focus.EditorPane(paneId) =>
        layout.editorPanes.get(paneId).flatMap(_.bufferId)
      case _ => None

  /** Get the next buffer ID in navigation order */
  def nextBufferInOrder(currentBufferId: BufferId): Option[BufferId] =
    if bufferOrder.isEmpty then None
    else
      val currentIndex = bufferOrder.indexOf(currentBufferId)
      if currentIndex == -1 then bufferOrder.headOption
      else
        val nextIndex = (currentIndex + 1) % bufferOrder.size
        Some(bufferOrder(nextIndex))

  /** Get the previous buffer ID in navigation order */
  def previousBufferInOrder(currentBufferId: BufferId): Option[BufferId] =
    if bufferOrder.isEmpty then None
    else
      val currentIndex = bufferOrder.indexOf(currentBufferId)
      if currentIndex == -1 then bufferOrder.headOption
      else
        val prevIndex = (currentIndex - 1 + bufferOrder.size) % bufferOrder.size
        Some(bufferOrder(prevIndex))

  def validationErrors: List[String] =
    val errors = List.newBuilder[String]

    // Focus validation
    focus match
      case Focus.EditorPane(paneId) if !layout.editorPanes.contains(paneId) =>
        errors += s"Focus points to non-existent pane: $paneId"
      case Focus.Surface(surfaceId) if surfaceById(surfaceId).isEmpty =>
        errors += s"Focus points to non-existent surface: $surfaceId"
      case _ => // Valid focus
    // Buffer-Pane consistency
    layout.editorPanes.foreach { (paneId, pane) =>
      pane.bufferId.foreach { bufferId =>
        if !buffers.contains(bufferId) then errors += s"Pane $paneId references non-existent buffer: $bufferId"
      }
    }

    errors.result()

  def validated: Either[List[String], AppState] =
    if isValid then Right(this) else Left(validationErrors)

object AppState:

  def initial(using com.serenity.rope.Balance): AppState =
    val initialBufferId = BufferId(0)
    val initialBuffer   = Buffer.newEmpty(initialBufferId)
    val initialPane     = EditorPane.withBuffer(PaneId(0), initialBufferId)
    val layout = Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0)),
      paneOrder = List(PaneId(0))
    )
    AppState(
      layout = layout,
      buffers = Map(initialBufferId -> initialBuffer),
      bufferOrder = List(initialBufferId),
      focus = Focus.EditorPane(PaneId(0)),
      nextBufferId = BufferId(1),
      nextPaneId = PaneId(1),
      nextSurfaceId = 0
    )

  def empty: AppState =
    AppState(
      layout = Layout.empty,
      buffers = Map.empty,
      focus = Focus.EditorPane(PaneId(0))
    )

enum AppAction:
  case CloseWorkflow(workflow: CloseWorkflowState)
