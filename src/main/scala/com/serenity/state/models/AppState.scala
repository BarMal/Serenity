package com.serenity.state.models

import com.serenity.animation.{AnimationState, WindowSitter}
import com.serenity.config.*
import com.serenity.lsp.model.Diagnostic
import com.serenity.markdown.MarkdownBlockLens
import com.serenity.rope.Rope
import com.serenity.text.TextEditing
import com.serenity.ui.layout.{Layout, SplitAxis, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.theme.Theme

enum SurfacePhase:
  case BufferFadingOut // surface not rendered; buffer chars in overlay area fading out
  case Visible         // surface rendered (may have fade-in animation)
  case Exiting         // ghost surface fading out; focus already restored

final case class SurfaceAnimationState(
    phase: SurfacePhase = SurfacePhase.Visible,
    animationState: AnimationState = AnimationState.empty,
    overlayHeight: Int = 0,    // rows in overlay (excluding border) for building fade-in
    bufferFadeLength: Int = 0, // ticks to stay in BufferFadingOut before transitioning
    phaseTick: Int = 0         // ticks elapsed in current phase
)

final case class FindResult(line: Int, column: Int)

/** Immutable identity for a background find operation. */
final case class FindSearchRequest(
    surfaceId: SurfaceId,
    bufferId: BufferId,
    query: String,
    content: Rope
)

object FindSearch:

  /** Finds whole-grapheme occurrences in deterministic document order. */
  def results(content: Rope, query: String): List[FindResult] =
    if query.isEmpty then Nil
    else
      content.searchAll(query).collect {
        case offset if TextEditing.isWholeGraphemeRange(RopeCharacterSource(content), offset, offset + query.length) =>
          val (line, column) = content.offsetToLineColumn(offset)
          FindResult(line, column)
      }

  final private case class RopeCharacterSource(content: Rope) extends TextEditing.CharacterSource:
    override def length: Int = content.weight

    override def charAt(index: Int): Char = content.index(index).getOrElse('\u0000')

final case class FindResultSet private (
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

final case class FindState(
    query: String,
    results: List[FindResult],
    currentIndex: Int
):
  def resultSet: FindResultSet =
    FindResultSet.normalized(query, results, currentIndex)

object FindState:
  def fromResultSet(resultSet: FindResultSet): FindState =
    FindState(resultSet.query, resultSet.results, resultSet.currentIndex)

final case class ThemeTransition(previousTheme: Theme, currentStep: Int, totalSteps: Int):
  def progress: Double         = if totalSteps <= 0 then 1.0 else currentStep.toDouble / totalSteps
  def advance: ThemeTransition = copy(currentStep = currentStep + 1)
  def isComplete: Boolean      = currentStep >= totalSteps

final case class NavigationPoint(
    paneId: PaneId,
    bufferId: BufferId,
    cursor: CursorPosition
)

final case class HoveredEditorTarget(
    paneId: PaneId,
    bufferId: BufferId,
    cursor: CursorPosition
)

final case class SpellCheckFingerprint(
    contentIdentity: Int,
    contentWeight: Int,
    contentNewlineCount: Int,
    contentLastLineLength: Int,
    usesTextFont: Boolean,
    dictionaryFingerprints: List[SpellCheckDictionaryFingerprint],
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
      dictionaryFingerprints = config.dictionaryFingerprints,
      config = config.normalized
    )

final case class SpellCheckCacheEntry(
    fingerprint: SpellCheckFingerprint,
    diagnostics: List[Diagnostic]
)

/** Scene-owned interval node for bounded comment overlap queries. */
final private case class CommentIntervalNode(
    comment: DocumentComment,
    maxEnd: Int,
    left: Option[CommentIntervalNode],
    right: Option[CommentIntervalNode]
)

/** Scene-owned annotation lookup keyed by buffer line. */
final case class AnnotationLineIndex(
    comments: Vector[DocumentComment],
    diagnosticsByLine: Map[Int, List[Diagnostic]]
):

  private lazy val commentTree: Option[CommentIntervalNode] =
    def build(sorted: Vector[DocumentComment]): Option[CommentIntervalNode] =
      if sorted.isEmpty then None
      else
        val middle  = sorted.length / 2
        val comment = sorted(middle)
        val left    = build(sorted.take(middle))
        val right   = build(sorted.drop(middle + 1))
        val maxEnd  = (comment.end.line :: left.toList.map(_.maxEnd) ::: right.toList.map(_.maxEnd)).max
        Some(CommentIntervalNode(comment, maxEnd, left, right))
    build(comments.sortBy(_.start.line))

  def commentsByLine(visibleLines: Set[Int]): Map[Int, List[DocumentComment]] =
    if visibleLines.isEmpty then Map.empty
    else
      val start = visibleLines.min
      val end   = visibleLines.max
      def overlapping(node: Option[CommentIntervalNode]): List[DocumentComment] =
        node match
          case None => Nil
          case Some(current) =>
            val fromLeft = if current.left.exists(_.maxEnd >= start) then overlapping(current.left) else Nil
            val here =
              if current.comment.start.line <= end && current.comment.end.line >= start then List(current.comment)
              else Nil
            val fromRight = if current.comment.start.line <= end then overlapping(current.right) else Nil
            fromLeft ::: here ::: fromRight
      overlapping(commentTree).foldLeft(Map.empty[Int, List[DocumentComment]]) { (byLine, comment) =>
        (comment.start.line.max(start) to comment.end.line.min(end)).iterator
          .filter(visibleLines.contains)
          .foldLeft(byLine)((updated, line) => updated.updated(line, comment :: updated.getOrElse(line, Nil)))
      }

final case class AppState(
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
    hoveredEditorTarget: Option[HoveredEditorTarget] = None,
    windowSitter: WindowSitter = WindowSitter.default
):

  /** Lazily indexes annotations for this immutable state snapshot. A new state snapshot gets a fresh index, while
    * repeated render plans for the same scene reuse the existing one.
    */
  lazy val annotationIndexByBuffer: Map[BufferId, () => AnnotationLineIndex] =
    buffers.iterator.map {
      case (bufferId, buffer) =>
        lazy val index =
          val diagnostics = this.diagnostics.getOrElse(com.serenity.spellcheck.SpellChecker.diagnosticsUri(buffer), Nil)
          AnnotationLineIndex(buffer.documentComments.toVector, diagnostics.groupMap(_.range.start.line)(identity))
        bufferId -> (() => index)
    }.toMap

  lazy val markdownFenceIndexByBuffer: Map[BufferId, () => MarkdownBlockLens.FenceRangeIndex] =
    buffers.iterator.map {
      case (bufferId, buffer) =>
        lazy val index = MarkdownBlockLens.fenceRangeIndex(buffer.content.lineCount, buffer.content.getLine)
        bufferId -> (() => index)
    }.toMap

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
    val orderedStored = layout.workspaceTree match
      case Some(tree) =>
        tree.dockedSurfaceIds.flatMap(surfaceId => storedPinned.find(_.id == surfaceId)) ++
          storedPinned.filterNot(surface => tree.dockedSurfaceIds.contains(surface.id))
      case None =>
        storedPinned
    orderedStored ++ cursorInfoBarSurface.filter {
      _.presentation match
        case SurfacePresentation.Pinned(_, _) => true
        case _                                => false
    }

  def expandedPanelSurface: Option[UiSurface] =
    val maximized = for
      tree      <- layout.workspaceTree
      nodeId    <- layout.maximizedWorkspaceNodeId
      surfaceId <- tree.surfaceIdForNode(nodeId)
      surface   <- surfaceById(surfaceId)
    yield surface
    maximized.orElse(uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    })

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

  def themeCreatorSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.ThemeCreator(_) => true
      case _                              => false
    }

  def fileSearchSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.FileSearch(_) => true
      case _                            => false
    }

  def contextualToolbarSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.ContextualToolbar(_) => true
      case _                                   => false
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

  /** Modal surfaces ordered from their parent to the topmost child. */
  def modalSurfaces: List[UiSurface] =
    uiSurfaces.collect { case surface @ UiSurface(_, _, SurfacePresentation.Modal, _) => surface }

  /** Compatibility alias for callers that still name modal ownership as blocking. */
  def blockingModalSurfaces: List[UiSurface] =
    modalSurfaces

  /** The only modal workflow permitted to receive input while a confirmation is open. */
  def topBlockingModalSurface: Option[UiSurface] =
    blockingModalSurfaces.lastOption

  def topModalSurface: Option[UiSurface] =
    modalSurfaces.lastOption

  /** The active modal workflow, retaining modeless workflow lookup during migration. */
  def modalSurface: Option[UiSurface] =
    topModalSurface.orElse(uiSurfaces.reverse.find(isModalWorkflow))

  def hasBlockingModal: Boolean =
    topBlockingModalSurface.nonEmpty

  /** Remove the topmost modal workflow and restore the focus that opened it. */
  def dismissTopModal: AppState =
    topModalSurface.orElse(activeSurface.filter(isModalWorkflow)) match
      case Some(surface) => copy(uiSurfaces = uiSurfaces.filterNot(_.id == surface.id)).popFocus
      case None          => this

  def peekSurface: Option[UiSurface] =
    uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) => true
        case _                                                             => false
    }

  private def findSurface(matches: SurfaceContent => Boolean): Option[UiSurface] =
    uiSurfaces.find(surface => matches(surface.content))

  private def isModalWorkflow(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false

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
    layout.workspaceTree.foreach { tree =>
      val pinnedSurfaceIds = uiSurfaces.collect {
        case UiSurface(id, _, SurfacePresentation.Pinned(_, _), _) => id
      }.toSet
      errors ++= tree.validationErrors(layout.editorPanes.keySet, pinnedSurfaceIds)
      focus match
        case Focus.EditorPane(paneId) if !tree.paneIds.contains(paneId) =>
          errors += s"Focus points outside workspace tree: $paneId"
        case _ =>
      layout.maximizedWorkspaceNodeId.foreach { nodeId =>
        if tree.surfaceIdForNode(nodeId).isEmpty then
          errors += s"Maximised workspace node is not a docked surface: ${nodeId.value}"
      }
    }

    errors.result()

  def validated: Either[List[String], AppState] =
    val reconciled = reconcileWorkspaceTree
    if reconciled.isValid then Right(reconciled) else Left(reconciled.validationErrors)

  private def reconcileWorkspaceTree: AppState =
    layout.workspaceTree match
      case None                                               => this
      case Some(tree) if workspaceTreeAlreadyReconciled(tree) => this
      case Some(tree) =>
        val paneIds = layout.editorPanes.keySet
        val prunedPanes = tree.paneIds
          .filterNot(paneIds.contains)
          .foldLeft(Option(tree)) {
            case (Some(currentTree), paneId) => currentTree.remove(paneId)
            case (None, _)                   => None
          }
        val paneReconciledTree = layout.paneOrder
          .filter(paneIds.contains)
          .foldLeft(prunedPanes) {
            case (Some(currentTree), paneId) if !currentTree.paneIds.contains(paneId) =>
              val splitId = WorkspaceNodeId(s"reconcile-pane-${paneId.value}")
              currentTree
                .split(
                  currentTree.paneIds.lastOption.getOrElse(paneId),
                  paneId,
                  SplitAxis.fromLegacy(layout.splitDirection),
                  splitId,
                  WorkspaceNodeId(s"reconcile-pane-leaf-${paneId.value}")
                )
            case (currentTree, _) => currentTree
          }
          .getOrElse(tree)
        val pinned = uiSurfaces.collect {
          case UiSurface(id, _, SurfacePresentation.Pinned(position, _), _) => id -> position
        }
        val pinnedIds = pinned.map(_._1).toSet
        val prunedTree = paneReconciledTree.dockedSurfaceIds
          .filterNot(pinnedIds.contains)
          .foldLeft(paneReconciledTree) {
            case (currentTree, surfaceId) =>
              currentTree.removeSurface(surfaceId).getOrElse(currentTree)
          }
        val reconciledTree = pinned.zipWithIndex.foldLeft(prunedTree) {
          case (currentTree, ((surfaceId, position), index)) =>
            if currentTree.dockedSurfaceIds.contains(surfaceId) then
              if currentTree.positionForSurface(surfaceId).contains(position) then currentTree
              else
                currentTree
                  .moveSurface(surfaceId, position, WorkspaceNodeId(s"reconcile-dock-$index-${surfaceId.value}"))
                  .getOrElse(currentTree)
            else
              val splitId = WorkspaceNodeId(s"dock-${surfaceId.value}")
              val leafId  = WorkspaceNodeId(s"dock-leaf-${surfaceId.value}")
              currentTree.dock(surfaceId, position, splitId, leafId).getOrElse(currentTree)
        }
        val orderedTree = pinned
          .groupBy(_._2)
          .foldLeft(reconciledTree) {
            case (currentTree, (position, surfacesAtPosition)) =>
              val desiredOrder = surfacesAtPosition.map(_._1)
              val currentOrder = currentTree.dockedSurfaceIds.filter { surfaceId =>
                currentTree.positionForSurface(surfaceId).contains(position)
              }
              if currentOrder == desiredOrder then currentTree
              else
                desiredOrder.zipWithIndex.foldLeft(currentTree) {
                  case (tree, (surfaceId, index)) =>
                    tree
                      .moveSurface(
                        surfaceId,
                        position,
                        WorkspaceNodeId(s"reconcile-order-${position.toString.toLowerCase}-$index-${surfaceId.value}")
                      )
                      .getOrElse(tree)
                }
          }
        copy(layout = layout.copy(workspaceTree = Some(orderedTree), paneOrder = orderedTree.paneIds))

  // Cheap pre-check for the common case where no pane or pinned-surface change requires rebuilding the tree,
  // so events that don't touch panes/docking (e.g. command-palette navigation) skip the full reconciliation pass.
  private def workspaceTreeAlreadyReconciled(tree: WorkspaceTree): Boolean =
    tree.dockedSurfaceIds.isEmpty &&
      !uiSurfaces.exists {
        case UiSurface(_, _, SurfacePresentation.Pinned(_, _), _) => true
        case _                                                    => false
      } &&
      tree.paneIds.toSet == layout.editorPanes.keySet &&
      layout.paneOrder.filter(layout.editorPanes.keySet.contains) == tree.paneIds

  // The compiler-generated equals walks every one of this class's ~20 fields (including the buffers map, which
  // then walks every open buffer) with no fast path -- paid on every dispatched event, including ones like
  // MouseMove that never change state. Short-circuiting on reference identity covers the overwhelmingly common
  // "nothing changed" case in O(1). Comparing via productIterator rather than hand-listing fields avoids the risk
  // of silently dropping a field from the comparison as the case class evolves.
  override def equals(obj: Any): Boolean =
    obj match
      case that: AnyRef if (this: AnyRef).eq(that) => true
      case that: AppState                          => productIterator.sameElements(that.productIterator)
      case _                                       => false

object AppState:

  def initial(using com.serenity.rope.Balance): AppState = initial(AppConfig.default)

  def initial(config: AppConfig)(using com.serenity.rope.Balance): AppState =
    val initialBufferId = BufferId(0)
    val initialBuffer   = Buffer.newEmpty(initialBufferId)
    val initialPane     = EditorPane.withBuffer(PaneId(0), initialBufferId)
    val layout = Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0)),
      paneOrder = List(PaneId(0)),
      workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0))))
    )
    AppState(
      layout = layout,
      buffers = Map(initialBufferId -> initialBuffer),
      bufferOrder = List(initialBufferId),
      focus = Focus.EditorPane(PaneId(0)),
      config = config,
      windowSitter = WindowSitter.fromConfig(config.windowSitterConfig),
      nextBufferId = BufferId(1),
      nextPaneId = PaneId(1),
      nextSurfaceId = 0
    )

  def empty: AppState = empty(AppConfig.default)

  def empty(config: AppConfig): AppState =
    AppState(
      layout = Layout.empty,
      buffers = Map.empty,
      focus = Focus.EditorPane(PaneId(0)),
      config = config,
      windowSitter = WindowSitter.fromConfig(config.windowSitterConfig)
    )

enum AppAction:
  case CloseWorkflow(workflow: CloseWorkflowState)
