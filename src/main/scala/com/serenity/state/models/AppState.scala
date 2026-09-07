package com.serenity.state.models

import com.serenity.animation.WindowSitter
import com.serenity.config.*
import com.serenity.markdown.MarkdownBlockLens
import com.serenity.text.TextStatistics
import com.serenity.ui.layout.{Layout, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}

final case class AppState(
    persisted: Persisted,
    runtime: Runtime = Runtime()
):

  /** Lazily indexes annotations for this immutable state snapshot. A new state snapshot gets a fresh index, while
    * repeated render plans for the same scene reuse the existing one.
    */
  lazy val annotationIndexByBuffer: Map[BufferId, () => AnnotationLineIndex] =
    persisted.buffers.iterator.map {
      case (bufferId, buffer) =>
        lazy val index =
          val diagnostics =
            runtime.diagnosticsState.diagnostics.getOrElse(
              com.serenity.spellcheck.SpellChecker.diagnosticsUri(buffer),
              Nil
            )
          AnnotationLineIndex(
            buffer.annotations.documentComments.toVector,
            diagnostics.groupMap(_.range.start.line)(identity)
          )
        bufferId -> (() => index)
    }.toMap

  lazy val markdownFenceIndexByBuffer: Map[BufferId, () => MarkdownBlockLens.FenceRangeIndex] =
    persisted.buffers.iterator.map {
      case (bufferId, buffer) =>
        lazy val index =
          MarkdownBlockLens.fenceRangeIndex(buffer.document.content.lineCount, buffer.document.content.getLine)
        bufferId -> (() => index)
    }.toMap

  /** Convenience accessor for syntax highlighting setting */
  def syntaxHighlightingEnabled: Boolean = persisted.config.languageToolsConfig.syntaxHighlightingEnabled
  def isValid: Boolean                   = AppStateValidation.validationErrors(this).isEmpty

  /** Cursor position for the currently active editor pane, if any. */
  def activeCursorPosition: Option[CursorPosition] =
    persisted.layout.activeEditorPaneId
      .flatMap(persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(persisted.buffers.get)
      .flatMap(_.editing.cursors.headOption)

  def cursorInfoBarSurface: Option[UiSurface] =
    if persisted.config.cursorInfoBarSegments.isEmpty then None
    else
      persisted.config.cursorInfoBarPlacement match
        case CursorInfoBarPlacement.Floating =>
          for
            paneId   <- persisted.layout.activeEditorPaneId
            pane     <- persisted.layout.editorPanes.get(paneId)
            bufferId <- pane.bufferId
            buffer   <- persisted.buffers.get(bufferId)
            cursor   <- buffer.editing.cursors.headOption
          yield UiSurface(
            id = UiSurface.CursorInfoBarSurfaceId,
            content = SurfaceContent.CursorInfoBar(
              formatCursorInfoBarSegments(persisted.config.cursorInfoBarSegments, cursor, buffer)
            ),
            presentation = SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        case CursorInfoBarPlacement.PinnedBottom =>
          None

  def cursorInfoBarText: Option[String] =
    val segments = persisted.config.cursorInfoBarSegments
    if segments.isEmpty then None
    else
      for
        paneId   <- persisted.layout.activeEditorPaneId
        pane     <- persisted.layout.editorPanes.get(paneId)
        bufferId <- pane.bufferId
        buffer   <- persisted.buffers.get(bufferId)
        cursor   <- buffer.editing.cursors.headOption
      yield formatCursorInfoBarSegments(segments, cursor, buffer)

  /** The active editor pane's buffer, if any -- the shared lookup `wordCountStatusText` and its helpers build on. */
  private def activeBuffer: Option[Buffer] =
    for
      paneId   <- persisted.layout.activeEditorPaneId
      pane     <- persisted.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- persisted.buffers.get(bufferId)
    yield buffer

  /** Whole-buffer word/character counts and reading time for the active editor pane, read in O(1) from the buffer's
    * `Rope` -- see `Rope.wordCount`/`Rope.nonWhitespaceCount` for how those stay current without a per-edit rescan.
    */
  def activeBufferTextStatistics: Option[TextStatistics] =
    activeBuffer.map(buffer => TextStatistics.of(buffer.document.content))

  /** Word/character counts for the active pane's current selection, or `None` when there is no non-empty selection. */
  def activeSelectionTextStatistics: Option[TextStatistics] =
    for
      buffer    <- activeBuffer
      selection <- buffer.primarySelection
      if selection.start != selection.end
      content     = buffer.document.content
      startOffset = content.lineColumnToOffset(selection.start.line, selection.start.column)
      endOffset   = content.lineColumnToOffset(selection.end.line, selection.end.column)
      if endOffset > startOffset
    yield TextStatistics.ofString(content.sliceString(startOffset, endOffset))

  /** Status-bar text for the word/character-count and reading-time display (#1203), or `None` when the feature is off
    * (`surfaceConfig.showWordCount`) or there is no active buffer. Selection-scoped when a non-empty selection is
    * active, whole-buffer otherwise -- mirrors `cursorInfoBarText`'s shape for the status-bar convention.
    */
  def wordCountStatusText: Option[String] =
    if !persisted.config.surfaceConfig.showWordCount then None
    else
      activeBufferTextStatistics.map { total =>
        activeSelectionTextStatistics match
          case Some(selection) =>
            s"${selection.wordCount} of ${total.wordCount} words selected, ${selection.characterCount} chars"
          case None =>
            s"${total.wordCount} words, ${total.characterCount} chars, ~${total.readingTimeMinutes} min read"
      }

  private def formatCursorInfoBarSegments(
    segments: List[CursorInfoBarSegment],
    cursor: CursorPosition,
    buffer: Buffer
  ): String =
    segments.map(formatCursorInfoBarSegment(_, cursor, buffer)).mkString(" | ")

  private def formatCursorInfoBarSegment(
    segment: CursorInfoBarSegment,
    cursor: CursorPosition,
    buffer: Buffer
  ): String =
    segment match
      case CursorInfoBarSegment.Position =>
        s"Line ${cursor.line + 1}, Col ${cursor.column + 1}"
      case CursorInfoBarSegment.Title =>
        buffer.document.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Unsaved")
      case CursorInfoBarSegment.WordCount =>
        s"${TextStatistics.of(buffer.document.content).wordCount} words"
      case CursorInfoBarSegment.CharCount =>
        s"${TextStatistics.of(buffer.document.content).characterCount} chars"
      case CursorInfoBarSegment.ReadingTime =>
        s"~${TextStatistics.of(buffer.document.content).readingTimeMinutes} min read"

  def floatingSurfaces: List[UiSurface] =
    runtime.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, _) => true
        case _                                  => false
    }

  /** Every floating surface a frame actually paints, including the ones derived per frame rather than stored in
    * `runtime.uiSurfaces` -- today that is the cursor info bar in `Floating` placement ([[cursorInfoBarSurface]]).
    *
    * [[floatingSurfaces]] answers a narrower question ("which floating surfaces does this state *hold*"), which is what
    * the layout wants: it composes the derived bar itself, as the fallback in
    * `LayoutEngine.orderedBelowCursorSurfaces`, and would double-count it here. Anything asking what is *on screen* --
    * mouse hit-testing above all -- has to use this one, or it treats a painted surface as absent and lets clicks fall
    * through to whatever it covers (#1292).
    */
  def visibleFloatingSurfaces: List[UiSurface] =
    floatingSurfaces ++ cursorInfoBarSurface.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, _) => true
        case _                                  => false
    }

  def pinnedSurfaces: List[UiSurface] =
    val storedPinned = runtime.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Pinned(_, _) => true
        case _                                => false
    }
    val orderedStored = persisted.layout.workspaceTree match
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
      tree      <- persisted.layout.workspaceTree
      nodeId    <- persisted.layout.maximizedWorkspaceNodeId
      surfaceId <- tree.surfaceIdForNode(nodeId)
      surface   <- surfaceById(surfaceId)
    yield surface
    maximized.orElse(runtime.uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    })

  def surfaceById(surfaceId: SurfaceId): Option[UiSurface] =
    runtime.uiSurfaces.find(_.id == surfaceId).orElse(cursorInfoBarSurface.filter(_.id == surfaceId))

  def activeSurface: Option[UiSurface] =
    persisted.focus match
      case Focus.Surface(surfaceId) => surfaceById(surfaceId)
      case _                        => None

  def commandRunnerSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.CommandPalette(_) => true
      case _                                => false
    }

  // `commandRunnerSubmenuSurface` (the second floating UiSurface for a settings group's ghost preview) is gone
  // (issue #1059) -- a settings group drilled into from either entry point now renders on `commandRunnerSurface`
  // itself, so this domain is always just that one surface.
  def commandRunnerDomainSurfaceIds: Set[SurfaceId] =
    Set.from(commandRunnerSurface.map(_.id))

  def hasCommandRunnerDomain: Boolean =
    commandRunnerDomainSurfaceIds.nonEmpty

  def isCommandRunnerDomainFocus(currentFocus: Focus = persisted.focus): Boolean =
    currentFocus match
      case Focus.Surface(surfaceId) => commandRunnerDomainSurfaceIds.contains(surfaceId)
      case _                        => false

  def preferredCommandRunnerFocus: Option[Focus] =
    commandRunnerSurface.map(surface => Focus.Surface(surface.id))

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

  def shortcutsHelpSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.ShortcutsHelp(_) => true
      case _                               => false
    }

  def tabListSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.TabList(_, _) => true
      case _                            => false
    }

  def recentFilesInModeSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.RecentFilesInMode(_, _) => true
      case _                                      => false
    }

  /** Modal surfaces ordered from their parent to the topmost child. */
  def modalSurfaces: List[UiSurface] =
    runtime.uiSurfaces.collect { case surface @ UiSurface(_, _, SurfacePresentation.Modal, _) => surface }

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
    topModalSurface.orElse(runtime.uiSurfaces.reverse.find(isModalWorkflow))

  def hasBlockingModal: Boolean =
    topBlockingModalSurface.nonEmpty

  /** Remove the topmost modal workflow and restore the focus that opened it. */
  def dismissTopModal: AppState =
    topModalSurface.orElse(activeSurface.filter(isModalWorkflow)) match
      case Some(surface) =>
        copy(runtime = runtime.copy(uiSurfaces = runtime.uiSurfaces.filterNot(_.id == surface.id))).popFocus
      case None => this

  def peekSurface: Option[UiSurface] =
    runtime.uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) => true
        case _                                                             => false
    }

  private def findSurface(matches: SurfaceContent => Boolean): Option[UiSurface] =
    runtime.uiSurfaces.find(surface => matches(surface.content))

  private def isModalWorkflow(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false

  def allocateSurfaceId: (AppState, SurfaceId) =
    val surfaceId = SurfaceId(s"surface-${runtime.nextSurfaceId}")
    (copy(runtime = runtime.copy(nextSurfaceId = runtime.nextSurfaceId + 1)), surfaceId)

  def pushFocus(newFocus: Focus): AppState =
    val deduplicated = runtime.focusHistory.filterNot(_ == persisted.focus)
    copy(
      persisted = persisted.copy(focus = newFocus),
      runtime = runtime.copy(focusHistory = persisted.focus :: deduplicated)
    )

  def popFocus: AppState =
    runtime.focusHistory match
      case head :: tail =>
        head match
          case Focus.Surface(sid) if surfaceById(sid).isEmpty =>
            copy(runtime = runtime.copy(focusHistory = tail)).popFocus
          case validFocus =>
            copy(persisted = persisted.copy(focus = validFocus), runtime = runtime.copy(focusHistory = tail))
      case Nil =>
        val fallback = persisted.layout.activeEditorPaneId
          .map(Focus.EditorPane(_))
          .getOrElse(Focus.EditorPane(PaneId(0)))
        copy(persisted = persisted.copy(focus = fallback))

  /** Get the currently focused buffer ID, if any */
  def focusedBufferId: Option[BufferId] =
    persisted.focus match
      case Focus.EditorPane(paneId) =>
        persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
      case _ => None

  /** Get the next buffer ID in navigation order */
  def nextBufferInOrder(currentBufferId: BufferId): Option[BufferId] =
    if persisted.bufferOrder.isEmpty then None
    else
      val currentIndex = persisted.bufferOrder.indexOf(currentBufferId)
      if currentIndex == -1 then persisted.bufferOrder.headOption
      else
        val nextIndex = (currentIndex + 1) % persisted.bufferOrder.size
        Some(persisted.bufferOrder(nextIndex))

  /** Get the previous buffer ID in navigation order */
  def previousBufferInOrder(currentBufferId: BufferId): Option[BufferId] =
    if persisted.bufferOrder.isEmpty then None
    else
      val currentIndex = persisted.bufferOrder.indexOf(currentBufferId)
      if currentIndex == -1 then persisted.bufferOrder.headOption
      else
        val prevIndex = (currentIndex - 1 + persisted.bufferOrder.size) % persisted.bufferOrder.size
        Some(persisted.bufferOrder(prevIndex))

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
      persisted = Persisted(
        layout = layout,
        buffers = Map(initialBufferId -> initialBuffer),
        bufferOrder = List(initialBufferId),
        focus = Focus.EditorPane(PaneId(0)),
        config = config
      ),
      runtime = Runtime(
        windowSitter = WindowSitter.fromConfig(config.windowSitterConfig),
        uiSurfaces = companionSpriteSurfaces(config),
        nextBufferId = BufferId(1),
        nextPaneId = PaneId(1),
        nextSurfaceId = 0
      )
    )

  def empty: AppState = empty(AppConfig.default)

  def empty(config: AppConfig): AppState =
    AppState(
      persisted = Persisted(
        layout = Layout.empty,
        buffers = Map.empty,
        focus = Focus.EditorPane(PaneId(0)),
        config = config
      ),
      runtime = Runtime(
        windowSitter = WindowSitter.fromConfig(config.windowSitterConfig),
        uiSurfaces = companionSpriteSurfaces(config)
      )
    )

  /** The companion sprite's pinned panel surface, present exactly when a freshly-started or freshly-restored session
    * should show it -- enabled in config, and visual flair not `Off`. Mirrors
    * `StateManagerEffectHandlers.syncCompanionSpritePanel`'s same visibility rule, so a session that starts with the
    * setting already on shows the pane immediately rather than only after the toggle is next flipped during the
    * session.
    */
  private def companionSpriteSurfaces(config: AppConfig): List[UiSurface] =
    Option
      .when(config.companionSpriteConfig.enabled && config.visualFlairLevel != VisualFlairLevel.Off) {
        UiSurface(
          id = SurfaceId.CompanionSprite,
          content = SurfaceContent.CompanionSprite,
          presentation =
            SurfacePresentation.Pinned(config.companionSpriteConfig.position, config.companionSpriteConfig.size)
        )
      }
      .toList

enum AppAction:
  case CloseWorkflow(workflow: CloseWorkflowState)
