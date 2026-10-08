package com.serenity.state.models

import com.serenity.config.*
import com.serenity.lsp.client.DocumentUri
import com.serenity.markdown.MarkdownBlockLens
import com.serenity.text.TextStatistics
import com.serenity.ui.layout.{Layout, SpacingScale, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}

final case class AppState(
    persisted: Persisted,
    runtime: Runtime = Runtime()
):

  // The paned buffers' annotation and semantic-token indexes are read by the renderer every frame. They are derived
  // values (#1864): `withBufferIndexesRefreshed` brings their memos up to date once per commit, recomputing only those
  // whose inputs changed, and every copy made afterwards reads them for free. A memo whose inputs have since changed is
  // never served -- that buffer's index is computed on demand instead, as for a buffer outside every pane.
  private def panedBufferIds: Set[BufferId] =
    persisted.layout.editorPanes.values.flatMap(_.bufferId).toSet

  private def indexSource(buffer: Buffer): BufferIndexes.Source =
    BufferIndexes.Source(buffer, runtime.bufferIndexMemos.uriFor(buffer), runtime.languageService)

  def withBufferIndexesRefreshed: AppState =
    val paned = panedBufferIds.flatMap(id => persisted.buffers.get(id).map(id -> _)).toMap
    val memos = runtime.bufferIndexMemos.refreshed(paned, runtime.languageService)
    if memos eq runtime.bufferIndexMemos then this else copy(runtime = runtime.copy(bufferIndexMemos = memos))

  /** The URI a buffer's diagnostics and semantic tokens are keyed by, derived once per file path while it is paned. */
  def documentUri(bufferId: BufferId): Option[DocumentUri] =
    persisted.buffers.get(bufferId).map(runtime.bufferIndexMemos.uriFor)

  def annotationIndex(bufferId: BufferId): Option[AnnotationLineIndex] =
    persisted.buffers.get(bufferId).map { buffer =>
      BufferIndexes.annotations.valueFor(runtime.bufferIndexMemos.annotations.get(bufferId), indexSource(buffer))
    }

  /** See [[SemanticTokensAvailability]] for what each case means and how the renderer treats it: `Pending` (no entry in
    * `runtime.languageService.semanticTokensState` at all yet) is deliberately distinct from `Unavailable` (confirmed
    * via `unavailableUris`) -- a request still in flight must not render the same muted style as a confirmed absence
    * (issue #859/#1177 rendering-slice review finding).
    */
  def semanticTokensAvailability(bufferId: BufferId): Option[SemanticTokensAvailability] =
    persisted.buffers.get(bufferId).map { buffer =>
      BufferIndexes.semanticTokens.valueFor(runtime.bufferIndexMemos.semanticTokens.get(bufferId), indexSource(buffer))
    }

  /** Computed on demand, unmemoised: nothing reads it per frame, and its input is the content, which every edit
    * changes.
    */
  def markdownFenceIndex(bufferId: BufferId): Option[MarkdownBlockLens.FenceRangeIndex] =
    persisted.buffers.get(bufferId).map(computeMarkdownFenceIndex)

  private def computeMarkdownFenceIndex(buffer: Buffer): MarkdownBlockLens.FenceRangeIndex =
    MarkdownBlockLens.fenceRangeIndex(buffer.document.content.lineCount, buffer.document.content.getLine)

  def syntaxHighlightingEnabled: Boolean = persisted.config.languageToolsConfig.syntaxHighlightingEnabled
  def isValid: Boolean                   = AppStateValidation.validationErrors(this).isEmpty

  /** `InterfaceConfig.elementGap`, resolved for this state's surface. `AppConfig` alone can't make this call --
    * `runtime.capabilities` lives only here -- and it must not: the config is shared and persisted across both
    * surfaces, so baking either one's default into it would be wrong. Unset (`None`) defaults to a GUI cell of
    * breathing room (flush-to-edge content there reads as unfinished), scaled by [[SpacingScale.densityMultiplier]] the
    * same way every pixel-resolved piece of UI chrome already varies with `interfaceDensity` (issue #1542 re-scope) --
    * `ceil`ed rather than rounded so `Compact`/`Comfortable` keep the existing one-cell floor instead of a gap that
    * visually merges into its neighbour, while `Spacious` visibly grows past it. TUI's existing density is left alone
    * (a cell there is a whole column out of a typical 80). An explicit value is honoured on both surfaces, unscaled --
    * a user who set a cell count meant exactly that count, not that count re-interpreted per density.
    */
  def effectiveUiElementGap: Double =
    persisted.config.uiElementGap.getOrElse(
      if runtime.capabilities.isCellGrid then 0.0 else effectiveUiElementGapDefaultCells
    )

  /** [[LineNumberLayout.marginLeft]], resolved the same way as [[effectiveUiElementGap]]. */
  def effectiveLineNumberMarginLeft: Int =
    persisted.config.surfaceConfig.lineNumberLayout.marginLeft.getOrElse(
      if runtime.capabilities.isCellGrid then 0 else effectiveUiElementGapDefaultCells.toInt
    )

  /** [[LineNumberLayout.padding]], resolved the same way as [[effectiveUiElementGap]]. */
  def effectiveLineNumberPadding: Int =
    persisted.config.surfaceConfig.lineNumberLayout.padding.getOrElse(
      if runtime.capabilities.isCellGrid then 0 else effectiveUiElementGapDefaultCells.toInt
    )

  /** The GUI's density-scaled default cell count shared by [[effectiveUiElementGap]], [[effectiveLineNumberMarginLeft]]
    * and [[effectiveLineNumberPadding]]: one cell scaled by [[SpacingScale.densityMultiplier]] and `ceil`ed to the next
    * whole cell, since none of the three has a pixel metric to resolve a fractional cell against.
    */
  private def effectiveUiElementGapDefaultCells: Double =
    math.ceil(1.0 * SpacingScale.densityMultiplier(persisted.config.interfaceDensity))

  /** The command palette's cursor gap, resolved for this state's surface. An explicit override wins; otherwise an
    * explicit `uiElementGap` wins; otherwise unset falls back per surface -- the GUI keeps the density-derived
    * `InterfaceDensityMetrics.overlayGapRows` this accessor always used (unlike [[effectiveUiElementGap]]'s flat
    * one-cell default, deliberately NOT reused here: `CursorOverlayLayoutSpec` pins the command palette's own
    * per-density gap, e.g. zero at `Compact`, so flattening it to one cell regardless of density would be a real
    * behavior change, not just a surface fix), while the TUI now gets the same flush-by-default treatment as every
    * other TUI spacing default rather than always inheriting the GUI-oriented density gap. Replaces
    * `AppConfig.effectiveCommandRunnerCursorGapRows` (`AppConfigOps`), which read the raw `uiElementGap` field and so
    * had no way to tell a TUI session from a GUI one at all.
    */
  def effectiveCommandRunnerCursorGapRows: Double =
    persisted.config.surfaceConfig.commandRunnerCursorGapRows.getOrElse(
      persisted.config.uiElementGap.filter(_ > 0.0).getOrElse {
        if runtime.capabilities.isCellGrid then 0.0
        else InterfaceDensityMetrics.forDensity(persisted.config.interfaceDensity).overlayGapRows.toDouble
      }
    )

  /** Cursor position for the currently active editor pane, if any. */
  def activeCursorPosition: Option[CursorPosition] =
    persisted.layout.activeEditorPaneId
      .flatMap(persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(persisted.buffers.get)
      .flatMap(_.editing.cursorPositions.headOption)

  def editingContext: EditingContext = EditingContext.of(this)

  /** The status text both placements show, or `None` when the status line is off or nothing is open. In safe mode it
    * always leads with the safe-mode label, even with nothing open.
    */
  def statusLineText: Option[String] =
    if !persisted.config.statusLine.isShown then None
    else
      val text = StatusLineText.render(this, persisted.config.statusLine.segments)
      if runtime.safeMode then Some((StatusLineText.SafeModeLabel :: text.toList).mkString(StatusLineText.Separator))
      else text

  /** Whether a typing burst is holding the floating status row hidden -- the only thing `runtime.typingActivity`
    * changes on screen, so with any other placement it needs no frames at all.
    */
  def typingHidesFloatingStatusLine: Boolean =
    persisted.config.statusLine.isFloating && runtime.typingActivity.isActive

  /** The floating status row, derived each frame rather than stored: it follows the caret and steps aside for the
    * length of a typing burst (`runtime.typingActivity`) so nothing near the caret moves while text is going in.
    */
  def floatingStatusLineSurface: Option[UiSurface] =
    if !persisted.config.statusLine.isFloating || runtime.typingActivity.isActive then None
    else
      for
        text   <- statusLineText
        cursor <- activeCursorPosition
      yield UiSurface(
        id = UiSurface.StatusLineSurfaceId,
        content = SurfaceContent.StatusLine(text),
        presentation = SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
      )

  /** The always-visible tab strip (issue #1074 epic, #1075-1077), derived each frame like [[floatingStatusLineSurface]]
    * rather than stored -- carries the same `TabListContent.build` snapshot the mode/tab corner widget's own popup list
    * uses, so there is no separate "open tabs" state to keep in sync. Only appears with 2+ open buffers: with one
    * buffer there is nothing to switch between, so `LayoutEngine` reserves no strip (`LayoutEngine.showsTabBar`) and
    * this returns `None` to match (issue #1074 decision).
    */
  def tabBarSurface: Option[UiSurface] =
    Option.when(persisted.bufferOrder.size >= 2) {
      val tabList = TabListContent.build(this)
      UiSurface(
        id = UiSurface.TabBarSurfaceId,
        content = SurfaceContent.TabBar(tabList.entries, tabList.activeBufferId),
        presentation = SurfacePresentation.Docked
      )
    }

  def commandRunnerContext: com.serenity.command.CommandRunnerContext =
    com.serenity.command.CommandRunnerContext(
      bufferLanguage = activeBuffer.flatMap(_.document.language),
      themeNames = runtime.themeDiscovery.availableThemeNames,
      currentThemeName = Some(persisted.theme.name),
      editingContext = Some(editingContext),
      projectPresence = runtime.projectPresence,
      opensFileOrFolder = runtime.capabilities.opensFileOrFolder
    )

  /** The active editor pane's buffer, if any. */
  def activeBuffer: Option[Buffer] =
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

  def floatingSurfaces: List[UiSurface] =
    runtime.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, _) => true
        case _                                  => false
    }

  /** Every floating surface a frame actually paints, including the ones derived per frame rather than stored in
    * `runtime.uiSurfaces` -- today that is the floating status row ([[floatingStatusLineSurface]]).
    *
    * [[floatingSurfaces]] answers a narrower question ("which floating surfaces does this state *hold*"), which is what
    * the layout wants: it composes the derived bar itself, as the fallback in
    * `LayoutEngine.orderedBelowCursorSurfaces`, and would double-count it here. Anything asking what is *on screen* --
    * mouse hit-testing above all -- has to use this one, or it treats a painted surface as absent and lets clicks fall
    * through to whatever it covers (#1292).
    */
  def visibleFloatingSurfaces: List[UiSurface] =
    floatingSurfaces ++ floatingStatusLineSurface.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, _) => true
        case _                                  => false
    }

  /** Every docked surface, in the workspace tree's own order -- the tree (issue #817) is the sole authority for which
    * surfaces are pinned and where, so this reads it directly rather than filtering `runtime.uiSurfaces` by
    * presentation. A `Docked` surface absent from the tree is invalid state, caught by `AppStateValidation`, not a
    * surface this still reports as pinned.
    */
  def pinnedSurfaces: List[UiSurface] =
    persisted.layout.workspaceTree.fold(List.empty[UiSurface])(_.dockedSurfaceIds.flatMap(surfaceById))

  def expandedPanelSurface: Option[UiSurface] =
    for
      tree      <- persisted.layout.workspaceTree
      nodeId    <- persisted.layout.maximizedWorkspaceNodeId
      surfaceId <- tree.surfaceIdForNode(nodeId)
      surface   <- surfaceById(surfaceId)
    yield surface

  def surfaceById(surfaceId: SurfaceId): Option[UiSurface] =
    runtime.uiSurfaces
      .find(_.id == surfaceId)
      .orElse(floatingStatusLineSurface.filter(_.id == surfaceId))
      .orElse(tabBarSurface.filter(_.id == surfaceId))

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

  def themeCreatorSurface: Option[UiSurface] =
    findSurface {
      case SurfaceContent.ThemeCreator(_) => true
      case _                              => false
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

  /** Closes the context menu, handing focus back to whatever held it before the menu opened. */
  def withoutContextMenu: AppState =
    copy(runtime =
      runtime.copy(uiSurfaces =
        runtime.uiSurfaces.filterNot(surface =>
          surface.content match
            case SurfaceContent.ContextMenu(_) => true
            case _                             => false
        )
      )
    ).popFocus

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

  /** The topmost dialog on the explicit modal layer (#814), if any. */
  def topModal: Option[ModalDialog] =
    runtime.modalStack.lastOption

  /** The active *modeless* modal workflow surface (TextPrompt/Find/ReplaceWorkflow/ListPicker) -- blocking dialogs live
    * on `runtime.modalStack` instead, see `topModal`.
    */
  def modalSurface: Option[UiSurface] =
    runtime.uiSurfaces.reverse.find(isModalWorkflow)

  def hasBlockingModal: Boolean =
    runtime.modalStack.nonEmpty

  /** A blocking dialog is up, or a modeless modal workflow has focus: either one owns input until it closes. */
  def isModalFocus: Boolean =
    hasBlockingModal || activeSurface.exists(isModalWorkflow)

  /** Remove the topmost modal dialog (or, absent one, the focused modeless modal workflow surface) and restore the
    * focus that opened it.
    */
  def dismissTopModal: AppState =
    topModal match
      case Some(_) =>
        copy(runtime = runtime.copy(modalStack = runtime.modalStack.dropRight(1))).popFocus
      case None =>
        activeSurface.filter(isModalWorkflow) match
          case Some(surface) =>
            copy(runtime = runtime.copy(uiSurfaces = runtime.uiSurfaces.filterNot(_.id == surface.id))).popFocus
          case None => this

  def peekSurface: Option[UiSurface] =
    runtime.uiSurfaces.find(_.isFloatingPeek)

  private def findSurface(matches: SurfaceContent => Boolean): Option[UiSurface] =
    runtime.uiSurfaces.find(surface => matches(surface.content))

  private def isModalWorkflow(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false

  def allocateSurfaceId: (AppState, SurfaceId) =
    val (nextSupply, surfaceId) = runtime.nextSurfaceId.next
    (copy(runtime = runtime.copy(nextSurfaceId = nextSupply)), surfaceId)

  def pushFocus(newFocus: Focus): AppState =
    val deduplicated = runtime.focusHistory.filterNot(_ == persisted.focus)
    copy(
      persisted = persisted.copy(focus = newFocus),
      runtime = runtime.copy(focusHistory = persisted.focus :: deduplicated)
    )

  /** A peek is shown without ever taking focus (#1940), so only other surfaces are focused here. */
  def pushFocusUnlessPeek(surface: UiSurface): AppState =
    if surface.focusPolicy == SurfaceFocusPolicy.Peek then this else pushFocus(Focus.Surface(surface.id))

  def popFocus: AppState =
    runtime.focusHistory match
      case head :: tail =>
        head match
          case Focus.Surface(sid) if surfaceById(sid).isEmpty =>
            copy(runtime = runtime.copy(focusHistory = tail)).popFocus
          case Focus.Modal if runtime.modalStack.isEmpty =>
            copy(runtime = runtime.copy(focusHistory = tail)).popFocus
          case validFocus =>
            copy(persisted = persisted.copy(focus = validFocus), runtime = runtime.copy(focusHistory = tail))
      case Nil =>
        val fallback = persisted.layout.activeEditorPaneId
          .map(Focus.EditorPane(_))
          .getOrElse(Focus.EditorPane(PaneId(0)))
        copy(persisted = persisted.copy(focus = fallback))

  def focusedBufferId: Option[BufferId] =
    persisted.focus match
      case Focus.EditorPane(paneId) =>
        persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
      case _ => None

  /** The buffer the user is working in: the focused pane's, or -- while a panel or other surface has focus -- the
    * active pane's, where a buffer switch will land.
    */
  def currentEditorBufferId: Option[BufferId] =
    persisted.focus match
      case Focus.EditorPane(_) => focusedBufferId
      case _ =>
        persisted.layout.activeEditorPaneId.flatMap(persisted.layout.editorPanes.get).flatMap(_.bufferId)

  def nextBufferInOrder(currentBufferId: BufferId): Option[BufferId] =
    if persisted.bufferOrder.isEmpty then None
    else
      val currentIndex = persisted.bufferOrder.indexOf(currentBufferId)
      if currentIndex == -1 then persisted.bufferOrder.headOption
      else
        val nextIndex = (currentIndex + 1) % persisted.bufferOrder.size
        Some(persisted.bufferOrder(nextIndex))

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
    val baseLayout = Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0)),
      workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0))))
    )
    AppState(
      persisted = Persisted(
        layout = baseLayout,
        buffers = Map(initialBufferId -> initialBuffer),
        bufferOrder = List(initialBufferId),
        focus = Focus.EditorPane(PaneId(0)),
        config = config
      ),
      runtime = Runtime(
        nextBufferId = BufferId(1),
        nextPaneId = PaneId(1),
        nextSurfaceId = SurfaceIdSupply.initial
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
      runtime = Runtime()
    )

enum AppAction:
  case CloseWorkflow(workflow: CloseWorkflowState)
