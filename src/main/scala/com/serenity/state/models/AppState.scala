package com.serenity.state.models

import java.util.concurrent.atomic.AtomicReference

import com.serenity.config.*
import com.serenity.markdown.MarkdownBlockLens
import com.serenity.text.TextStatistics
import com.serenity.ui.layout.{Layout, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}

final case class AppState(
    persisted: Persisted,
    runtime: Runtime = Runtime()
):

  // Per-buffer, not a whole-workspace wrapper map: a fresh `AppState` snapshot is produced on essentially every edit
  // (#1456), so a caller reaching for one buffer's index must not pay an O(buffers) map-build to get there, even
  // though the actual index computation below was already deferred. Each snapshot still gets its own fresh cache, and
  // repeated lookups against the same snapshot -- e.g. several render passes over one scene -- reuse the computed
  // index instead of recomputing it.
  //
  // `AtomicReference` rather than `Ref[IO, Map[...]]`: both caches below are read and written synchronously from
  // `annotationIndex`/`markdownFenceIndex`, which are called from the render path
  // (`RendererPaneSetup.prepareEditorPaneRenderPlan`, `RendererPaneContent`, `RendererMarkdownLens.isInlineMarkdownLens`)
  // -- plain, `Unit`/value-returning methods that run synchronously inside the renderer's own frame-preparation code,
  // never inside an IO fiber of their own. A `Ref`-backed cache here would just force its `IO` via `unsafeRunSync`
  // right back into these synchronous signatures at every call site, hiding a plain compare-and-set behind an effect
  // type nothing here ever suspends on. This is the same tradeoff already settled for
  // `com.serenity.ui.renderer.RendererFrameState.BoundedRefCache` and `com.serenity.state.manager.MouseTargetCache`'s
  // `lastComputation` (#1431/#1434), `ThemeManager`'s highlight/lex caches (#1412/#1431/#1434), and
  // `RuntimeDisplayState` (#1448) -- a lock-free CAS cache reached from synchronous, non-IO call sites, not an
  // oversight of the "use `Ref[IO,A]`" rule.
  private val annotationIndexCache: AtomicReference[Map[BufferId, AnnotationLineIndex]] =
    new AtomicReference(Map.empty)

  private val markdownFenceIndexCache: AtomicReference[Map[BufferId, MarkdownBlockLens.FenceRangeIndex]] =
    new AtomicReference(Map.empty)

  private val semanticTokensCache: AtomicReference[Map[BufferId, SemanticTokensAvailability]] =
    new AtomicReference(Map.empty)

  /** `bufferId`'s annotation index, computed (and cached) only for that buffer -- see `annotationIndexCache`. */
  def annotationIndex(bufferId: BufferId): Option[AnnotationLineIndex] =
    persisted.buffers.get(bufferId).map { buffer =>
      annotationIndexCache.get().get(bufferId) match
        case Some(cached) => cached
        case None =>
          val diagnostics =
            runtime.diagnosticsState.diagnostics.getOrElse(
              com.serenity.spellcheck.SpellChecker.diagnosticsUri(buffer),
              Nil
            )
          val computed = AnnotationLineIndex(
            buffer.annotations.documentComments.toVector,
            diagnostics.groupMap(_.range.start.line)(identity)
          )
          val _ = annotationIndexCache.updateAndGet(_.updated(bufferId, computed))
          computed
    }

  /** `bufferId`'s semantic-tokens status, computed (and cached) only for that buffer -- see `semanticTokensCache`. See
    * [[SemanticTokensAvailability]] for what each case means and how the renderer treats it: `Pending` (no entry in
    * `runtime.semanticTokensState` at all yet) is deliberately distinct from `Unavailable` (confirmed via
    * `unavailableUris`) -- a request still in flight must not render the same muted style as a confirmed absence (issue
    * #859/#1177 rendering-slice review finding).
    */
  def semanticTokensAvailability(bufferId: BufferId): Option[SemanticTokensAvailability] =
    persisted.buffers.get(bufferId).map { buffer =>
      semanticTokensCache.get().get(bufferId) match
        case Some(cached) => cached
        case None =>
          val uri = com.serenity.spellcheck.SpellChecker.diagnosticsUri(buffer)
          val computed = runtime.semanticTokensState.byUri.get(uri) match
            case Some(tokens) => SemanticTokensAvailability.Available(tokens.groupBy(_.line))
            case None =>
              if runtime.semanticTokensState.unavailableUris.contains(uri) then SemanticTokensAvailability.Unavailable
              else SemanticTokensAvailability.Pending
          val _ = semanticTokensCache.updateAndGet(_.updated(bufferId, computed))
          computed
    }

  /** `bufferId`'s markdown fence-range index, computed (and cached) only for that buffer -- see
    * `markdownFenceIndexCache`.
    */
  def markdownFenceIndex(bufferId: BufferId): Option[MarkdownBlockLens.FenceRangeIndex] =
    persisted.buffers.get(bufferId).map { buffer =>
      markdownFenceIndexCache.get().get(bufferId) match
        case Some(cached) => cached
        case None =>
          val computed =
            MarkdownBlockLens.fenceRangeIndex(buffer.document.content.lineCount, buffer.document.content.getLine)
          val _ = markdownFenceIndexCache.updateAndGet(_.updated(bufferId, computed))
          computed
    }

  def syntaxHighlightingEnabled: Boolean = persisted.config.languageToolsConfig.syntaxHighlightingEnabled
  def isValid: Boolean                   = AppStateValidation.validationErrors(this).isEmpty

  /** `InterfaceConfig.elementGap`, resolved for this state's surface. `AppConfig` alone can't make this call --
    * `runtime.isTuiMode` lives only here -- and it must not: the config is shared and persisted across both surfaces,
    * so baking either one's default into it would be wrong. Unset (`None`) defaults to a GUI cell of breathing room
    * (flush-to-edge content there reads as unfinished) but leaves the TUI's existing density alone (a cell there is a
    * whole column out of a typical 80). An explicit value is honoured on both surfaces.
    */
  def effectiveUiElementGap: Double =
    persisted.config.uiElementGap.getOrElse(if runtime.isTuiMode then 0.0 else 1.0)

  /** [[LineNumberLayout.marginLeft]], resolved the same way as [[effectiveUiElementGap]]. */
  def effectiveLineNumberMarginLeft: Int =
    persisted.config.surfaceConfig.lineNumberLayout.marginLeft.getOrElse(if runtime.isTuiMode then 0 else 1)

  /** [[LineNumberLayout.padding]], resolved the same way as [[effectiveUiElementGap]]. */
  def effectiveLineNumberPadding: Int =
    persisted.config.surfaceConfig.lineNumberLayout.padding.getOrElse(if runtime.isTuiMode then 0 else 1)

  /** The command palette's cursor gap, resolved for this state's surface. An explicit override wins; otherwise an
    * explicit `uiElementGap` wins; otherwise unset falls back per surface -- the GUI keeps the density-derived
    * `InterfaceDensityMetrics.overlayGapRows` this accessor always used (unlike [[effectiveUiElementGap]]'s flat
    * one-cell default, deliberately NOT reused here: `CursorOverlayLayoutSpec` pins the command palette's own
    * per-density gap, e.g. zero at `Compact`, so flattening it to one cell regardless of density would be a real
    * behavior change, not just a surface fix), while the TUI now gets the same flush-by-default treatment as every
    * other TUI spacing default rather than always inheriting the GUI-oriented density gap. Replaces
    * `AppConfig.effectiveCommandRunnerCursorGapRows` (`AppConfigMotionOps`), which read the raw `uiElementGap`
    * field and so had no way to tell a TUI session from a GUI one at all.
    */
  def effectiveCommandRunnerCursorGapRows: Double =
    persisted.config.surfaceConfig.commandRunnerCursorGapRows.getOrElse(
      persisted.config.uiElementGap.filter(_ > 0.0).getOrElse {
        if runtime.isTuiMode then 0.0
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

  /** The status text both placements show, or `None` when the status line is off or nothing is open. */
  def statusLineText: Option[String] =
    if !persisted.config.statusLine.isShown then None
    else StatusLineText.render(this, persisted.config.statusLine.segments)

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
      themeNames = runtime.availableThemeNames,
      currentThemeName = Some(persisted.theme.name),
      editingContext = Some(editingContext)
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

  /** The topmost dialog on the explicit modal layer (#814), if any. */
  def topModal: Option[ModalDialog] =
    runtime.modalStack.lastOption

  /** The active *modeless* modal workflow surface (GotoLine/Find/ReplaceWorkflow/Custom) -- blocking dialogs live on
    * `runtime.modalStack` instead, see `topModal`.
    */
  def modalSurface: Option[UiSurface] =
    runtime.uiSurfaces.reverse.find(isModalWorkflow)

  def hasBlockingModal: Boolean =
    runtime.modalStack.nonEmpty

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
    val layout = dockCompanionSprite(baseLayout, config)
    AppState(
      persisted = Persisted(
        layout = layout,
        buffers = Map(initialBufferId -> initialBuffer),
        bufferOrder = List(initialBufferId),
        focus = Focus.EditorPane(PaneId(0)),
        config = config
      ),
      runtime = Runtime(
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
        layout = dockCompanionSprite(Layout.empty, config),
        buffers = Map.empty,
        focus = Focus.EditorPane(PaneId(0)),
        config = config
      ),
      runtime = Runtime(uiSurfaces = companionSpriteSurfaces(config))
    )

  /** The companion sprite's pinned panel surface, present exactly when a freshly-started or freshly-restored session
    * should show it -- enabled in config, and visual flair not `Off`. Mirrors
    * `StateManagerEffectHandlers.syncCompanionSpritePanel`'s same visibility rule, so a session that starts with the
    * setting already on shows the pane immediately rather than only after the toggle is next flipped during the
    * session.
    */
  def companionSpriteSurfaces(config: AppConfig): List[UiSurface] =
    Option
      .when(config.companionSpriteConfig.enabled && config.visualFlairLevel != VisualFlairLevel.Off) {
        UiSurface(
          id = SurfaceId.CompanionSprite,
          content = SurfaceContent.CompanionSprite,
          presentation = SurfacePresentation.Docked
        )
      }
      .toList

  /** Docks the companion sprite surface (if enabled) into `layout`'s workspace tree at its configured edge and size --
    * the tree is the sole record of a docked surface's position and size (issue #817), so a surface built with
    * `SurfacePresentation.Docked` needs an explicit tree entry, not just a place in `uiSurfaces`. A no-op when the
    * sprite is disabled, `layout` carries no tree yet (`Layout.empty`), or it's already docked (idempotent, so a caller
    * that isn't sure which applies -- e.g. `AppStartup.initializeState`'s open-path-at-startup flow, where
    * `AppState.empty`'s companion sprite surface predates the real workspace tree `fileOpener.openFile` builds -- can
    * call it unconditionally once that tree exists).
    */
  def dockCompanionSprite(layout: Layout, config: AppConfig): Layout =
    companionSpriteSurfaces(config).headOption match
      case None => layout
      case Some(surface) =>
        layout.workspaceTree match
          case None => layout
          case Some(tree) =>
            val position          = config.companionSpriteConfig.position
            val (splitId, leafId) = tree.nextDockIds(surface.id)
            // No viewport is known this early in startup -- `dockSized` (via `allocationRatio`) falls back to an
            // assumed total, the same fallback a `pin` call would hit in the same no-viewport-yet situation.
            val docked =
              tree.dockSized(surface.id, position, splitId, leafId, config.companionSpriteConfig.size, None)
            layout.copy(workspaceTree = docked.orElse(layout.workspaceTree))

enum AppAction:
  case CloseWorkflow(workflow: CloseWorkflowState)
