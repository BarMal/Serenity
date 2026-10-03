package com.serenity.state.models

import com.serenity.config.*
import com.serenity.markdown.MarkdownBlockLens
import com.serenity.text.TextStatistics
import com.serenity.ui.layout.{Layout, SpacingScale, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}

final case class AppState(
    persisted: Persisted,
    runtime: Runtime = Runtime()
):

  // Per-buffer, not a whole-workspace wrapper map: a fresh `AppState` snapshot is produced on essentially every edit
  // (#1456), so a caller reaching for one buffer's index must not pay an O(buffers) map-build to get there. Only the
  // buffers actually placed in a pane are indexed eagerly, below, at construction -- the render path
  // (`RendererPaneSetup.prepareEditorPaneRenderPlan`) revisits exactly those buffers every frame until the next edit
  // produces a new `AppState` (and so a freshly recomputed map), which is the same amortization the old cache gave a
  // paned buffer after its first touch. `annotationIndex`/`semanticTokensAvailability` still answer for any buffer,
  // paned or not -- one outside a pane is simply computed on demand, uncached, since nothing re-renders it every frame
  // to make caching worth its keep.
  //
  // `markdownFenceIndex` is NOT precomputed here, unlike the other two: nothing in the render path calls it per frame
  // (`RendererPaneSetup` never reads it), so there is no amortized cost to recoup -- only a whole-buffer `getLine` scan
  // (`MarkdownBlockLens.fenceRangeIndex`) that every paned buffer would otherwise pay at construction regardless of
  // language, on every edit. It is computed on demand, exactly like a non-paned buffer's index.
  //
  // Plain immutable fields, not `AtomicReference` (#1677): a mutable field -- even one instance-scoped and never
  // observed to leak across instances -- still turns `AppState` into a value nothing can safely treat as pure data:
  // `copy`, `equals`/`hashCode`, and every place that reasons about two `AppState`s as independent, referentially
  // transparent values (not least running two of them in the same JVM in tests) has to reason around it instead.
  // Eagerly computing just the paned buffers' indexes at construction removes the field without reintroducing the
  // O(buffers) rebuild #1456 fixed, since construction cost here scales with the pane count, not the buffer count.
  private def panedBufferIds: Set[BufferId] =
    persisted.layout.editorPanes.values.flatMap(_.bufferId).toSet

  private val annotationIndexByPanedBuffer: Map[BufferId, AnnotationLineIndex] =
    panedBufferIds.flatMap(id => persisted.buffers.get(id).map(buffer => id -> computeAnnotationIndex(buffer))).toMap

  private val semanticTokensAvailabilityByPanedBuffer: Map[BufferId, SemanticTokensAvailability] =
    panedBufferIds
      .flatMap(id => persisted.buffers.get(id).map(buffer => id -> computeSemanticTokensAvailability(buffer)))
      .toMap

  /** `bufferId`'s annotation index -- precomputed if `bufferId` is placed in a pane (`annotationIndexByPanedBuffer`),
    * computed fresh on demand otherwise.
    */
  def annotationIndex(bufferId: BufferId): Option[AnnotationLineIndex] =
    persisted.buffers.get(bufferId).map { buffer =>
      annotationIndexByPanedBuffer.getOrElse(bufferId, computeAnnotationIndex(buffer))
    }

  private def computeAnnotationIndex(buffer: Buffer): AnnotationLineIndex =
    val diagnostics =
      runtime.languageService.diagnosticsState.diagnostics
        .getOrElse(com.serenity.spellcheck.SpellChecker.diagnosticsUri(buffer), Nil)
    AnnotationLineIndex(
      buffer.annotations.documentComments.toVector,
      diagnostics.groupMap(_.range.start.line)(identity)
    )

  /** `bufferId`'s semantic-tokens status -- precomputed if `bufferId` is placed in a pane
    * (`semanticTokensAvailabilityByPanedBuffer`), computed fresh on demand otherwise. See
    * [[SemanticTokensAvailability]] for what each case means and how the renderer treats it: `Pending` (no entry in
    * `runtime.languageService.semanticTokensState` at all yet) is deliberately distinct from `Unavailable` (confirmed
    * via `unavailableUris`) -- a request still in flight must not render the same muted style as a confirmed absence
    * (issue #859/#1177 rendering-slice review finding).
    */
  def semanticTokensAvailability(bufferId: BufferId): Option[SemanticTokensAvailability] =
    persisted.buffers.get(bufferId).map { buffer =>
      semanticTokensAvailabilityByPanedBuffer.getOrElse(bufferId, computeSemanticTokensAvailability(buffer))
    }

  private def computeSemanticTokensAvailability(buffer: Buffer): SemanticTokensAvailability =
    val uri = com.serenity.spellcheck.SpellChecker.diagnosticsUri(buffer)
    runtime.languageService.semanticTokensState.byUri.get(uri) match
      case Some(tokens) => SemanticTokensAvailability.Available(tokens.groupBy(_.line))
      case None =>
        if runtime.languageService.semanticTokensState.unavailableUris.contains(uri) then
          SemanticTokensAvailability.Unavailable
        else SemanticTokensAvailability.Pending

  /** `bufferId`'s markdown fence-range index, computed fresh on demand -- see the class-level comment on why this one,
    * unlike [[annotationIndex]] and [[semanticTokensAvailability]], is never precomputed for a paned buffer.
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
    * `AppConfig.effectiveCommandRunnerCursorGapRows` (`AppConfigMotionOps`), which read the raw `uiElementGap` field
    * and so had no way to tell a TUI session from a GUI one at all.
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

  /** The status text both placements show, or `None` when the status line is off or nothing is open. */
  def statusLineText: Option[String] =
    if !persisted.config.statusLine.isShown then None
    else StatusLineText.render(this, persisted.config.statusLine.segments)

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
      projectPresence = runtime.projectPresence
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
        nextSurfaceId = SurfaceIdSupply.initial
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
