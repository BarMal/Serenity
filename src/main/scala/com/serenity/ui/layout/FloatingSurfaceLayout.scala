package com.serenity.ui.layout

import com.serenity.config.{AppConfig, InterfaceDensityMetrics}
import com.serenity.state.models.*

/** Rect, size and cursor-anchor resolution for *one* floating surface -- both the live, cursor-tracking path and the
  * cursor-peek prototype's frozen-anchor variant.
  *
  * This is the single-surface primitive layer of floating layout, and everything below is its API rather than an
  * internal detail: [[OverlayStackLayout]] composes these into multi-surface stacks and `LayoutEngine` uses them
  * directly for the above-cursor stack and modal centring. The dependency runs one way only -- nothing here knows about
  * stacking -- which is why the two objects stay separate instead of being one file.
  */
object FloatingSurfaceLayout:

  def calculateFloatingSurfaceRect(
    surface: UiSurface,
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout],
    topYOverride: Option[Int] = None,
    forcedHeight: Option[Int] = None
  ): Option[LayoutRect] =
    for
      paneId     <- state.persisted.layout.activeEditorPaneId
      pane       <- state.persisted.layout.editorPanes.get(paneId)
      paneLayout <- paneLayouts.get(paneId)
      bufferId   <- pane.bufferId
      buffer     <- state.persisted.buffers.get(bufferId)
      rect <- calculateFloatingSurfaceRect(surface, buffer, paneLayout.contentRect, state, topYOverride, forcedHeight)
    yield rect

  private def calculateFloatingSurfaceRect(
    surface: UiSurface,
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    topYOverride: Option[Int],
    forcedHeight: Option[Int]
  ): Option[LayoutRect] =
    surface.content match
      case SurfaceContent.CommandRunnerPeek(_) =>
        calculateFrozenCursorPeekRect(surface, contentRect, state)
      case _ =>
        calculateLiveFloatingSurfaceRect(surface, buffer, contentRect, state, topYOverride, forcedHeight)

  /** The cursor-peek prototype's own rect resolution -- deliberately never calls [[floatingAnchor]] or
    * `CursorLayout.calculateScreenPositionInContent`: `state.runtime.cursorPeekResolvedAnchor` was already resolved
    * once, at render time, by `CursorPeekAnchorResolution` (state.manager), and is reused verbatim here on every
    * subsequent paint rather than re-derived, so a reformat underneath an open peek cannot move it. Sizing
    * (`calculateFloatingSurfaceWidth`/`calculateFloatingSurfaceHeight`) is still shared with the live path for a
    * consistent look.
    */
  private def calculateFrozenCursorPeekRect(
    surface: UiSurface,
    contentRect: LayoutRect,
    state: AppState
  ): Option[LayoutRect] =
    for
      anchorScreenPosition <- state.runtime.cursorPeekResolvedAnchor
      placement <- surface.presentation match
        case SurfacePresentation.Floating(_, p) => Some(p)
        case _                                  => None
      preferredWidth  = calculateFloatingSurfaceWidth(contentRect.width)
      preferredHeight = calculateFloatingSurfaceHeight(surface.content, preferredWidth, contentRect.height, state)
      gapRows         = wholeRowOrigin(floatingCursorGapRows(state, surface.content))
      slot            = FrozenPeekSlot(surface.id, preferredWidth, preferredHeight)
      placed <- resolveFrozenCursorPeekStack(
        List(slot),
        anchorScreenPosition,
        contentRect,
        placement,
        gapRows
      ).headOption
    yield placed.rect

  private def calculateLiveFloatingSurfaceRect(
    surface: UiSurface,
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    topYOverride: Option[Int],
    forcedHeight: Option[Int]
  ): Option[LayoutRect] =
    val borderCells = SurfaceFrameLayout.borderCellsFor(surface.content)
    val preferredWidth = surface.content match
      case SurfaceContent.ContextualToolbar(toolbarState) =>
        ContextualToolbarLayout.compactContentWidth(
          toolbarState,
          state,
          contentRect.width - (borderCells * 2)
        ) + (borderCells * 2)
      case SurfaceContent.CommandPalette(_) | SurfaceContent.ShortcutsHelp(_) |
          SurfaceContent.ModalWorkflow(Modal.FileWorkflow(_)) =>
        calculateFloatingSurfaceWidth(contentRect.width)
      case _ =>
        contentRect.width
    val preferredHeight = calculateFloatingSurfaceHeight(surface.content, preferredWidth, contentRect.height, state)
    val finalHeight     = forcedHeight.getOrElse(preferredHeight)
    val gapRows         = wholeRowOrigin(floatingCursorGapRows(state, surface.content))

    for
      anchor <- floatingAnchor(surface, state, buffer)
      screenPosition <- CursorLayout.calculateScreenPositionInContent(
        anchor,
        buffer.document.content,
        contentRect,
        buffer.viewport,
        state.persisted.config.surfaceConfig.wordWrapEnabled
      )
      if surface.content match
        case SurfaceContent.ContextualToolbar(_) => contentRect.contains(screenPosition.x, screenPosition.y)
        case _                                   => true
    yield
      val horizontalAnchorX = toolbarSelectionEndScreenPosition(surface, buffer, contentRect, state)
        .filter(_.y == screenPosition.y)
        .map(selectionEnd => (screenPosition.x + selectionEnd.x) / 2)
        .getOrElse(screenPosition.x)
      val overlayX = surface.content match
        // The command palette/settings surface -- and the shortcuts-help reference (issue #1247), for the same
        // reason -- is horizontally centered on screen, not cursor-anchored: unlike the contextual toolbar (anchored
        // to a text selection) or other floating content, its width and content bear no relationship to the cursor's
        // horizontal position, and cursor-anchoring left it pinned near whichever column the caret happened to be in
        // -- often far from center, sometimes hard against an edge. Vertical placement (above/below the cursor,
        // `overlayY` below) is unaffected.
        case SurfaceContent.CommandPalette(_) | SurfaceContent.ShortcutsHelp(_) |
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(_)) =>
          contentRect.x + math.max(0, (contentRect.width - preferredWidth) / 2)
        case _ =>
          math.max(
            contentRect.x,
            math.min(horizontalAnchorX - (preferredWidth / 2), contentRect.right - preferredWidth)
          )
      val preferredAboveY = screenPosition.y - finalHeight - gapRows
      val preferredBelowY = toolbarSelectionEndScreenPosition(surface, buffer, contentRect, state)
        .map(_.y + 1 + gapRows)
        .getOrElse(screenPosition.y + 1 + gapRows)
      val overlayY = topYOverride.getOrElse(surface.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) =>
          surface.content match
            case SurfaceContent.ContextualToolbar(_)
                if preferredAboveY < contentRect.y &&
                  preferredBelowY + finalHeight <= contentRect.bottom =>
              preferredBelowY
            case _ =>
              math.max(contentRect.y, preferredAboveY)
        case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) =>
          surface.content match
            case SurfaceContent.ContextualToolbar(_) if preferredAboveY >= contentRect.y =>
              preferredAboveY
            case _ if preferredBelowY + finalHeight <= contentRect.bottom =>
              preferredBelowY
            case _ =>
              math.max(contentRect.y, screenPosition.y - finalHeight - gapRows)
        case _ =>
          contentRect.y)

      LayoutRect(
        x = overlayX,
        y = overlayY,
        width = preferredWidth,
        height = finalHeight
      )

  private def toolbarSelectionEndScreenPosition(
    surface: UiSurface,
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState
  ): Option[ScreenPosition] =
    surface.content match
      case SurfaceContent.ContextualToolbar(_) =>
        buffer.primarySelection.flatMap(selection =>
          CursorLayout.calculateScreenPositionInContent(
            selection.end,
            buffer.document.content,
            contentRect,
            buffer.viewport,
            state.persisted.config.surfaceConfig.wordWrapEnabled
          )
        )
      case _ =>
        None

  final case class FloatingAnchorFrame(contentRect: LayoutRect, screenPosition: ScreenPosition)

  def calculateFloatingAnchorFrame(
    surface: UiSurface,
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout]
  ): Option[FloatingAnchorFrame] =
    for
      paneId     <- state.persisted.layout.activeEditorPaneId
      pane       <- state.persisted.layout.editorPanes.get(paneId)
      paneLayout <- paneLayouts.get(paneId)
      bufferId   <- pane.bufferId
      buffer     <- state.persisted.buffers.get(bufferId)
      anchor     <- floatingAnchor(surface, state, buffer)
      screenPosition <- CursorLayout.calculateScreenPositionInContent(
        anchor,
        buffer.document.content,
        paneLayout.contentRect,
        buffer.viewport,
        state.persisted.config.surfaceConfig.wordWrapEnabled
      )
    yield FloatingAnchorFrame(paneLayout.contentRect, screenPosition)

  private def calculateFloatingSurfaceWidth(maxWidth: Int): Int =
    math.min(math.max(0, maxWidth), 72)

  def floatingCursorGapRows(state: AppState, content: SurfaceContent): Double =
    content match
      case SurfaceContent.CommandPalette(_) =>
        math.max(
          0.0,
          state.persisted.config.surfaceConfig.commandRunnerCursorGapRows.getOrElse(floatingStackGapRows(state))
        )
      case _ => floatingStackGapRows(state)

  def floatingStackGapRows(state: AppState): Double =
    Option
      .when(state.persisted.config.uiElementGap > 0.0)(state.persisted.config.uiElementGap)
      .getOrElse(InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity).overlayGapRows.toDouble)

  def wholeRowOrigin(rows: Double): Int =
    math.floor(math.max(0.0, rows)).toInt

  def calculateFloatingSurfaceHeight(
    content: SurfaceContent,
    maxWidth: Int,
    maxHeight: Int,
    state: AppState
  ): Int =
    val densityMetrics = InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity)
    val commandMaxHeight =
      state.persisted.config.surfaceConfig.commandRunnerVisibleRows
        .map(rows =>
          SurfaceFrameLayout.frameHeightForItemRows(
            AppConfig.clampCommandRunnerVisibleRows(rows),
            hasHeader = true,
            hasFooter = true,
            borderCells = SurfaceFrameLayout.CommandSurfaceBorderCells,
            itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
            itemTargetRows = SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
          )
        )
        .getOrElse(densityMetrics.commandSurfaceMaxHeight)
    val preferredHeight = content match
      case SurfaceContent.StartPage(_)            => maxHeight
      case SurfaceContent.QuickInfo(text)         => math.max(3, text.linesIterator.size + 2)
      case SurfaceContent.FilePreview(_, content) => math.max(4, math.min(6, content.linesIterator.take(4).size + 2))
      case SurfaceContent.SymbolDefinition(_, _)  => 4
      case SurfaceContent.CursorInfoBar(_)        => 3
      case SurfaceContent.DirectoryListing(_, entries, _) => math.max(4, math.min(6, entries.take(4).size + 2))
      case SurfaceContent.DirectoryTree(tree, _) =>
        math.max(4, math.min(8, DirectoryTreeData.visibleRows(tree).size + 2))
      case SurfaceContent.CommandPalette(runner) if runner.isSettingsSurface =>
        math.min(commandMaxHeight, math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1))
      case SurfaceContent.CommandPalette(_) =>
        math.min(
          commandMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.CommandRunnerPeek(_) =>
        math.min(
          commandMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.ThemePicker(_) | SurfaceContent.ThemeCreator(_) | SurfaceContent.FileSearch(_) =>
        math.min(
          densityMetrics.commandSurfaceMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.ContextualToolbar(toolbarState) =>
        val borderCells  = SurfaceFrameLayout.borderCellsFor(content)
        val contentWidth = (maxWidth - (borderCells * 2)).max(1)
        val toolbarRows  = ContextualToolbarLayout.rowCount(toolbarState, state, contentWidth)
        SurfaceFrameLayout.frameHeightForItemRows(
          toolbarRows,
          hasHeader = false,
          hasFooter = false,
          borderCells = borderCells,
          itemGapRows = state.persisted.config.uiElementGap,
          itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.ContextMenu(menu) =>
        SurfaceFrameLayout.frameHeightForItemRows(
          itemRows = menu.items.length,
          hasHeader = true,
          hasFooter = menu.items.nonEmpty,
          borderCells = SurfaceFrameLayout.borderCellsFor(content),
          itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
          itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.CommentLens(lens) =>
        math.max(4, math.min(8, lens.draft.split("\n", -1).length + 3))
      case SurfaceContent.ModalWorkflow(modal) =>
        ModalSurfaceComposition.frameHeight(
          modal,
          SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_, _) | SurfaceContent.Comments(_, _) |
          SurfaceContent.Diagnostics(_, _) | SurfaceContent.MarkdownPreview(_, _) | SurfaceContent.CompanionSprite =>
        math.min(8, math.max(4, maxHeight - 1))
      case SurfaceContent.ShortcutsHelp(groups) =>
        // Wants enough rows for every group heading plus its entries, but never more than the viewport allows --
        // `resolveShortcutsHelp` clips to whatever height it is actually given.
        math.min(maxHeight - 1, math.max(4, groups.map(g => g.entries.size + 1).sum + 2))
      case SurfaceContent.TabList(entries, _) =>
        // Scales with the number of open tabs rather than the fixed `commandSurfaceMaxHeight` cap the command
        // palette itself is still capped by (issue #1045) -- a many-tab session should see all of its tabs, not
        // just the ~3 that cap would allow.
        math.min(maxHeight - 1, math.max(4, entries.size + 2))
      case SurfaceContent.RecentFilesInMode(_, paths) =>
        math.min(maxHeight - 1, math.max(4, paths.size + 2))
      case SurfaceContent.GhostOverlay(_, cachedRect) =>
        cachedRect.height

    math.max(3, math.min(maxHeight, preferredHeight))

  private def surfaceAnchor(surface: UiSurface): Option[CursorPosition] =
    surface.presentation match
      case SurfacePresentation.Floating(anchor, _) => anchor
      case _                                       => None

  private def floatingAnchor(
    surface: UiSurface,
    state: AppState,
    activeBuffer: Buffer
  ): Option[CursorPosition] =
    surface.content match
      case SurfaceContent.ContextualToolbar(_) =>
        activeBuffer.primarySelection.map(_.start).orElse(state.activeCursorPosition).orElse(surfaceAnchor(surface))
      case SurfaceContent.CommandPalette(_) =>
        state.activeCursorPosition.orElse(surfaceAnchor(surface))
      case _ =>
        surfaceAnchor(surface).orElse(state.activeCursorPosition)

  /** A panel's slot in a frozen cursor-peek stack: an id plus preferred size. Ordered-list-with-insertion shape
    * (`List[FrozenPeekSlot]`, not a single surface) so [[resolveFrozenCursorPeekStack]] is already the general
    * multi-panel case even though the cursor-peek prototype only ever passes one slot today.
    */
  final case class FrozenPeekSlot(id: SurfaceId, preferredWidth: Int, preferredHeight: Int)

  final case class FrozenPeekPlacement(id: SurfaceId, rect: LayoutRect)

  /** Resolves a frozen-anchor cursor-peek stack, box-layout style: each slot in `slots` is stacked in order starting
    * from `anchorScreenPosition`, on the side `placement` prefers, falling back to the other side and then clamping
    * within `contentRect` when neither side has room -- the same height-budget clamp
    * `OverlayStackLayout.stackBelowCursorSurfaces` already uses for its own (live) stack, reused rather than inventing
    * a second overflow mechanism. A slot with no height budget left is dropped from the result entirely rather than
    * rendered at zero height.
    *
    * Deliberately distinct from [[floatingAnchor]]/[[calculateFloatingSurfaceRect]]: `anchorScreenPosition` is supplied
    * once by the caller (captured from the cursor's *line* at summon time) rather than derived here from `AppState`/the
    * active buffer, so this function never re-reads live cursor state -- callers that want the cursor-peek prototype's
    * "frozen for the whole session, even across a reformat" behaviour resolve the anchor once and hold onto the result;
    * callers of the existing floating surfaces keep re-deriving it every layout pass, unchanged.
    */
  def resolveFrozenCursorPeekStack(
    slots: List[FrozenPeekSlot],
    anchorScreenPosition: ScreenPosition,
    contentRect: LayoutRect,
    placement: SurfacePlacement,
    gapRows: Int
  ): List[FrozenPeekPlacement] =
    if slots.isEmpty then Nil
    else
      val totalHeight     = slots.map(_.preferredHeight).sum + (gapRows * (slots.length - 1).max(0))
      val preferredBelowY = anchorScreenPosition.y + 1 + gapRows
      val preferredAboveY = anchorScreenPosition.y - gapRows - totalHeight
      val fitsBelow       = preferredBelowY + totalHeight <= contentRect.bottom
      val fitsAbove       = preferredAboveY >= contentRect.y

      def clamped: Int =
        math.max(
          contentRect.y,
          math.min(preferredBelowY, contentRect.bottom - math.min(totalHeight, contentRect.height))
        )

      val stackY = placement match
        case SurfacePlacement.BelowCursor =>
          if fitsBelow then preferredBelowY else if fitsAbove then preferredAboveY else clamped
        case SurfacePlacement.AboveCursor =>
          if fitsAbove then preferredAboveY else if fitsBelow then preferredBelowY else clamped
        case SurfacePlacement.Corner(_) =>
          // Never actually reached: `resolveFrozenCursorPeekStack` is only ever called with the cursor-peek
          // prototype's own AboveCursor/BelowCursor placement (`surfaceConfig.commandRunnerCursorPeekPlacement`).
          // Kept exhaustive, with the same fallback as BelowCursor, rather than partial.
          if fitsBelow then preferredBelowY else if fitsAbove then preferredAboveY else clamped

      val (_, placed) = slots.foldLeft((stackY, List.empty[FrozenPeekPlacement])) {
        case ((currentY, acc), slot) =>
          val heightBudget   = math.max(0, contentRect.bottom - currentY)
          val adjustedHeight = math.min(slot.preferredHeight, heightBudget)
          val adjustedY      = if adjustedHeight == 0 then contentRect.bottom else currentY
          val width          = math.min(slot.preferredWidth, contentRect.width)
          val x = math.max(
            contentRect.x,
            math.min(anchorScreenPosition.x - (width / 2), contentRect.right - width)
          )
          val rect = LayoutRect(x = x, y = adjustedY, width = width, height = adjustedHeight)
          (adjustedY + adjustedHeight + gapRows, acc :+ FrozenPeekPlacement(slot.id, rect))
      }
      placed.filter(_.rect.height > 0)
