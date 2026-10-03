package com.serenity.state.manager

import com.serenity.command.CommandRegistry
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalEventReducer
import com.serenity.ui.layout.{PanelPosition, WrappedLineCache}

/** The `SurfaceContent -> LocalEventHandler` and `PanelPosition -> LocalEventHandler` associations that
  * [[StateManagerEventPipeline.getLocalHandlerForFocus]] dispatches focused input through.
  *
  * Both associations are exhaustive matches with no wildcard case, so adding a new `SurfaceContent` or `PanelPosition`
  * case fails to compile here until its handler is declared -- there is no default a new case can silently fall into.
  *
  * Components are stateless apart from their constructor arguments (verified by inspection: none of the
  * `state.components` classes hold a `var` or mutable field), so every handler built from fixed, constructor-time data
  * is a `val`, built once per event pipeline and reused across every dispatch. The modal and pinned-panel handlers can
  * move the cursor and place the viewport, so they measure through the pipeline's `wrapCache`.
  */
final private[manager] class FocusHandlerRouting(wrapCache: WrappedLineCache):

  private val registry = CommandRegistry.withToggleUI

  private val commandRunner: LocalEventHandler     = new CommandRunnerComponent(registry)
  private val themeCreator: LocalEventHandler      = new ThemeCreatorComponent()
  private val contextualToolbar: LocalEventHandler = new ContextualToolbarComponent
  private val commentLens: LocalEventHandler       = new CommentLensComponent()
  private val startupPage: LocalEventHandler       = new StartupPageComponent()

  /** Handler for floating "peek" content: read-only info popups and previews that only respond to dismiss/navigate (see
    * `PeekOverlayComponent`), plus content that is only ever presented Docked (`DirectoryTree`, `Terminal`, `Outline`,
    * `Comments`, `Diagnostics` -- see `UiSurface.fromPanelContent`, which is their only construction site) and so never
    * actually reaches this table in practice, and the transient `GhostOverlay` fade-out surface, which is allocated
    * under a fresh id that is never pushed onto the focus stack. All are routed here to match this codebase's prior
    * behaviour, where every one of them fell through a wildcard to `PeekOverlayComponent`.
    */
  private val peekOverlay: LocalEventHandler = new PeekOverlayComponent()
  private val contextMenu: LocalEventHandler = new ContextMenuComponent()

  private val modalTextPrompt: LocalEventHandler   = new ModalComponent(ModalType.TextPrompt, wrapCache = wrapCache)
  private val modalFind: LocalEventHandler         = new ModalComponent(ModalType.Find, wrapCache = wrapCache)
  private val modalFileWorkflow: LocalEventHandler = new ModalComponent(ModalType.FileWorkflow, wrapCache = wrapCache)
  private val modalReplaceWorkflow: LocalEventHandler =
    new ModalComponent(ModalType.ReplaceWorkflow, wrapCache = wrapCache)
  private val modalConfirm: LocalEventHandler    = new ModalComponent(ModalType.Confirm, wrapCache = wrapCache)
  private val modalListPicker: LocalEventHandler = new ModalComponent(ModalType.ListPicker, wrapCache = wrapCache)
  private val modalPanelArrangement: LocalEventHandler =
    new ModalComponent(ModalType.PanelArrangement, wrapCache = wrapCache)

  private val pinnedLeft: LocalEventHandler   = new PinnedPanelComponent(PanelPosition.Left, wrapCache = wrapCache)
  private val pinnedRight: LocalEventHandler  = new PinnedPanelComponent(PanelPosition.Right, wrapCache = wrapCache)
  private val pinnedBottom: LocalEventHandler = new PinnedPanelComponent(PanelPosition.Bottom, wrapCache = wrapCache)
  private val pinnedTop: LocalEventHandler    = new PinnedPanelComponent(PanelPosition.Top, wrapCache = wrapCache)

  private[manager] def forPinnedPanel(position: PanelPosition): LocalEventHandler =
    position match
      case PanelPosition.Left   => pinnedLeft
      case PanelPosition.Right  => pinnedRight
      case PanelPosition.Bottom => pinnedBottom
      case PanelPosition.Top    => pinnedTop

  private[manager] def forModalType(modalType: ModalType): LocalEventHandler =
    modalType match
      case ModalType.TextPrompt       => modalTextPrompt
      case ModalType.Find             => modalFind
      case ModalType.FileWorkflow     => modalFileWorkflow
      case ModalType.ReplaceWorkflow  => modalReplaceWorkflow
      case ModalType.Confirm          => modalConfirm
      case ModalType.ListPicker       => modalListPicker
      case ModalType.PanelArrangement => modalPanelArrangement

  /** The handler for a Floating-presented surface, keyed purely by its content. Blocking dialogs (#814) are no longer
    * `UiSurface`s at all -- they live on `runtime.modalStack` and focus as `Focus.Modal`, routed by
    * `StateManagerEventPipeline.getLocalHandlerForFocus` via `forModalType` directly, not through this table.
    */
  private[manager] def forSurfaceContent(content: SurfaceContent): LocalEventHandler =
    content match
      case SurfaceContent.CommandPalette(_) =>
        commandRunner
      case SurfaceContent.ThemeCreator(_)      => themeCreator
      case SurfaceContent.ContextualToolbar(_) => contextualToolbar
      case SurfaceContent.CommentLens(_)       => commentLens
      case SurfaceContent.StartPage(_)         => startupPage
      case SurfaceContent.ModalWorkflow(modal) => forModalType(ModalEventReducer.modalType(modal))

      case SurfaceContent.QuickInfo(_)              => peekOverlay
      case SurfaceContent.FilePreview(_, _)         => peekOverlay
      case SurfaceContent.SymbolDefinition(_, _)    => peekOverlay
      case SurfaceContent.StatusLine(_)             => peekOverlay
      case SurfaceContent.DirectoryListing(_, _, _) => peekOverlay
      case SurfaceContent.ContextMenu(_)            => contextMenu
      case SurfaceContent.MarkdownPreview(_, _)     => peekOverlay
      case SurfaceContent.DirectoryTree(_, _)       => peekOverlay
      case SurfaceContent.Terminal(_, _)            => peekOverlay
      case SurfaceContent.Outline(_, _)             => peekOverlay
      case SurfaceContent.Comments(_, _)            => peekOverlay
      case SurfaceContent.Diagnostics(_, _)         => peekOverlay
      case SurfaceContent.GhostOverlay(_, _)        => peekOverlay
      // Cursor-peek prototype: never focused in practice (look-but-don't-touch), but routed as a read-only peek
      // overlay rather than left unhandled, matching every other passive preview content case above.
      case SurfaceContent.CommandRunnerPeek(_) => peekOverlay
      // Shortcuts-help reference (issue #1247): `AppEventReducer.toggleShortcutsHelp` never pushes focus to it
      // either, for the same "look but don't touch" reason -- routed here only so this table stays exhaustive.
      case SurfaceContent.ShortcutsHelp(_) => peekOverlay
      // Mode/tab corner widget's tab list and recent-in-mode list (issue #1307): same "look but don't touch"
      // toggle-only pattern as ShortcutsHelp above.
      case SurfaceContent.TabList(_, _)           => peekOverlay
      case SurfaceContent.RecentFilesInMode(_, _) => peekOverlay
      // Tab strip (issue #1075/#1076): the strip itself is never pushed onto the focus stack -- a click switches the
      // active pane's buffer (issue #1077) or opens a new tab from the trailing `+` affordance (issue #1080) via
      // `MouseHitTesting`/`TabBarMouseHitTesting`, without ever routing through this table's focused-handler
      // dispatch. Close/reorder (#1078/#1079/#1081) remain out of scope. Routed here only so this table stays
      // exhaustive, same "look but don't touch" pattern as TabList.
      case SurfaceContent.TabBar(_, _) => peekOverlay
