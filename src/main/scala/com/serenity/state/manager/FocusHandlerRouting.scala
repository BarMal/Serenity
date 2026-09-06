package com.serenity.state.manager

import com.serenity.command.CommandRegistry
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.ui.layout.PanelPosition

/** The `SurfaceContent -> LocalEventHandler` and `PanelPosition -> LocalEventHandler` associations that
  * [[StateManagerEventPipeline.getLocalHandlerForFocus]] dispatches focused input through.
  *
  * Both associations are exhaustive matches with no wildcard case, so adding a new `SurfaceContent` or `PanelPosition`
  * case fails to compile here until its handler is declared -- there is no default a new case can silently fall into.
  *
  * Components are stateless apart from their constructor arguments (verified by inspection: none of the
  * `state.components` classes hold a `var` or mutable field), so every handler built from fixed, constructor-time data
  * is a `val`, built once and reused across every dispatch. The one exception is `ModalType.Custom`, whose `name` is
  * open-ended, plugin/extension-defined data -- the same kind of per-instance identity that `Focus.EditorPane`'s
  * `paneId` already carries -- so that branch alone builds a `ModalComponent` per dispatch.
  */
private[manager] object FocusHandlerRouting:

  private val registry = CommandRegistry.withToggleUI

  private val commandRunner: LocalEventHandler     = new CommandRunnerComponent(registry)
  private val themePicker: LocalEventHandler       = new ThemePickerComponent()
  private val themeCreator: LocalEventHandler      = new ThemeCreatorComponent()
  private val fileSearch: LocalEventHandler        = new FileSearchComponent()
  private val contextualToolbar: LocalEventHandler = new ContextualToolbarComponent(registry)
  private val commentLens: LocalEventHandler       = new CommentLensComponent()
  private val startupPage: LocalEventHandler       = new StartupPageComponent()

  /** Handler for floating "peek" content: read-only info popups and previews that only respond to dismiss/navigate (see
    * `PeekOverlayComponent`), plus content that is only ever presented Pinned or Expanded (`DirectoryTree`, `Terminal`,
    * `Outline`, `Comments`, `Diagnostics` -- see `UiSurface.fromPanelContent`, which is their only construction site)
    * and so never actually reaches this table in practice, and the transient `GhostOverlay` fade-out surface, which is
    * allocated under a fresh id that is never pushed onto the focus stack. All are routed here to match this codebase's
    * prior behaviour, where every one of them fell through a wildcard to `PeekOverlayComponent`.
    */
  private val peekOverlay: LocalEventHandler = new PeekOverlayComponent()

  private val modalGotoLine: LocalEventHandler        = new ModalComponent(ModalType.GotoLine)
  private val modalFind: LocalEventHandler            = new ModalComponent(ModalType.Find)
  private val modalFileWorkflow: LocalEventHandler    = new ModalComponent(ModalType.FileWorkflow)
  private val modalReplaceWorkflow: LocalEventHandler = new ModalComponent(ModalType.ReplaceWorkflow)
  private val modalCloseWorkflow: LocalEventHandler   = new ModalComponent(ModalType.CloseWorkflow)

  private val pinnedLeft: LocalEventHandler   = new PinnedPanelComponent(PanelPosition.Left)
  private val pinnedRight: LocalEventHandler  = new PinnedPanelComponent(PanelPosition.Right)
  private val pinnedBottom: LocalEventHandler = new PinnedPanelComponent(PanelPosition.Bottom)
  private val pinnedTop: LocalEventHandler    = new PinnedPanelComponent(PanelPosition.Top)

  private[manager] def forPinnedPanel(position: PanelPosition): LocalEventHandler =
    position match
      case PanelPosition.Left   => pinnedLeft
      case PanelPosition.Right  => pinnedRight
      case PanelPosition.Bottom => pinnedBottom
      case PanelPosition.Top    => pinnedTop

  private def forModalType(modalType: ModalType): LocalEventHandler =
    modalType match
      case ModalType.GotoLine        => modalGotoLine
      case ModalType.Find            => modalFind
      case ModalType.FileWorkflow    => modalFileWorkflow
      case ModalType.ReplaceWorkflow => modalReplaceWorkflow
      case ModalType.CloseWorkflow   => modalCloseWorkflow
      case custom: ModalType.Custom  => new ModalComponent(custom)

  /** The handler for a Modal- or Floating-presented surface, keyed purely by its content.
    *
    * `SurfacePresentation.Modal` surfaces are only ever constructed with `SurfaceContent.ModalWorkflow` (see
    * `ModalStateReducer.show`), so this single content-keyed table correctly serves both presentations -- there is no
    * separate Modal-only routing decision to keep in sync.
    */
  private[manager] def forSurfaceContent(content: SurfaceContent): LocalEventHandler =
    content match
      case SurfaceContent.CommandPalette(_) =>
        commandRunner
      case SurfaceContent.ThemePicker(_)       => themePicker
      case SurfaceContent.ThemeCreator(_)      => themeCreator
      case SurfaceContent.FileSearch(_)        => fileSearch
      case SurfaceContent.ContextualToolbar(_) => contextualToolbar
      case SurfaceContent.CommentLens(_)       => commentLens
      case SurfaceContent.StartPage(_)         => startupPage
      case SurfaceContent.ModalWorkflow(modal) => forModalType(ModalMouseHitTesting.modalType(modal))

      case SurfaceContent.QuickInfo(_)              => peekOverlay
      case SurfaceContent.FilePreview(_, _)         => peekOverlay
      case SurfaceContent.SymbolDefinition(_, _)    => peekOverlay
      case SurfaceContent.CursorInfoBar(_)          => peekOverlay
      case SurfaceContent.DirectoryListing(_, _, _) => peekOverlay
      case SurfaceContent.ContextMenu(_)            => peekOverlay
      case SurfaceContent.MarkdownPreview(_, _)     => peekOverlay
      case SurfaceContent.DirectoryTree(_, _)       => peekOverlay
      case SurfaceContent.Terminal(_, _)            => peekOverlay
      case SurfaceContent.Outline(_, _)             => peekOverlay
      case SurfaceContent.Comments(_, _)            => peekOverlay
      case SurfaceContent.Diagnostics(_, _)         => peekOverlay
      case SurfaceContent.GhostOverlay(_, _)        => peekOverlay
      case SurfaceContent.CompanionSprite           => peekOverlay
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
