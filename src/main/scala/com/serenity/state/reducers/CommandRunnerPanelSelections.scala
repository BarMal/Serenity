package com.serenity.state.reducers

import com.serenity.command.PanelKind
import com.serenity.state.models.*
import com.serenity.ui.layout.PanelPosition

private[serenity] object CommandRunnerPanelSelections:

  def fromState(state: AppState): Map[String, Int] =
    List(
      PanelKind.Explorer,
      PanelKind.Outline,
      PanelKind.Comments,
      PanelKind.Diagnostics,
      PanelKind.MarkdownPreview
    ).map(kind => optionId(kind) -> selectedIndex(kind, state)).toMap

  private def selectedIndex(kind: PanelKind, state: AppState): Int =
    state.runtime.uiSurfaces.reverse
      .find(surface => panelKind(surface.content).contains(kind))
      .flatMap(positionOf)
      .map(positionIndex)
      .getOrElse(0)

  private def optionId(kind: PanelKind): String =
    kind match
      case PanelKind.Explorer        => "panel-explorer-pin"
      case PanelKind.Outline         => "panel-outline-pin"
      case PanelKind.Comments        => "panel-comments-pin"
      case PanelKind.Diagnostics     => "panel-diagnostics-pin"
      case PanelKind.MarkdownPreview => "panel-markdown-preview-pin"

  private def panelKind(content: SurfaceContent): Option[PanelKind] =
    content match
      case SurfaceContent.DirectoryTree(_, _)       => Some(PanelKind.Explorer)
      case SurfaceContent.Outline(_, _)             => Some(PanelKind.Outline)
      case SurfaceContent.Comments(_, _)            => Some(PanelKind.Comments)
      case SurfaceContent.Diagnostics(_, _)         => Some(PanelKind.Diagnostics)
      case SurfaceContent.MarkdownPreview(_, _)     => Some(PanelKind.MarkdownPreview)
      case SurfaceContent.StartPage(_)              => None
      case SurfaceContent.QuickInfo(_)              => None
      case SurfaceContent.FilePreview(_, _)         => None
      case SurfaceContent.SymbolDefinition(_, _)    => None
      case SurfaceContent.CursorInfoBar(_)          => None
      case SurfaceContent.DirectoryListing(_, _, _) => None
      case SurfaceContent.CommandPalette(_)         => None
      case SurfaceContent.CommandRunnerPeek(_)      => None
      case SurfaceContent.ThemePicker(_)            => None
      case SurfaceContent.ThemeCreator(_)           => None
      case SurfaceContent.FileSearch(_)             => None
      case SurfaceContent.ContextualToolbar(_)      => None
      case SurfaceContent.ContextMenu(_)            => None
      case SurfaceContent.CommentLens(_)            => None
      case SurfaceContent.ModalWorkflow(_)          => None
      case SurfaceContent.Terminal(_, _)            => None
      case SurfaceContent.ShortcutsHelp(_)          => None
      case SurfaceContent.GhostOverlay(_, _)        => None

  private def positionOf(surface: UiSurface): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _)   => Some(position)
      case SurfacePresentation.Expanded(position, _) => Some(position)
      case SurfacePresentation.Floating(_, _)        => None
      case SurfacePresentation.Modal                 => None

  private def positionIndex(position: PanelPosition): Int =
    position match
      case PanelPosition.Top    => 1
      case PanelPosition.Right  => 2
      case PanelPosition.Bottom => 3
      case PanelPosition.Left   => 4
