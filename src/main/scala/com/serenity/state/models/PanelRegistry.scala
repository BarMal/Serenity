package com.serenity.state.models

import com.serenity.animation.sprite.CompanionSpriteConfig
import com.serenity.command.{CommandFamily, FrontendSupport}
import com.serenity.ui.layout.{PanelContent, PanelPosition}

/** Every panel the workspace can show, registered once with [[PanelRegistry]] (issue #1310). An enum rather than an
  * open string id so that [[PanelRegistry.registrationFor]], and every match on a panel, is checked for exhaustiveness:
  * a panel added here without a registration doesn't compile.
  */
enum PanelId(val key: String):
  case Explorer        extends PanelId("explorer")
  case Outline         extends PanelId("outline")
  case Comments        extends PanelId("comments")
  case Diagnostics     extends PanelId("diagnostics")
  case MarkdownPreview extends PanelId("markdown-preview")
  case ProjectOutput   extends PanelId("project-output")
  case Companion       extends PanelId("companion")

  /** The surface id this panel is always docked under, so there is at most one of each. */
  def surfaceId: SurfaceId = SurfaceId(s"panel-$key")

object PanelId:

  def of(content: PanelContent): PanelId =
    content match
      case PanelContent.DirectoryTree(_, _)   => Explorer
      case PanelContent.Outline(_, _)         => Outline
      case PanelContent.Comments(_, _)        => Comments
      case PanelContent.Diagnostics(_, _)     => Diagnostics
      case PanelContent.MarkdownPreview(_, _) => MarkdownPreview
      case PanelContent.Terminal(_, _)        => ProjectOutput
      case PanelContent.CompanionSprite       => Companion

  /** Exhaustive over [[SurfaceContent]] on purpose: a new kind of content has to decide whether it is a panel. */
  def forContent(content: SurfaceContent): Option[PanelId] =
    content match
      case SurfaceContent.DirectoryTree(_, _)       => Some(Explorer)
      case SurfaceContent.Outline(_, _)             => Some(Outline)
      case SurfaceContent.Comments(_, _)            => Some(Comments)
      case SurfaceContent.Diagnostics(_, _)         => Some(Diagnostics)
      case SurfaceContent.MarkdownPreview(_, _)     => Some(MarkdownPreview)
      case SurfaceContent.StartPage(_)              => None
      case SurfaceContent.QuickInfo(_)              => None
      case SurfaceContent.FilePreview(_, _)         => None
      case SurfaceContent.SymbolDefinition(_, _)    => None
      case SurfaceContent.StatusLine(_)             => None
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
      case SurfaceContent.Terminal(_, _)            => Some(ProjectOutput)
      case SurfaceContent.ShortcutsHelp(_)          => None
      case SurfaceContent.TabList(_, _)             => None
      case SurfaceContent.RecentFilesInMode(_, _)   => None
      case SurfaceContent.TabBar(_, _)              => None
      case SurfaceContent.GhostOverlay(_, _)        => None
      case SurfaceContent.CompanionSprite           => Some(Companion)

/** A display mode a registered panel can be shown through (issue #1310). Shortcut-summoned (mode 2) is deliberately
  * absent until #1311's chord system exposes a `Command`-typed completion to register against -- adding a case nothing
  * can use yet would be exactly the kind of stub CLAUDE.md rules out.
  */
enum PanelDisplayMode:
  case Palette
  case Corner
  case Dock

/** One panel's identity, label, default placement, and where it is offered: the mode family and frontends its commands
  * belong to, and whether it can take keyboard focus. `paletteContent` builds the panel's floating presentation for
  * [[PanelDisplayMode.Palette]]; docked content is derived from state by the panel itself (`PanelContentSync`), so a
  * dock-only panel has none.
  */
final case class PanelRegistration(
    id: PanelId,
    label: String,
    description: String,
    defaultPosition: PanelPosition,
    defaultSize: PanelPosition => Int,
    family: CommandFamily,
    frontend: FrontendSupport,
    focusable: Boolean,
    supportedModes: Set[PanelDisplayMode],
    paletteContent: Option[AppState => SurfaceContent] = None
)

/** Panels registered once, consulted by every display mode instead of each hand-adding a case to
  * `ViewIntent`/`Command`/`GlobalAppEvent` per panel -- the cost #1307 paid twice for `TabList`/`RecentFilesInMode`.
  */
final case class PanelRegistry(registrations: Map[PanelId, PanelRegistration]):

  def get(id: PanelId): Option[PanelRegistration] = registrations.get(id)

  def all: List[PanelRegistration] = registrations.values.toList

  def supporting(mode: PanelDisplayMode): List[PanelRegistration] = all.filter(_.supportedModes.contains(mode))

object PanelRegistry:

  def apply(registrations: List[PanelRegistration]): PanelRegistry =
    PanelRegistry(registrations.map(registration => registration.id -> registration).toMap)

  val empty: PanelRegistry = PanelRegistry(Map.empty[PanelId, PanelRegistration])

  val default: PanelRegistry = PanelRegistry(PanelId.values.toList.map(registrationFor))

  def registrationFor(id: PanelId): PanelRegistration =
    id match
      case PanelId.Explorer =>
        docked(id, "Explorer", "Browse the files under the working directory.", PanelPosition.Left)
      case PanelId.Outline =>
        docked(id, "Outline", "The active document's headings, bookmarks and placeholders.", PanelPosition.Right)
      case PanelId.Comments =>
        docked(id, "Comments", "The active document's comments.", PanelPosition.Right)
      case PanelId.Diagnostics =>
        docked(id, "Diagnostics", "Language-server and spelling issues in the active document.", PanelPosition.Bottom)
          .copy(family = CommandFamily.Code)
      // Docked only in the GUI: a cell surface can't draw the rendered preview, so the terminal opens it in a window
      // of its own instead (`ViewIntent.OpenMarkdownPreview`).
      case PanelId.MarkdownPreview =>
        docked(id, "Markdown Preview", "A rendered preview of the active Markdown document.", PanelPosition.Right)
          .copy(defaultSize = _ => 40, frontend = FrontendSupport.GuiOnly)
      case PanelId.ProjectOutput =>
        docked(id, "Project Output", "Output from the latest build, test or run task.", PanelPosition.Bottom)
          .copy(defaultSize = _ => 14, family = CommandFamily.Code)
      case PanelId.Companion =>
        docked(id, "Companion", "A small pixel-art companion that reacts to your typing.", PanelPosition.Right)
          .copy(defaultSize = _ => CompanionSpriteConfig.DefaultSize, focusable = false)

  private def docked(id: PanelId, label: String, description: String, position: PanelPosition): PanelRegistration =
    PanelRegistration(
      id,
      label,
      description,
      position,
      sideOrEdgeSize,
      CommandFamily.Core,
      FrontendSupport.Both,
      focusable = true,
      Set(PanelDisplayMode.Dock)
    )

  private def sideOrEdgeSize(position: PanelPosition): Int =
    position match
      case PanelPosition.Left | PanelPosition.Right => 30
      case PanelPosition.Top | PanelPosition.Bottom => 10
