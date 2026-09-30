package com.serenity.command

import com.serenity.config.AppMode
import com.serenity.state.models.{EditingContext, Shell}

/** Which workspace mode a command belongs to. */
enum CommandFamily(val modes: Set[AppMode]):
  case Core  extends CommandFamily(AppMode.values.toSet)
  case Code  extends CommandFamily(Set(AppMode.Code))
  case Prose extends CommandFamily(Set(AppMode.Prose))

/** Which frontends a command or setting has any effect in. */
enum FrontendSupport(val shells: Set[Shell]):
  case Both    extends FrontendSupport(Shell.values.toSet)
  case GuiOnly extends FrontendSupport(Set(Shell.Gui))

/** The kind of buffer a command acts on. Softer than [[CommandFamily]]: it ranks the palette's opening list and, for
  * rich text, asks before converting a plain buffer rather than hiding the command.
  */
enum BufferRequirement:
  case AnyBuffer
  case RichText
  case Markdown

extension (command: Command)
  def scope: CommandScope                  = CommandScope.of(command.intent)
  def bufferRequirement: BufferRequirement = CommandScope.bufferRequirementOf(command.intent)

final case class CommandScope(family: CommandFamily, frontend: FrontendSupport):

  def admits(mode: AppMode, shell: Shell): Boolean =
    family.modes.contains(mode) && frontend.shells.contains(shell)

  def admits(context: EditingContext): Boolean =
    admits(context.mode, context.shell)

  def unavailableReason(context: EditingContext): Option[String] =
    if !family.modes.contains(context.mode) then
      Some(s"Only available in ${family.modes.map(_.configKey).toList.sorted.mkString(" or ")} mode.")
    else if !frontend.shells.contains(context.shell) then Some("Only available in the graphical app.")
    else None

object CommandScope:

  val core: CommandScope = CommandScope(CommandFamily.Core, FrontendSupport.Both)

  /** Every command's scope comes from its intent, never from a per-command field, so an intent added without a
    * classification fails to compile (-Werror makes the non-exhaustive match an error) instead of silently defaulting.
    *
    * Settings changes are always Core: whether a settings *row* is shown is decided per row, but applying a value must
    * stay possible from anywhere -- a GUI-only setting edited from the terminal, or switching the app mode itself.
    */
  def of(intent: CommandIntent): CommandScope =
    CommandScope(familyOf(intent), FrontendSupport.Both)

  def bufferRequirementOf(intent: CommandIntent): BufferRequirement =
    intent match
      case CommandIntent.RichText(_)                          => BufferRequirement.RichText
      case CommandIntent.View(ViewIntent.OpenMarkdownPreview) => BufferRequirement.Markdown
      case _                                                  => BufferRequirement.AnyBuffer

  private def familyOf(intent: CommandIntent): CommandFamily =
    intent match
      case CommandIntent.Lsp(_) | CommandIntent.Project(_) => CommandFamily.Code
      case CommandIntent.RichText(_) | CommandIntent.Darlings(_) | CommandIntent.Placeholders(_) =>
        CommandFamily.Prose
      case CommandIntent.Edit(edit) => editFamily(edit)
      case CommandIntent.View(view) => viewFamily(view)
      case CommandIntent.Lifecycle(_) | CommandIntent.File(_) | CommandIntent.Comments(_) |
          CommandIntent.Navigation(_) | CommandIntent.Theme(_) | CommandIntent.Session(_) |
          CommandIntent.Keybindings(_) | CommandIntent.UiPresets(_) | CommandIntent.Settings(_) =>
        CommandFamily.Core

  private def editFamily(intent: EditIntent): CommandFamily =
    intent match
      case EditIntent.FormatCurrentFile => CommandFamily.Code
      case EditIntent.FindInCurrentFile | EditIntent.FindAllInCurrentFile | EditIntent.ReplaceInCurrentFile |
          EditIntent.ReplaceAllInCurrentFile | EditIntent.Copy | EditIntent.Cut | EditIntent.Paste |
          EditIntent.SelectAll | EditIntent.Undo | EditIntent.Redo =>
        CommandFamily.Core

  private def viewFamily(intent: ViewIntent): CommandFamily =
    intent match
      case ViewIntent.PinDiagnosticsPanel => CommandFamily.Code
      case ViewIntent.NextTab | ViewIntent.PreviousTab | ViewIntent.SplitPaneHorizontal | ViewIntent.SplitPaneVertical |
          ViewIntent.ClosePane | ViewIntent.FocusPanel(_) | ViewIntent.UnpinPanel(_) | ViewIntent.ExpandPanel(_) |
          ViewIntent.CollapseExpandedPanel | ViewIntent.MovePanelEarlier(_) | ViewIntent.MovePanelLater(_) |
          ViewIntent.PinExplorerPanel | ViewIntent.PinOutlinePanel | ViewIntent.PinCommentsPanel |
          ViewIntent.SetPanelPin(_, _) | ViewIntent.OpenMarkdownPreview | ViewIntent.SetMarkdownViewMode(_) |
          ViewIntent.SetDefaultDocumentMode(_) | ViewIntent.SetAppMode(_) |
          ViewIntent.SetShowAllSettingsRegardlessOfMode(_) | ViewIntent.ToggleShortcutsHelp | ViewIntent.ToggleTabList |
          ViewIntent.ToggleRecentFilesInMode | ViewIntent.TogglePanel(_) | ViewIntent.SetPanelSize(_, _) =>
        CommandFamily.Core
