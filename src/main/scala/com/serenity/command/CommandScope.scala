package com.serenity.command

import com.serenity.config.AppMode
import com.serenity.state.models.{EditingContext, PanelId, PanelRegistry, Shell}

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
    intent match
      case CommandIntent.View(view) => viewScope(view)
      case other                    => CommandScope(familyOf(other), FrontendSupport.Both)

  def bufferRequirementOf(intent: CommandIntent): BufferRequirement =
    intent match
      case CommandIntent.RichText(_)                             => BufferRequirement.RichText
      case CommandIntent.View(ViewIntent.OpenMarkdownPreview)    => BufferRequirement.Markdown
      case CommandIntent.View(ViewIntent.ToggleMarkdownReadMode) => BufferRequirement.Markdown
      case _                                                     => BufferRequirement.AnyBuffer

  private def familyOf(intent: CommandIntent): CommandFamily =
    intent match
      case CommandIntent.Lsp(_) | CommandIntent.Project(_) => CommandFamily.Code
      case CommandIntent.RichText(_) | CommandIntent.Darlings(_) | CommandIntent.Placeholders(_) =>
        CommandFamily.Prose
      case CommandIntent.Edit(edit) => editFamily(edit)
      case CommandIntent.View(view) => viewScope(view).family
      case CommandIntent.Lifecycle(_) | CommandIntent.Diagnostics(_) | CommandIntent.File(_) |
          CommandIntent.Comments(_) | CommandIntent.Navigation(_) | CommandIntent.Theme(_) | CommandIntent.Session(_) |
          CommandIntent.Keybindings(_) | CommandIntent.UiPresets(_) | CommandIntent.Settings(_) |
          CommandIntent.Spelling(_) | CommandIntent.Scoped(_, _) =>
        CommandFamily.Core

  private def editFamily(intent: EditIntent): CommandFamily =
    intent match
      case EditIntent.FormatCurrentFile => CommandFamily.Code
      case EditIntent.FindInCurrentFile | EditIntent.FindAllInCurrentFile | EditIntent.ReplaceInCurrentFile |
          EditIntent.ReplaceAllInCurrentFile | EditIntent.Copy | EditIntent.Cut | EditIntent.Paste |
          EditIntent.ChoosePasteFromHistory | EditIntent.PasteFromHistory(_) | EditIntent.SelectAll | EditIntent.Undo |
          EditIntent.Redo =>
        CommandFamily.Core

  /** A panel command is offered where its panel is (the panel's registration); hiding one never is refused, so a panel
    * left docked in a mode it doesn't belong to can always be put away.
    */
  private def viewScope(intent: ViewIntent): CommandScope =
    intent match
      case ViewIntent.TogglePanelShown(id)       => panelScope(id)
      case ViewIntent.FocusPanel(id)             => panelScope(id)
      case ViewIntent.PlacePanel(id, Some(_), _) => panelScope(id)
      case ViewIntent.PlacePanel(_, None, _)     => core
      case ViewIntent.SetPanelPin(id, Some(_))   => panelScope(id)
      case ViewIntent.SetPanelPin(_, None)       => core
      case ViewIntent.NextTab | ViewIntent.PreviousTab | ViewIntent.SplitPaneHorizontal | ViewIntent.SplitPaneVertical |
          ViewIntent.ClosePane | ViewIntent.ToggleMaximisePanel | ViewIntent.OpenChapterNote |
          ViewIntent.OpenKeywordNote | ViewIntent.ToggleChapterGhosts | ViewIntent.ToggleNotesPin |
          ViewIntent.FocusInDirection(_) | ViewIntent.ArrangePanels | ViewIntent.OpenMarkdownPreview |
          ViewIntent.SetMarkdownViewMode(_) | ViewIntent.ToggleMarkdownReadMode | ViewIntent.SetDefaultDocumentMode(_) |
          ViewIntent.SetAppMode(_) | ViewIntent.SetAppModeStoppingProjectTask(_) |
          ViewIntent.SetShowAllSettingsRegardlessOfMode(_) | ViewIntent.ToggleShortcutsHelp | ViewIntent.ToggleTabList |
          ViewIntent.ToggleRecentFilesInMode | ViewIntent.TogglePanel(_) | ViewIntent.SetPanelSize(_, _) =>
        core

  private def panelScope(id: PanelId): CommandScope =
    val registration = PanelRegistry.registrationFor(id)
    CommandScope(registration.family, registration.frontend)
