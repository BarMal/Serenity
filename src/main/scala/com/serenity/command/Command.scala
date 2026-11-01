package com.serenity.command

import com.serenity.config.*
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.Direction
import com.serenity.project.ProjectTaskKind
import com.serenity.richtext.{InlineMark, ParagraphAlignment, ParagraphRole}
import com.serenity.session.SessionId
import com.serenity.state.models.{
  BufferId,
  ClipboardEntry,
  CloseWorkflowChoice,
  CommentId,
  PanelId,
  RestartMode,
  SurfaceId
}
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.serenity.ui.layout.PanelPosition

enum CommandCategory:
  case All
  case File
  case View
  case Edit
  case Project
  case Settings

enum LifecycleIntent:
  case QuitApp
  // Quits like QuitApp (unsaved work is still asked about), then starts again; see SafeModeCommands.
  case Restart(mode: RestartMode)
  // What the "save changes before closing?" prompt's answers run; see CloseCommands.
  case ResolveClose(choice: CloseWorkflowChoice)

enum EditIntent:
  case FindInCurrentFile
  case FindAllInCurrentFile
  case ReplaceInCurrentFile
  case ReplaceAllInCurrentFile
  case Copy
  case Cut
  case Paste
  case ChoosePasteFromHistory
  case PasteFromHistory(entry: ClipboardEntry)
  case SelectAll
  case SelectNextOccurrence
  case SkipOccurrence
  case SelectAllOccurrences
  case Undo
  case Redo
  case ToggleLineComment
  case ToggleBlockComment
  case FormatCurrentFile

enum RichTextIntent:
  case ToggleRichTextMark(mark: InlineMark)
  case SetRichTextFontFamily(family: String)
  case SetRichTextFontSize(size: Float)
  case AdjustRichTextFontSize(deltaPt: Float)
  case SetRichTextColor(color: String)
  case SetRichTextParagraphRole(role: ParagraphRole)
  case SetRichTextParagraphAlignment(alignment: ParagraphAlignment)
  case ConvertToRichText(andThen: Option[RichTextIntent])

enum CommentsIntent:
  case ToggleCommentLens
  case AddDocumentComment(text: String)
  case DeleteDocumentComment
  case ReplyToDocumentComment(text: String)

  /** What the comment lens writes when its draft is saved: an empty `text` deletes the comment. */
  case SaveCommentDraft(id: CommentId, text: String)
  case ResolveDocumentComment
  case ReopenDocumentComment
  case ToggleResolvedComments
  case NextDocumentComment
  case PreviousDocumentComment

enum NavigationIntent:
  case OpenGotoLine
  case ToggleBookmark
  case NextBookmark
  case PreviousBookmark
  case NextDocumentSymbol
  case PreviousDocumentSymbol
  case NavigateBack
  case NavigateForward
  case GoToBufferLine(bufferId: BufferId, line: Int)

enum LspIntent:
  case RequestLspHover
  case RequestLspCompletion
  case RequestLspDefinition
  case RequestLspReferences
  // No parameter -- like `RequestLspDefinition`, the target symbol is resolved from the cursor position at interpret
  // time. Opens the rename-symbol `TextPrompt` for the new name; the rename itself is requested on that modal's
  // submit, not here (#1467).
  case OpenRenameSymbolPrompt

enum ThemeIntent:
  case ToggleTheme
  case ApplyTheme(name: String)
  case SetFollowSystem(enabled: Boolean)
  case SetFollowSystemTheme(slot: AppearanceSlot, name: String)
  case ReloadTheme
  case OpenThemeChooser
  case OpenThemeCreator
  case ExportCurrentTheme
  case ReloadThemes

enum ViewIntent:
  case NextTab
  case PreviousTab
  case SplitPaneHorizontal
  case SplitPaneVertical
  case ClosePane

  /** Shows a hidden panel at its default edge, or hides a shown one wherever it is docked. */
  case TogglePanelShown(id: PanelId)

  /** Focuses a panel, showing it at its default edge first if it is hidden. */
  case FocusPanel(id: PanelId)

  /** Maximises the focused panel into the workspace, or restores the maximised one. */
  case ToggleMaximisePanel

  /** Shows the note for the chapter the cursor is in beside the manuscript, creating it the first time. */
  case OpenChapterNote, OpenKeywordNote

  /** Shows or hides the faded overview under empty chapters; the notes themselves are untouched. */
  case ToggleChapterGhosts

  /** Pins the notes pane to the note it shows, or lets it follow the cursor's chapter again. */
  case ToggleNotesPin
  case ToggleTermHighlights

  /** Moves focus to the nearest editor pane or docked panel in `direction`. */
  case FocusInDirection(direction: Direction)

  /** Docks a panel `index` panels in from the start of `position`'s edge (moving it there if it is elsewhere), or hides
    * it when `position` is `None`.
    */
  case PlacePanel(id: PanelId, position: Option[PanelPosition], index: Int)

  /** Opens the Arrange Panels list. */
  case ArrangePanels
  case SetPanelPin(id: PanelId, position: Option[PanelPosition])
  case OpenMarkdownPreview
  case SetMarkdownViewMode(mode: MarkdownViewMode)

  /** Switches between hiding Markdown markers everywhere (read) and hiding them off the caret line (live preview). */
  case ToggleMarkdownReadMode
  case SetDefaultDocumentMode(mode: DefaultDocumentMode)
  case SetAppMode(mode: AppMode)

  /** [[SetAppMode]] once the writer has agreed to the project task it would stop: the answer to the prompt that asks.
    */
  case SetAppModeStoppingProjectTask(mode: AppMode)
  case SetShowAllSettingsRegardlessOfMode(value: Boolean)
  case ToggleShortcutsHelp
  case ToggleTabList
  case ToggleRecentFilesInMode
  // issue #1310: the panel/placement framework's mode-1 (command palette) and mode-4 (docked, resizable) entry
  // points -- parametric over PanelId/SurfaceId instead of one case per registered panel, mirroring how
  // SetPanelPin above already generalizes several one-off pin commands into one parametric case.
  case TogglePanel(id: PanelId)
  case SetPanelSize(surfaceId: SurfaceId, delta: Int)

enum ProjectIntent:
  case RunProjectTask(kind: ProjectTaskKind)
  case CancelProjectTask

enum SessionIntent:
  case SaveSession
  case RestoreSession
  case ClearSession
  case StartupNewSession
  case StartupRestoreSession
  case StartupOpenFile
  case StartupOpenFolder
  case StartupOpenFileOrFolder
  case ReturnToStartPage
  // Named sessions (issue #1390): `SaveSession`/`RestoreSession`/`ClearSession` above all operate implicitly on the
  // "current" session (`SessionManager`'s own notion of the one on `SessionIndex.currentSessionId`) -- these three
  // instead reach the named-session methods (`saveSessionAs`/`listSessions`/`renameSession`) that had no command
  // surface at all before this: opening the save-as/open/rename modals, which the modal's own Enter then completes.
  case OpenSaveSessionAsPrompt
  case OpenSessionPicker
  case OpenRenameSessionPicker
  // What the session pickers' entries run; see SessionCommands.
  case OpenNamedSession(sessionId: SessionId)
  case RenameNamedSession(sessionId: SessionId, currentName: String)

enum KeybindingsIntent:
  case SetGlobalHotkey(action: HotkeyAction, binding: String)
  case ResolveGlobalHotkeyConflict(action: HotkeyAction, binding: String)
  case ResolveFocusedKeymapConflict(itemId: String, binding: String)
  case SetEditorKeyBinding(action: EditorKeyAction, binding: String)
  case SetCommandRunnerKeyBinding(action: CommandRunnerKeyAction, binding: String)
  case SetModalKeyBinding(action: ModalKeyAction, binding: String)
  case SetPanelKeyBinding(action: PanelKeyAction, binding: String)
  case SetPeekKeyBinding(action: PeekKeyAction, binding: String)
  case ResetGlobalHotkey(action: HotkeyAction)
  case ResetEditorKeyBinding(action: EditorKeyAction)
  case ResetCommandRunnerKeyBinding(action: CommandRunnerKeyAction)
  case ResetModalKeyBinding(action: ModalKeyAction)
  case ResetPanelKeyBinding(action: PanelKeyAction)
  case ResetPeekKeyBinding(action: PeekKeyAction)

enum UiPresetsIntent:
  case SaveUiPresetAsNew(name: String)
  case OverwriteUiPreset(name: String)
  case ApplyUiPreset(name: String)
  case DuplicateUiPreset(sourceName: String, targetName: String)
  case RenameUiPreset(sourceName: String, targetName: String)
  case DeleteUiPreset(name: String)
  case ResetUiPreset(name: String)
  case SetUiPresetTheme(name: String, themeName: String)
  case UseCurrentThemeForUiPreset(name: String)
  case ClearUiPresetTheme(name: String)
  // `ApplyUiPreset` (the splash's workflow shortcuts, the top-level searchable "Apply <Name> Preset" commands) stays
  // a one-shot, apply-everything action -- ReviewUiPreset is the deliberate alternative reached from a preset's own
  // settings group ("Apply Preset" there), which opens the diff-toggle review instead of applying immediately.
  case ReviewUiPreset(name: String)
  // `selectedKeys` are `PresetChange.key`s (see `UiPresetDiff`) still checked when the review was submitted --
  // baked in at that point rather than re-read from runner state afterward.
  case ConfirmUiPresetDiffApply(name: String, selectedKeys: List[String])

/** Font family/size and ligature settings. Mirrors the `AppConfig`/`FontLoader.FontConfig` domain split. */
enum FontIntent:
  case IncreaseFontSize
  case DecreaseFontSize
  case SetFontSize(size: Float)
  case SetCodeFontSize(size: Float)
  case SetTextFontSize(size: Float)
  case SetUiFontSize(size: Float)
  case SetTextScaleMode(mode: TextScaleMode)
  case SetTextScaleMultiplier(scale: Double)
  case SetCodeFontFamily(family: String)
  case SetTextFontFamily(family: String)
  case SetUiFontFamily(family: String)
  case SetLigatures(enabled: Boolean)
  case SetCodeLigatures(enabled: Boolean)
  case SetTextLigatures(enabled: Boolean)
  case SetUiLigatures(enabled: Boolean)
  case ToggleLigatures

/** Cursor rendering mode and its info-bar presentation. */
enum CursorIntent:
  case SetCursorMode(mode: CursorMode)

/** The status line: which segments it shows, in what order, and where it lives. */
enum StatusLineIntent:
  case SetSegmentIncluded(segment: StatusSegment, included: Boolean)
  case MoveSegmentEarlier(segment: StatusSegment)
  case MoveSegmentLater(segment: StatusSegment)
  case SetPlacement(placement: StatusLinePlacement)

  /** Off when shown (either placement), back to pinned when off. */
  case ToggleVisibility

  /** Sets or clears (`None`) the active document's word-count goal the `WordGoal` segment shows progress against. */
  case SetWordGoal(goal: Option[Int])

/** What the editor surface itself shows around and inside the text: chrome rows, wrap, scrolling, insets. */
enum TextDisplayIntent:
  case ToggleLineNumbers
  case ToggleWordWrap
  case ToggleFocusedTextBody
  case ToggleContextualToolbar
  case TogglePaneHeaders
  case ToggleVisualLineCursorNavigation
  case ToggleTypewriterScrolling
  case ToggleColumnMode
  case SetLineNumbers(enabled: Boolean)
  case SetLineNumberSide(side: LineNumberSide)
  case SetLineNumberMarginLeft(cells: Int)
  case SetLineNumberMarginRight(cells: Int)
  case SetLineNumberPadding(cells: Int)
  case SetWordWrap(enabled: Boolean)
  case SetVisualLineCursorNavigation(enabled: Boolean)
  case SetTypewriterScrolling(enabled: Boolean)
  case SetColumnMode(enabled: Boolean)
  case SetColumnTargetWidth(cells: Int)
  case SetColumnGap(cells: Int)
  case SetColumnCount(count: Option[Int])
  case SetFocusedTextBody(enabled: Boolean)
  case SetContextualToolbarEnabled(enabled: Boolean)
  case SetContextualToolbarDisplayMode(mode: ToolbarDisplayMode)
  case SetTextAreaLeftInset(value: Double)
  case SetTextAreaRightInset(value: Double)
  case SetTextAreaTopInset(value: Double)
  case SetTextAreaBottomInset(value: Double)
  case SetDropCapsEnabled(enabled: Boolean)
  case SetTabSize(size: Int)
  case SetInsertSpaces(enabled: Boolean)

/** How interface surfaces are sized and spaced -- density, gaps, outlines, window chrome, key hints -- and where Escape
  * from a focused panel returns focus.
  */
enum InterfaceChromeIntent:
  case SetCommandRunnerShowKeyHints(enabled: Boolean)
  case SetUiElementGap(gap: Double)
  case SetUiOutlineThicknessPx(thickness: Int)
  case SetUiCornerRadiusPx(radius: Int)
  case SetTabMaxWidth(width: Int)
  case SetDockedCornerStyle(style: DockedCornerStyle)
  case SetPaneHeaderCorners(corners: PaneHeaderCorners)
  case SetInterfaceDensity(density: InterfaceDensity)
  case SetWindowChromeMode(mode: WindowChromeMode)
  case SetMenuBarMode(mode: MenuBarMode)
  case SetUiLayout(layout: UiLayout)
  case ToggleUiLayout
  case SetSummonModifiers(modifiers: Set[Modifier])
  case SetSummonLock(lock: HotkeyTrigger)
  case SetSummonLetter(element: ChromeElement, letter: Option[Char])
  // `useLayoutKeys` are the `LayoutPin.key`s handed back to the layout; every other key the file pins stays as the
  // file has it. Reached from the prompt a switch (or the first launch) opens, never from the palette.
  case ReviewUiLayoutOverride(layout: UiLayout)
  case ConfirmUiLayoutOverride(layout: UiLayout, useLayoutKeys: List[String])
  case SetWheelScrollLines(lines: Int)
  case SetPanelEscapeTarget(mode: AppMode, target: PanelEscapeTarget)

enum SpellCheckIntent:
  case SetSpellCheckEnabled(enabled: Boolean)
  case SetSpellCheckLanguages(languages: List[String])
  case SetSpellCheckDictionaryPaths(paths: List[String])
  case SetSpellCheckWords(words: List[String])
  // #1531: no parameter -- like `LspIntent.RequestLspDefinition`, the word is resolved from the cursor position at
  // interpret time (here, the word covered by the active spell-check diagnostic under the cursor) rather than
  // threaded through the command.
  case AddWordAtCursorToDictionary

/** What the writer does with a misspelling (#1939). The menu and popup act on a word they have already resolved, so
  * those cases carry it rather than looking at the cursor, which a right-click leaves where it was.
  */
enum SpellingIntent:
  case ShowSuggestions
  case Replace(line: Int, start: Int, end: Int, misspelled: String, replacement: String)
  case AddToDictionary(word: String)
  case IgnoreOnce(line: Int, start: Int, end: Int, word: String)
  case IgnoreEverywhere(word: String)
  case IgnoreOnceAtCursor
  case IgnoreEverywhereAtCursor

/** Settings with no more specific home: render tuning and the settings surface's own open/save commands. */
enum GeneralSettingsIntent:
  case OpenSettings
  case SaveConfig
  // Moves config.conf aside to a timestamped backup, then restores every setting to its default.
  case ResetSettings
  case SetRenderFpsTarget(target: RenderFpsTarget)
  case SetRenderDamageGranularity(granularity: RenderDamageGranularity)
  case SetRenderBackend(backend: RenderBackendSetting)
  case SetRenderWayland(mode: WaylandRender)
  case SetCommandRunnerVisibleRows(rows: Option[Int])
  case SetCommandRunnerItemGapRows(rows: Option[Double])
  case SetCommandRunnerCursorGapRows(rows: Option[Double])
  case SetAutoSaveMode(mode: AutoSaveMode)
  case SetAutoSaveDelayMillis(millis: Long)

/** The `Settings` family of [[CommandIntent]], split one level deeper than the other groups because it is by far the
  * largest (~85 cases) — mirrors the domain split already established on `AppConfig`.
  */
enum SettingsIntent:
  case Font(intent: FontIntent)
  case Cursor(intent: CursorIntent)
  case StatusLine(intent: StatusLineIntent)
  case TextDisplay(intent: TextDisplayIntent)
  case Headings(intent: HeadingsIntent)
  case InterfaceChrome(intent: InterfaceChromeIntent)
  case SpellCheck(intent: SpellCheckIntent)
  case General(intent: GeneralSettingsIntent)

/** What a settings edit changes: the live config, or one stored UI preset. */
enum SettingsTarget:
  case Global
  case Preset(name: String)

/** Every command the command runner can execute, grouped into per-family sub-enums so that both `interpretCommand` and
  * this type stay exhaustiveness-checked one family at a time instead of as one 170-case flat match.
  */
enum CommandIntent:
  case Lifecycle(intent: LifecycleIntent)
  case Diagnostics(intent: DiagnosticsIntent)
  case File(intent: FileIntent)
  case Edit(intent: EditIntent)
  case RichText(intent: RichTextIntent)
  case Comments(intent: CommentsIntent)
  case Placeholders(intent: PlaceholderIntent)
  case Darlings(intent: DarlingIntent)
  case Spelling(intent: SpellingIntent)
  case Navigation(intent: NavigationIntent)
  case Lsp(intent: LspIntent)
  case Theme(intent: ThemeIntent)
  case View(intent: ViewIntent)
  case Project(intent: ProjectIntent)
  case Session(intent: SessionIntent)
  case Keybindings(intent: KeybindingsIntent)
  case UiPresets(intent: UiPresetsIntent)
  case Settings(intent: SettingsIntent)
  // An edit aimed at `target`; `Global` is the edit itself, a `Preset` change goes to that stored preset alone.
  case Scoped(target: SettingsTarget, intent: CommandIntent)

/** A command that can be executed in the command runner */
final case class Command private (
    name: String,
    label: String,
    description: String,
    intent: CommandIntent,
    category: CommandCategory,
    // #1298: when true, submitting this command from a settings submenu leaves the submenu open at its current
    // position instead of closing the whole command-runner overlay (mirrors how an `OptionItem` cycle already stays
    // open) -- for a command meant to be pressed repeatedly in a row, such as the info bar segment movers.
    keepMenuOpenOnSubmit: Boolean
)

object Command:

  def typed(
    name: String,
    description: String,
    intent: CommandIntent,
    category: CommandCategory = CommandCategory.Edit,
    label: String = "",
    keepMenuOpenOnSubmit: Boolean = false
  ): Command =
    Command(
      name,
      Option(label).filter(_.nonEmpty).getOrElse(Command.defaultLabel(name)),
      description,
      intent,
      category,
      keepMenuOpenOnSubmit
    )

  private def defaultLabel(name: String): String =
    name
      .split("[-_ ]+")
      .toList
      .filter(_.nonEmpty)
      .map(word => s"${word.head.toUpper}${word.drop(1)}")
      .mkString(" ")

final case class CommandOption(
    label: String,
    intent: CommandIntent,
    hint: Option[String] = None
)
