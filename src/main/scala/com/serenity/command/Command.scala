package com.serenity.command

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import com.serenity.lsp.config.LanguageId
import com.serenity.project.ProjectTaskKind
import com.serenity.richtext.{InlineMark, ParagraphAlignment, ParagraphRole}
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.serenity.ui.layout.PanelPosition

enum AnimationMode:
  case None
  case Quick
  case Smooth
  case Subtle

enum CommandCategory:
  case All
  case File
  case View
  case Edit
  case Project
  case Settings

enum CommandIntent:
  case SaveCurrentFile
  case SaveCurrentFileAs
  case SaveConfig
  case SaveSession
  case RestoreSession
  case ClearSession
  case OpenFile
  case QuitApp
  case CloseAll
  case CloseOthers
  case NewFile
  case CloseCurrentFile
  case FindInCurrentFile
  case FindAllInCurrentFile
  case ReplaceInCurrentFile
  case ReplaceAllInCurrentFile
  case Copy
  case Cut
  case Paste
  case SelectAll
  case ToggleRichTextMark(mark: InlineMark)
  case SetRichTextFontFamily(family: String)
  case SetRichTextFontSize(size: Float)
  case SetRichTextColor(color: String)
  case SetRichTextParagraphRole(role: ParagraphRole)
  case SetRichTextParagraphAlignment(alignment: ParagraphAlignment)
  case ToggleCommentLens
  case AddDocumentComment(text: String)
  case DeleteDocumentComment
  case NextDocumentComment
  case PreviousDocumentComment
  case OpenGotoLine
  case ToggleBookmark
  case NextBookmark
  case PreviousBookmark
  case NextDocumentSymbol
  case PreviousDocumentSymbol
  case NavigateBack
  case NavigateForward
  case RequestLspHover
  case RequestLspDefinition
  case ToggleTheme
  case ReloadTheme
  case FormatCurrentFile
  case SetAnimationMode(mode: AnimationMode)
  case SetMaterialPreset(preset: MaterialPreset)
  case SetMotionPreset(preset: MotionPreset)
  case SetElementTransitionSpeedScale(scale: Double)
  case SetEditorInsertionTransitionKind(kind: TransitionKind)
  case SetBackgroundStyle(style: BackgroundStyle)
  case SetBlurRadius(r: Float)
  case SetAnimationDuration(ms: Int)
  case SetAnimationSteps(n: Int)
  case ToggleLineNumbers
  case ToggleGutter
  case SetCursorMode(mode: CursorMode)
  case SetCursorInfoBarMode(mode: CursorInfoBarMode)
  case SetCursorInfoBarPlacement(placement: CursorInfoBarPlacement)
  case SetUiElementGap(gap: Int)
  case SetUiCornerRadiusPx(radius: Int)
  case OpenThemeChooser
  case ReloadThemes
  case PinExplorerPanel
  case PinOutlinePanel
  case PinDiagnosticsPanel
  case OpenMarkdownPreview
  case SetMarkdownViewMode(mode: MarkdownViewMode)
  case SetDefaultDocumentMode(mode: DefaultDocumentMode)
  case SetSpellCheckEnabled(enabled: Boolean)
  case SetSpellCheckLanguages(languages: List[String])
  case SetSpellCheckWords(words: List[String])
  case SetInterfaceDensity(density: InterfaceDensity)
  case SetTextAreaLeftInset(value: Double)
  case SetTextAreaRightInset(value: Double)
  case FocusPanel(position: PanelPosition)
  case UnpinPanel(position: PanelPosition)
  case ExpandPanel(position: PanelPosition)
  case CollapseExpandedPanel
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
  case SaveUiPreset(name: String)
  case ApplyUiPreset(name: String)
  case DuplicateUiPreset(sourceName: String, targetName: String)
  case RenameUiPreset(sourceName: String, targetName: String)
  case DeleteUiPreset(name: String)
  case ResetUiPreset(name: String)
  case RunProjectTask(kind: ProjectTaskKind)
  case ToggleLigatures
  case StartupNewSession
  case StartupRestoreSession
  case StartupOpenFile
  case SetBufferLanguage(language: Option[LanguageId])
  case SetGlobalHotkey(action: HotkeyAction, binding: String)
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

/** A command that can be executed in the command runner */
case class Command private (
    name: String,
    label: String,
    description: String,
    intent: CommandIntent,
    category: CommandCategory = CommandCategory.Edit
)

object Command:

  def typed(
    name: String,
    description: String,
    intent: CommandIntent,
    category: CommandCategory = CommandCategory.Edit,
    label: String = ""
  ): Command =
    Command(name, Option(label).filter(_.nonEmpty).getOrElse(Command.defaultLabel(name)), description, intent, category)

  private def defaultLabel(name: String): String =
    name
      .split("[-_ ]+")
      .toList
      .filter(_.nonEmpty)
      .map(word => word.head.toUpper + word.drop(1))
      .mkString(" ")

case class CommandOption(
    label: String,
    intent: CommandIntent,
    hint: Option[String] = None
)

sealed trait CommandSurfaceItem:
  def id: String
  def category: CommandCategory
  def searchText: String

object CommandSurfaceItem:

  case class CommandItem(command: Command) extends CommandSurfaceItem:
    override def id: String                = command.name
    override def category: CommandCategory = command.category
    override def searchText: String        = s"${command.name} ${command.label} ${command.description}"

  case class OptionItem(
      id: String,
      label: String,
      options: List[CommandOption],
      selectedIndex: Int,
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override def searchText: String =
      s"$label ${options.map(option => s"${option.label} ${option.hint.getOrElse("")}").mkString(" ")}"

    def selectedOption: String =
      options.lift(selectedIndex).map(_.label).getOrElse("")

    def selectedHint: Option[String] =
      options.lift(selectedIndex).flatMap(_.hint).orElse(hint)

    def selectedIntent: Option[CommandIntent] =
      options.lift(selectedIndex).map(_.intent)

    def moveSelection(delta: Int): OptionItem =
      if options.isEmpty then this
      else
        val rawIndex     = (selectedIndex + delta) % options.length
        val wrappedIndex = if rawIndex < 0 then options.length + rawIndex else rawIndex
        copy(selectedIndex = wrappedIndex)

  case class InputItem(
      id: String,
      label: String,
      hint: String,
      currentValue: String,
      isDecimal: Boolean,
      parse: String => Option[CommandIntent],
      category: CommandCategory,
      acceptsBindingText: Boolean = false,
      acceptsFreeText: Boolean = false
  ) extends CommandSurfaceItem:
    override def searchText: String = s"$label $hint"

    def accepts(currentText: String, char: Char): Boolean =
      if acceptsFreeText then !char.isControl
      else if acceptsBindingText then char.isLetterOrDigit || char == '+' || char == '-' || char == '_'
      else char.isDigit || (char == '.' && isDecimal && !currentText.contains('.'))

    def isOutOfBounds(text: String): Boolean =
      text.nonEmpty && parse(text).isEmpty

    def withCurrentValue(v: String): InputItem = copy(currentValue = v)

  case class GroupItem(
      id: String,
      label: String,
      children: List[CommandSurfaceItem],
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override def searchText: String =
      s"$label ${children.map(_.searchText).mkString(" ")}"

/** Search result for a command with relevance scoring */
case class CommandSearchResult(
    command: Command,
    relevance: Double
):
  def name: String        = command.name
  def description: String = command.description

/** Functional command searcher that filters and ranks commands */
class CommandSearcher(commands: List[Command]):

  /** Search commands by name and description, returning top results */
  def search(term: String, maxResults: Int = 5): List[Command] =
    if term.isEmpty then commands.take(maxResults)
    else
      val lowercaseTerm = term.toLowerCase

      commands
        .map(cmd => CommandSearchResult(cmd, calculateRelevance(cmd, lowercaseTerm)))
        .filter(_.relevance > 0)
        .sortBy(-_.relevance)
        .take(maxResults)
        .map(_.command)

  /** Calculate relevance score for a command based on search term */
  private def calculateRelevance(command: Command, term: String): Double =
    val nameLower  = command.name.toLowerCase
    val labelLower = command.label.toLowerCase
    val descLower  = command.description.toLowerCase

    if nameLower == term then 100.0
    else if labelLower == term then 95.0
    else if nameLower.startsWith(term) then 80.0
    else if labelLower.startsWith(term) then 75.0
    else if nameLower.contains(term) then 60.0
    else if labelLower.contains(term) then 55.0
    else if descLower.contains(term) then 40.0
    else 0.0
