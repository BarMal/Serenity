package com.serenity.command

import java.nio.file.Path

import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.*
import com.serenity.lsp.config.LanguageId
import com.serenity.project.ProjectTaskKind
import com.serenity.richtext.{InlineMark, ParagraphAlignment, ParagraphRole}
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.serenity.ui.layout.PanelPosition

enum CommandCategory:
  case All
  case File
  case View
  case Edit
  case Project
  case Settings

enum CommandRunnerMode:
  case Palette
  case Settings

enum PanelKind:
  case Explorer
  case Outline
  case Comments
  case Diagnostics
  case MarkdownPreview

enum CommandIntent:
  case OpenSettings
  case SaveCurrentFile
  case SaveCurrentFileAs
  case SaveConfig
  case SaveSession
  case RestoreSession
  case ClearSession
  case OpenFile
  case OpenRecentFile(path: Path)
  case OpenFileSearch
  case QuitApp
  case CloseAll
  case CloseOthers
  case NewFile
  case NextTab
  case PreviousTab
  case CloseCurrentFile
  case FindInCurrentFile
  case FindAllInCurrentFile
  case ReplaceInCurrentFile
  case ReplaceAllInCurrentFile
  case Copy
  case Cut
  case Paste
  case SelectAll
  case Undo
  case Redo
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
  case RequestLspCompletion
  case RequestLspDefinition
  case ToggleTheme
  case ReloadTheme
  case FormatCurrentFile
  case SetMaterialPreset(preset: MaterialPreset)
  case SetPostProcessingEffect(effect: PostProcessingEffect)
  case SetUiShadowsEnabled(enabled: Boolean)
  case SetMotionPreset(preset: MotionPreset)
  case SetMotionAccessibility(accessibility: MotionAccessibility)
  case SetElementTransitionSpeedScale(scale: Double)
  case SetEditorTextTransitionSpeedScale(scale: Double)
  case SetCommandRunnerTransitionSpeedScale(scale: Double)
  case SetUiTransitionSpeedScale(scale: Double)
  case SetCursorTransitionSpeedScale(scale: Double)
  case SetCommandRunnerAnimation(animation: Option[AnimationConfig])
  case SetUiAnimation(animation: Option[AnimationConfig])
  case SetCommandRunnerVisibleRows(rows: Option[Int])
  case SetCommandRunnerItemGapRows(rows: Double)
  case SetCommandRunnerCursorGapRows(rows: Option[Double])
  case SetRenderFpsTarget(target: RenderFpsTarget)
  case SetEditorInsertionTransitionKind(kind: TransitionKind)
  case SetCommandRunnerTransitionKind(kind: TransitionKind)
  case SetPanelOpenTransitionKind(kind: TransitionKind)
  case SetPanelCloseTransitionKind(kind: TransitionKind)
  case SetBackgroundStyle(style: BackgroundStyle)
  case SetBlurRadius(r: Float)
  case SetAnimationDuration(ms: Int)
  case SetAnimationSteps(n: Int)
  case ToggleLineNumbers
  case ToggleGutter
  case ToggleWordWrap
  case ToggleFocusedTextBody
  case ToggleContextualToolbar
  case SetLineNumbers(enabled: Boolean)
  case SetGutter(enabled: Boolean)
  case SetWordWrap(enabled: Boolean)
  case SetFocusedTextBody(enabled: Boolean)
  case SetContextualToolbarEnabled(enabled: Boolean)
  case SetContextualToolbarDisplayMode(mode: ToolbarDisplayMode)
  case SetCursorMode(mode: CursorMode)
  case SetCursorInfoBarMode(mode: CursorInfoBarMode)
  case SetCursorInfoBarPlacement(placement: CursorInfoBarPlacement)
  case SetUiElementGap(gap: Double)
  case SetUiCornerRadiusPx(radius: Int)
  case SetUiOutlineThicknessPx(thickness: Int)
  case OpenThemeChooser
  case OpenThemeCreator
  case ExportCurrentTheme
  case ReloadThemes
  case PinExplorerPanel
  case PinOutlinePanel
  case PinCommentsPanel
  case PinDiagnosticsPanel
  case OpenMarkdownPreview
  case SetPanelPin(kind: PanelKind, position: Option[PanelPosition])
  case SetMarkdownViewMode(mode: MarkdownViewMode)
  case SetDefaultDocumentMode(mode: DefaultDocumentMode)
  case SetSpellCheckEnabled(enabled: Boolean)
  case SetSpellCheckLanguages(languages: List[String])
  case SetSpellCheckDictionaryPaths(paths: List[String])
  case SetSpellCheckWords(words: List[String])
  case SetInterfaceDensity(density: InterfaceDensity)
  case SetWindowChromeMode(mode: WindowChromeMode)
  case SetWindowSitterEnabled(enabled: Boolean)
  case SetWindowSitterAction(action: com.serenity.animation.WindowSitterAction)
  case SetWindowSitterFrames(frames: Vector[String])
  case SetWindowSitterActiveTicks(ticks: Int)
  case SetWindowSitterFastActiveTicks(ticks: Int)
  case SetWindowSitterFastTypingThresholdMs(ms: Int)
  case SetTextAreaLeftInset(value: Double)
  case SetTextAreaRightInset(value: Double)
  case SetTextAreaTopInset(value: Double)
  case SetTextAreaBottomInset(value: Double)
  case FocusPanel(position: PanelPosition)
  case UnpinPanel(position: PanelPosition)
  case ExpandPanel(position: PanelPosition)
  case CollapseExpandedPanel
  case MovePanelEarlier(kind: PanelKind)
  case MovePanelLater(kind: PanelKind)
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
  case SaveUiPresetAsNew(name: String)
  case OverwriteUiPreset(name: String)
  case ApplyUiPreset(name: String)
  case DuplicateUiPreset(sourceName: String, targetName: String)
  case RenameUiPreset(sourceName: String, targetName: String)
  case DeleteUiPreset(name: String)
  case ResetUiPreset(name: String)
  case RunProjectTask(kind: ProjectTaskKind)
  case CancelProjectTask
  case ToggleLigatures
  case StartupNewSession
  case StartupRestoreSession
  case StartupOpenFile
  case SetBufferLanguage(language: Option[LanguageId])
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

/** A command that can be executed in the command runner */
final case class Command private (
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

final case class CommandOption(
    label: String,
    intent: CommandIntent,
    hint: Option[String] = None
)

sealed trait CommandSurfaceItem:
  def id: String
  def category: CommandCategory
  def searchText: String

object CommandSurfaceItem:

  final case class CommandItem(command: Command) extends CommandSurfaceItem:
    override def id: String                = command.name
    override def category: CommandCategory = command.category
    override lazy val searchText: String   = s"${command.name} ${command.label} ${command.description}"

  final case class OptionItem(
      id: String,
      label: String,
      options: List[CommandOption],
      selectedIndex: Int,
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override lazy val searchText: String =
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

  final case class InputItem(
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
    override lazy val searchText: String = s"$label $hint"

    def accepts(currentText: String, char: Char): Boolean =
      if acceptsFreeText then !char.isControl
      else if acceptsBindingText then char.isLetterOrDigit || char == '+' || char == '-' || char == '_'
      else char.isDigit || (char == '.' && isDecimal && !currentText.contains('.'))

    def isOutOfBounds(text: String): Boolean =
      text.nonEmpty && parse(text).isEmpty

    def withCurrentValue(v: String): InputItem = copy(currentValue = v)

  /** A direct search target for a setting leaf, independent of its rendered label. */
  final case class SettingSearchItem(
      id: String,
      targetGroupId: String,
      targetItemId: String,
      label: String,
      breadcrumb: String,
      effectiveValue: Option[String],
      sourceScope: String,
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override lazy val searchText: String =
      s"$label ${effectiveValue.getOrElse("")} $sourceScope $breadcrumb ${hint.getOrElse("")}".trim

  final case class GroupItem(
      id: String,
      label: String,
      children: List[CommandSurfaceItem],
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override lazy val searchText: String =
      s"$label ${children.map(_.searchText).mkString(" ")}"

/** Search result for a command with relevance scoring */
final case class CommandSearchResult(
    command: Command,
    relevance: Double
):
  def name: String        = command.name
  def description: String = command.description

/** Functional command searcher that filters and ranks commands */
class CommandSearcher(commands: List[Command]):

  /** Search commands by name and description, returning top results */
  def search(term: String, maxResults: Int = 5): List[Command] =
    if term.trim.isEmpty then commands.take(maxResults)
    else
      val tokens = CommandSearcher.tokens(term)

      commands.zipWithIndex
        .flatMap {
          case (command, index) =>
            calculateRelevance(command, tokens).map(relevance => (command, relevance, index))
        }
        .sortBy { case (_, relevance, index) => (-relevance, index) }
        .take(maxResults)
        .map(_._1)

  /** Calculate relevance only when every token has a metadata match. */
  private def calculateRelevance(command: Command, tokens: List[String]): Option[Double] =
    val nameTokens        = CommandSearcher.tokens(command.name)
    val labelTokens       = CommandSearcher.tokens(command.label)
    val descriptionTokens = CommandSearcher.tokens(command.description)

    tokens.foldLeft(Option(0.0)) { (score, token) =>
      score.flatMap { total =>
        CommandSearcher.tokenRelevance(token, nameTokens, labelTokens, descriptionTokens).map(total + _)
      }
    }

object CommandSearcher:
  private def tokens(value: String): List[String] =
    Option(value).toList.flatMap(_.toLowerCase.split("[^\\p{Alnum}]+")).filter(_.nonEmpty)

  private def tokenRelevance(
    token: String,
    nameTokens: List[String],
    labelTokens: List[String],
    descriptionTokens: List[String]
  ): Option[Double] =
    val fields = List(nameTokens -> 100.0, labelTokens -> 95.0, descriptionTokens -> 40.0)
    fields.collectFirst {
      case (fieldTokens, exactScore) if fieldTokens.contains(token)              => exactScore
      case (fieldTokens, prefixScore) if fieldTokens.exists(_.startsWith(token)) => prefixScore - 20.0
      case (fieldTokens, containsScore) if fieldTokens.exists(_.contains(token)) => containsScore - 40.0
    }
