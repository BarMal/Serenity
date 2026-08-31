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

enum PanelKind:
  case Explorer
  case Outline
  case Comments
  case Diagnostics
  case MarkdownPreview

enum LifecycleIntent:
  case QuitApp

enum FileIntent:
  case SaveCurrentFile
  case SaveCurrentFileAs
  case OpenFile
  case OpenRecentFile(path: Path)
  case OpenFileSearch
  case CloseAll
  case CloseOthers
  case CloseCurrentFile
  case NewFile
  case SetBufferLanguage(language: Option[LanguageId])

enum EditIntent:
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
  case FormatCurrentFile

enum RichTextIntent:
  case ToggleRichTextMark(mark: InlineMark)
  case SetRichTextFontFamily(family: String)
  case SetRichTextFontSize(size: Float)
  case SetRichTextColor(color: String)
  case SetRichTextParagraphRole(role: ParagraphRole)
  case SetRichTextParagraphAlignment(alignment: ParagraphAlignment)

enum CommentsIntent:
  case ToggleCommentLens
  case AddDocumentComment(text: String)
  case DeleteDocumentComment
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

enum LspIntent:
  case RequestLspHover
  case RequestLspCompletion
  case RequestLspDefinition

enum ThemeIntent:
  case ToggleTheme
  case ReloadTheme
  case OpenThemeChooser
  case OpenThemeCreator
  case ExportCurrentTheme
  case ReloadThemes

enum ViewIntent:
  case NextTab
  case PreviousTab
  case FocusPanel(position: PanelPosition)
  case UnpinPanel(position: PanelPosition)
  case ExpandPanel(position: PanelPosition)
  case CollapseExpandedPanel
  case MovePanelEarlier(kind: PanelKind)
  case MovePanelLater(kind: PanelKind)
  case PinExplorerPanel
  case PinOutlinePanel
  case PinCommentsPanel
  case PinDiagnosticsPanel
  case SetPanelPin(kind: PanelKind, position: Option[PanelPosition])
  case OpenMarkdownPreview
  case SetMarkdownViewMode(mode: MarkdownViewMode)
  case SetDefaultDocumentMode(mode: DefaultDocumentMode)
  case ToggleShortcutsHelp

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

/** Motion presets, transition speeds/kinds, and command-runner animation tuning. */
enum MotionIntent:
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
  case SetEditorInsertionTransitionKind(kind: TransitionKind)
  case SetCommandRunnerTransitionKind(kind: TransitionKind)
  case SetPanelOpenTransitionKind(kind: TransitionKind)
  case SetPanelCloseTransitionKind(kind: TransitionKind)

/** Cursor rendering mode and its info-bar presentation. */
enum CursorIntent:
  case SetCursorMode(mode: CursorMode)
  case SetCursorInfoBarMode(mode: CursorInfoBarMode)
  case SetCursorInfoBarPlacement(placement: CursorInfoBarPlacement)

/** Panel/text-area chrome: line numbers, gutter, word wrap, toolbar, spacing, window chrome and sitter, insets. */
enum PanelChromeIntent:
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
  case SetCommandRunnerShowKeyHints(enabled: Boolean)
  case SetUiElementGap(gap: Double)
  case SetUiCornerRadiusPx(radius: Int)
  case SetUiOutlineThicknessPx(thickness: Int)
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

enum SpellCheckIntent:
  case SetSpellCheckEnabled(enabled: Boolean)
  case SetSpellCheckLanguages(languages: List[String])
  case SetSpellCheckDictionaryPaths(paths: List[String])
  case SetSpellCheckWords(words: List[String])

/** Settings with no more specific home: material/post-processing/shadows, render tuning, background, and the settings
  * surface's own open/save commands.
  */
enum GeneralSettingsIntent:
  case OpenSettings
  case SaveConfig
  case SetMaterialPreset(preset: MaterialPreset)
  case SetPostProcessingEffect(effect: PostProcessingEffect)
  case SetUiShadowsEnabled(enabled: Boolean)
  case SetRenderFpsTarget(target: RenderFpsTarget)
  case SetRenderDamageGranularity(granularity: RenderDamageGranularity)
  case SetBackgroundStyle(style: BackgroundStyle)
  case SetBlurRadius(r: Float)
  case SetAnimationDuration(ms: Int)
  case SetAnimationSteps(n: Int)

/** The `Settings` family of [[CommandIntent]], split one level deeper than the other groups because it is by far the
  * largest (~85 cases) — mirrors the domain split already established on `AppConfig`.
  */
enum SettingsIntent:
  case Font(intent: FontIntent)
  case Motion(intent: MotionIntent)
  case Cursor(intent: CursorIntent)
  case PanelChrome(intent: PanelChromeIntent)
  case SpellCheck(intent: SpellCheckIntent)
  case General(intent: GeneralSettingsIntent)

/** Every command the command runner can execute, grouped into per-family sub-enums so that both `interpretCommand` and
  * this type stay exhaustiveness-checked one family at a time instead of as one 170-case flat match.
  */
enum CommandIntent:
  case Lifecycle(intent: LifecycleIntent)
  case File(intent: FileIntent)
  case Edit(intent: EditIntent)
  case RichText(intent: RichTextIntent)
  case Comments(intent: CommentsIntent)
  case Navigation(intent: NavigationIntent)
  case Lsp(intent: LspIntent)
  case Theme(intent: ThemeIntent)
  case View(intent: ViewIntent)
  case Project(intent: ProjectIntent)
  case Session(intent: SessionIntent)
  case Keybindings(intent: KeybindingsIntent)
  case UiPresets(intent: UiPresetsIntent)
  case Settings(intent: SettingsIntent)

/** A command that can be executed in the command runner */
final case class Command private (
    name: String,
    label: String,
    description: String,
    intent: CommandIntent,
    category: CommandCategory
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
      .map(word => s"${word.head.toUpper}${word.drop(1)}")
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
