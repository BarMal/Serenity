package com.serenity.state.models

import com.serenity.command.*
import com.serenity.config.{MarkdownViewMode, ToolbarDisplayMode}
import com.serenity.project.ProjectTaskKind
import com.serenity.richtext.*
import com.serenity.ui.fonts.FontLoader

enum ContextualToolbarItem:
  def id: String
  def label: String
  def icon: String

  case Button(
      id: String,
      label: String,
      intent: CommandIntent,
      icon: String,
      selected: Boolean = false
  )

  case Dropdown(
      id: String,
      label: String,
      icon: String,
      optionItem: CommandSurfaceItem.OptionItem
  )

  case Input(
      id: String,
      label: String,
      icon: String,
      inputItem: CommandSurfaceItem.InputItem
  )

enum ContextualToolbarDetailState:
  case Dropdown(itemId: String, selectedIndex: Int)
  case Input(itemId: String, text: String = "")

enum ContextualToolbarHit:
  case TopLevelItem(index: Int)
  case DropdownOption(itemId: String, optionIndex: Int)
  case InputDetail(itemId: String)

final case class ContextualToolbarState(
    focusedIndex: Int = 0,
    displayMode: ToolbarDisplayMode = ToolbarDisplayMode.IconAndText,
    detailState: Option[ContextualToolbarDetailState] = None
):

  def focusedItem(items: List[ContextualToolbarItem]): Option[ContextualToolbarItem] =
    items.lift(normalizedFocusedIndex(items))

  def moveFocus(delta: Int, items: List[ContextualToolbarItem]): ContextualToolbarState =
    if items.isEmpty then copy(focusedIndex = 0, detailState = None)
    else
      val raw = (normalizedFocusedIndex(items) + delta) % items.length
      copy(
        focusedIndex = if raw < 0 then raw + items.length else raw,
        detailState = None
      )

  def withFocusedIndex(index: Int, items: List[ContextualToolbarItem]): ContextualToolbarState =
    copy(focusedIndex = clampIndex(index, items))

  /** Sets the focused item to a geometry-derived index (e.g. from a vertical row move) and closes any open detail. */
  def withFocusedIndexClearingDetail(index: Int, items: List[ContextualToolbarItem]): ContextualToolbarState =
    copy(focusedIndex = clampIndex(index, items), detailState = None)

  /** Sets the selected option of an already-open dropdown detail to a geometry-derived index. */
  def withDetailSelectionIndex(itemId: String, index: Int): ContextualToolbarState =
    copy(detailState = Some(ContextualToolbarDetailState.Dropdown(itemId, index)))

  def openFocusedDetail(items: List[ContextualToolbarItem]): ContextualToolbarState =
    focusedItem(items) match
      case Some(item: ContextualToolbarItem.Dropdown) =>
        copy(detailState = Some(ContextualToolbarDetailState.Dropdown(item.id, item.optionItem.selectedIndex)))
      case Some(item: ContextualToolbarItem.Input) =>
        copy(detailState = Some(ContextualToolbarDetailState.Input(item.id, item.inputItem.currentValue)))
      case _ =>
        this

  def closeDetail: ContextualToolbarState =
    copy(detailState = None)

  def moveDetailSelection(delta: Int, items: List[ContextualToolbarItem]): ContextualToolbarState =
    normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Dropdown(itemId, selectedIndex)) =>
        ContextualToolbar.dropdownItem(itemId, items) match
          case Some(dropdown) if dropdown.optionItem.options.nonEmpty =>
            val raw = (selectedIndex + delta) % dropdown.optionItem.options.length
            copy(
              detailState = Some(
                ContextualToolbarDetailState.Dropdown(
                  itemId,
                  if raw < 0 then raw + dropdown.optionItem.options.length else raw
                )
              )
            )
          case _ =>
            this
      case _ =>
        this

  def insertDetailChar(char: Char, items: List[ContextualToolbarItem]): ContextualToolbarState =
    normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Input(itemId, text)) =>
        ContextualToolbar.inputItem(itemId, items) match
          case Some(input) if input.inputItem.accepts(text, char) =>
            copy(detailState = Some(ContextualToolbarDetailState.Input(itemId, text + char)))
          case _ =>
            this
      case _ =>
        this

  def deleteDetailBackward(items: List[ContextualToolbarItem]): ContextualToolbarState =
    normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Input(itemId, text)) =>
        copy(detailState = Some(ContextualToolbarDetailState.Input(itemId, text.dropRight(1))))
      case _ =>
        this

  def normalized(items: List[ContextualToolbarItem]): ContextualToolbarState =
    copy(
      focusedIndex = clampIndex(focusedIndex, items),
      detailState = detailState.flatMap {
        case ContextualToolbarDetailState.Dropdown(itemId, selectedIndex) =>
          ContextualToolbar.dropdownItem(itemId, items).map { item =>
            ContextualToolbarDetailState.Dropdown(
              itemId,
              selectedIndex.max(0).min((item.optionItem.options.length - 1).max(0))
            )
          }
        case ContextualToolbarDetailState.Input(itemId, text) =>
          ContextualToolbar.inputItem(itemId, items).map(_ => ContextualToolbarDetailState.Input(itemId, text))
      }
    )

  private def normalizedFocusedIndex(items: List[ContextualToolbarItem]): Int =
    clampIndex(focusedIndex, items)

  private def clampIndex(index: Int, items: List[ContextualToolbarItem]): Int =
    if items.isEmpty then 0 else index.max(0).min(items.length - 1)

object ContextualToolbar:

  private val colorPresets = List(
    "Ink"  -> "#202020",
    "Blue" -> "#336699"
  )

  private val proseGroupIds = Map(
    "bold"             -> 0,
    "italic"           -> 0,
    "underline"        -> 0,
    "font-family"      -> 1,
    "font-family-text" -> 1,
    "font-size"        -> 1,
    "color"            -> 2,
    "color-hex"        -> 2,
    "paragraph-role"   -> 3,
    "align-left"       -> 4,
    "align-center"     -> 4,
    "align-right"      -> 4,
    "align-justify"    -> 4
  )

  val markdownItems: List[ContextualToolbarItem] = List(
    ContextualToolbarItem
      .Button("markdown-preview", "Preview", CommandIntent.View(ViewIntent.OpenMarkdownPreview), "\uf1c5"),
    ContextualToolbarItem.Button(
      "markdown-view-source",
      "Source",
      CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.Source)),
      "\ue86f"
    ),
    ContextualToolbarItem.Button(
      "markdown-view-split",
      "Split",
      CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview)),
      "\uf06d"
    ),
    ContextualToolbarItem.Button(
      "markdown-view-inline-lens",
      "Lens",
      CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens)),
      "\ue8b6"
    )
  )

  val codeItems: List[ContextualToolbarItem] = List(
    ContextualToolbarItem.Button(
      "project-build",
      "Build",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Build)),
      "\ue869"
    ),
    ContextualToolbarItem.Button(
      "project-test",
      "Test",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Test)),
      "\ue86c"
    ),
    ContextualToolbarItem
      .Button("project-run", "Run", CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Run)), "\ue037"),
    ContextualToolbarItem.Button(
      "project-debug",
      "Run Debug Task",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Debug)),
      "\ue868"
    )
  )

  def itemsFor(state: AppState): List[ContextualToolbarItem] =
    val context = state.editingContext
    state.activeBuffer.toList.flatMap { buffer =>
      context.buffer match
        case Some(BufferKind.Markdown) =>
          applyMarkdownSelections(markdownItems, state.persisted.config.markdownViewMode)
        // Prose workspaces have no project to build/test/run/debug (issue #1294), so the buttons that would launch one
        // are never offered there, even for a buffer whose own language happens to read as code.
        case Some(BufferKind.Code(_)) if context.hasCodeTooling => codeItems
        case _                                                  => proseItems(state, buffer)
    }

  def focusedCommand(
    toolbarState: ContextualToolbarState,
    state: AppState
  ): Option[Command] =
    toolbarState
      .normalized(itemsFor(state))
      .focusedItem(itemsFor(state))
      .collect { case item: ContextualToolbarItem.Button => item }
      .map(item => Command.typed(item.id, item.label, item.intent, label = item.label))

  def detailCommand(toolbarState: ContextualToolbarState, state: AppState): Option[Command] =
    val items = itemsFor(state)
    toolbarState.normalized(items).detailState.flatMap {
      case ContextualToolbarDetailState.Dropdown(itemId, selectedIndex) =>
        dropdownItem(itemId, items)
          .flatMap(_.optionItem.options.lift(selectedIndex))
          .map(option =>
            Command.typed(
              s"$itemId-$selectedIndex",
              option.label,
              option.intent,
              CommandCategory.Edit,
              label = option.label
            )
          )
      case ContextualToolbarDetailState.Input(itemId, text) =>
        inputItem(itemId, items)
          .flatMap(_.inputItem.parse(text))
          .map(intent => Command.typed(itemId, itemId, intent, CommandCategory.Edit, label = itemId))
    }

  def displayText(item: ContextualToolbarItem, mode: ToolbarDisplayMode): String =
    item match
      case ContextualToolbarItem.Button(_, label, _, icon, _) =>
        mode match
          case ToolbarDisplayMode.IconOnly    => icon
          case ToolbarDisplayMode.TextOnly    => label
          case ToolbarDisplayMode.IconAndText => s"$icon $label"
      case ContextualToolbarItem.Dropdown(_, label, icon, optionItem) =>
        val text = s"$label ${optionItem.selectedOption}".trim
        mode match
          case ToolbarDisplayMode.IconOnly    => icon
          case ToolbarDisplayMode.TextOnly    => text
          case ToolbarDisplayMode.IconAndText => s"$icon $text"
      case ContextualToolbarItem.Input(_, label, icon, inputItem) =>
        val text = s"$label ${inputItem.currentValue}".trim
        mode match
          case ToolbarDisplayMode.IconOnly    => icon
          case ToolbarDisplayMode.TextOnly    => text
          case ToolbarDisplayMode.IconAndText => s"$icon $text"

  /** The semantic formatting-control group an item belongs to, or `None` if it does not participate in grouping. */
  def formattingGroupId(item: ContextualToolbarItem): Option[Int] =
    proseGroupIds.get(item.id)

  /** Whether adjacent formatting controls belong to different visual groups. */
  def hasTrailingGroupSeparator(
    item: ContextualToolbarItem,
    nextItem: Option[ContextualToolbarItem]
  ): Boolean =
    (formattingGroupId(item), nextItem.flatMap(formattingGroupId)) match
      case (Some(group), Some(nextGroup)) => group != nextGroup
      case _                              => false

  def dropdownItem(itemId: String, items: List[ContextualToolbarItem]): Option[ContextualToolbarItem.Dropdown] =
    items.collectFirst { case item: ContextualToolbarItem.Dropdown if item.id == itemId => item }

  def inputItem(itemId: String, items: List[ContextualToolbarItem]): Option[ContextualToolbarItem.Input] =
    items.collectFirst { case item: ContextualToolbarItem.Input if item.id == itemId => item }

  def detailInputItem(
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem]
  ): Option[(ContextualToolbarItem.Input, String)] =
    toolbarState.normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Input(itemId, text)) =>
        inputItem(itemId, items).map(_ -> text)
      case _ =>
        None

  private def applyMarkdownSelections(
    items: List[ContextualToolbarItem],
    mode: MarkdownViewMode
  ): List[ContextualToolbarItem] =
    items.map {
      case item: ContextualToolbarItem.Button =>
        val selected =
          item.intent match
            case CommandIntent.View(ViewIntent.SetMarkdownViewMode(buttonMode)) => buttonMode == mode
            case _                                                              => false
        item.copy(selected = selected)
      case item =>
        item
    }

  private def proseItems(state: AppState, buffer: Buffer): List[ContextualToolbarItem] =
    val document  = ContextualToolbarRichTextStyle.richTextDocumentFor(buffer)
    val style     = ContextualToolbarRichTextStyle.activeStyle(buffer, document)
    val paragraph = ContextualToolbarRichTextStyle.activeParagraph(buffer, document)
    val currentFamily =
      style.fontFamily.orElse(Some(state.persisted.config.editorConfig.fontConfig.textFontFamily)).getOrElse("")
    val currentFontSize  = style.fontSize.getOrElse(state.persisted.config.editorConfig.fontConfig.textFontSize)
    val currentColor     = normalizedColor(style.color)
    val currentColorText = currentColor.getOrElse("#202020")
    val paragraphRole    = paragraph.map(_.role).getOrElse(ParagraphRole.Body)

    List(
      ContextualToolbarItem.Button(
        "bold",
        "Bold",
        CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)),
        "\ue238",
        selected = style.marks.contains(InlineMark.Bold)
      ),
      ContextualToolbarItem.Button(
        "italic",
        "Italic",
        CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(InlineMark.Italic)),
        "\ue23f",
        selected = style.marks.contains(InlineMark.Italic)
      ),
      ContextualToolbarItem.Button(
        "underline",
        "Underline",
        CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(InlineMark.Underline)),
        "\ue765",
        selected = style.marks.contains(InlineMark.Underline)
      )
    ) ++ fontFamilyItems(currentFamily, currentFontSize)
      ++ colorItems(currentColor, currentColorText)
      ++ paragraphRoleItems(paragraphRole)
      ++ List(
        ContextualToolbarItem.Button(
          "align-left",
          "Left",
          CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Left)),
          "\ue236",
          selected = paragraph.exists(_.alignment == ParagraphAlignment.Left)
        ),
        ContextualToolbarItem.Button(
          "align-center",
          "Center",
          CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Center)),
          "\ue234",
          selected = paragraph.exists(_.alignment == ParagraphAlignment.Center)
        ),
        ContextualToolbarItem.Button(
          "align-right",
          "Right",
          CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Right)),
          "\ue237",
          selected = paragraph.exists(_.alignment == ParagraphAlignment.Right)
        ),
        ContextualToolbarItem.Button(
          "align-justify",
          "Justify",
          CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Justify)),
          "\ue235",
          selected = paragraph.exists(_.alignment == ParagraphAlignment.Justify)
        )
      )

  private def fontFamilyItems(currentFamily: String, currentFontSize: Float): List[ContextualToolbarItem] =
    val familyOptions = normalizedFontFamilies(currentFamily).map(family =>
      CommandOption(family, CommandIntent.RichText(RichTextIntent.SetRichTextFontFamily(family)))
    )
    val familyIndex = familyOptions.indexWhere(_.label.equalsIgnoreCase(currentFamily)) match
      case -1    => 0
      case index => index

    List(
      ContextualToolbarItem.Dropdown(
        id = "font-family",
        label = "Font",
        icon = "\ue167",
        optionItem = CommandSurfaceItem.OptionItem(
          id = "font-family",
          label = "Font",
          options = familyOptions,
          selectedIndex = familyIndex,
          category = CommandCategory.Edit
        )
      ),
      ContextualToolbarItem.Input(
        id = "font-family-text",
        label = "Family",
        icon = "\ue262",
        inputItem = CommandSurfaceItem.InputItem(
          id = "font-family-text",
          label = "Family",
          hint = "Family name",
          currentValue = currentFamily,
          kind = CommandSurfaceItem.InputKind.FreeText,
          parse = CommandRunnerSettingsInputItems.parseRichTextFontFamily,
          category = CommandCategory.Edit
        )
      ),
      ContextualToolbarItem.Input(
        id = "font-size",
        label = "Size",
        icon = "\ue245",
        inputItem = CommandSurfaceItem.InputItem(
          id = "font-size",
          label = "Size",
          hint = "Points (8.0-48.0)",
          currentValue = formatFontSize(currentFontSize),
          kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
          parse = text =>
            text.toFloatOption
              .filter(size => size >= 8.0f && size <= 48.0f)
              .map(commandIntentArg => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(commandIntentArg))),
          category = CommandCategory.Edit
        )
      )
    )

  private def colorItems(currentColor: Option[String], currentColorText: String): List[ContextualToolbarItem] =
    val colorOptions = normalizedColorOptions(currentColor).map {
      case (label, color) =>
        CommandOption(label, CommandIntent.RichText(RichTextIntent.SetRichTextColor(color)))
    }
    val colorIndex =
      colorOptions.indexWhere(
        _.intent == CommandIntent.RichText(RichTextIntent.SetRichTextColor(currentColorText))
      ) match
        case -1    => 0
        case index => index

    List(
      ContextualToolbarItem.Dropdown(
        id = "color",
        label = "Color",
        icon = "\ue40a",
        optionItem = CommandSurfaceItem.OptionItem(
          id = "color",
          label = "Color",
          options = colorOptions,
          selectedIndex = colorIndex,
          category = CommandCategory.Edit
        )
      ),
      ContextualToolbarItem.Input(
        id = "color-hex",
        label = "Hex",
        icon = "\ue9ef",
        inputItem = CommandSurfaceItem.InputItem(
          id = "color-hex",
          label = "Hex",
          hint = "#RRGGBB",
          currentValue = currentColorText,
          kind = CommandSurfaceItem.InputKind.FreeText,
          parse = CommandRunnerSettingsInputItems.parseRichTextColor,
          category = CommandCategory.Edit
        )
      )
    )

  private def paragraphRoleItems(paragraphRole: ParagraphRole): List[ContextualToolbarItem] =
    val paragraphRoleOptions =
      CommandOption("Body", CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Body))) ::
        (1 to 6).toList.map(level =>
          CommandOption(
            s"H$level",
            CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(level)))
          )
        )
    val paragraphRoleIndex = paragraphRole match
      case ParagraphRole.Body           => 0
      case ParagraphRole.Heading(level) => level.max(1).min(paragraphRoleOptions.length - 1)

    List(
      ContextualToolbarItem.Dropdown(
        id = "paragraph-role",
        label = "Role",
        icon = "\ue264",
        optionItem = CommandSurfaceItem.OptionItem(
          id = "paragraph-role",
          label = "Role",
          options = paragraphRoleOptions,
          selectedIndex = paragraphRoleIndex,
          category = CommandCategory.Edit
        )
      )
    )

  private def normalizedFontFamilies(currentFamily: String): List[String] =
    val trimmedCurrent = currentFamily.trim
    val available      = FontLoader.availableTextFamilies
    if trimmedCurrent.nonEmpty && !available.exists(_.equalsIgnoreCase(trimmedCurrent)) then trimmedCurrent :: available
    else available

  private def normalizedColorOptions(currentColor: Option[String]): List[(String, String)] =
    currentColor match
      case Some(color) if !colorPresets.exists(_._2 == color) => (color, color) :: colorPresets
      case _                                                  => colorPresets

  private def normalizedColor(color: Option[String]): Option[String] =
    color.map(_.trim.toLowerCase)

  private def formatFontSize(size: Float): String =
    val rounded = size.round.toFloat
    if rounded == size then rounded.toInt.toString else f"$size%.1f"
