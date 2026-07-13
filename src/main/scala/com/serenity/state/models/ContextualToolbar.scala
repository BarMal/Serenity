package com.serenity.state.models

import com.serenity.command.*
import com.serenity.config.{MarkdownViewMode, ToolbarDisplayMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.ui.fonts.FontLoader

enum ContextualToolbarItem:
  def id: String
  def label: String
  def icon: String

  case Button(
      id: String,
      label: String,
      commandName: String,
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

case class ContextualToolbarState(
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

  def moveFocusVertical(
    deltaRows: Int,
    items: List[ContextualToolbarItem],
    contentWidth: Int
  ): ContextualToolbarState =
    if items.isEmpty || contentWidth <= 0 then this
    else
      val nextIndex =
        ContextualToolbar.moveVerticalIndex(
          rowGroups = ContextualToolbar.rowGroups(items, contentWidth, displayMode),
          currentIndex = normalizedFocusedIndex(items),
          deltaRows = deltaRows
        )
      copy(focusedIndex = nextIndex, detailState = None)

  def withFocusedIndex(index: Int, items: List[ContextualToolbarItem]): ContextualToolbarState =
    copy(focusedIndex = clampIndex(index, items))

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

  def moveDetailSelectionVertical(
    deltaRows: Int,
    items: List[ContextualToolbarItem],
    contentWidth: Int
  ): ContextualToolbarState =
    normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Dropdown(itemId, selectedIndex)) if contentWidth > 0 =>
        val rowGroups = ContextualToolbar.detailRowGroups(this, items, contentWidth)
        copy(
          detailState = Some(
            ContextualToolbarDetailState.Dropdown(
              itemId,
              ContextualToolbar.moveVerticalIndex(rowGroups, selectedIndex, deltaRows)
            )
          )
        )
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

  val markdownItems: List[ContextualToolbarItem] = List(
    ContextualToolbarItem.Button("markdown-preview", "Preview", "markdown-preview", "\uf1c5"),
    ContextualToolbarItem.Button("markdown-view-source", "Source", "markdown-view-source", "\ue86f"),
    ContextualToolbarItem.Button("markdown-view-split", "Split", "markdown-view-split", "\uf06d"),
    ContextualToolbarItem.Button("markdown-view-inline-lens", "Lens", "markdown-view-inline-lens", "\ue8b6")
  )

  val codeItems: List[ContextualToolbarItem] = List(
    ContextualToolbarItem.Button("project-build", "Build", "project-build", "\ue869"),
    ContextualToolbarItem.Button("project-test", "Test", "project-test", "\ue86c"),
    ContextualToolbarItem.Button("project-run", "Run", "project-run", "\ue037"),
    ContextualToolbarItem.Button("project-debug", "Debug", "project-debug", "\ue868")
  )

  def itemsFor(state: AppState): List[ContextualToolbarItem] =
    state.layout.activeEditorPaneId
      .flatMap(state.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.buffers.get)
      .map {
        case buffer if buffer.language.contains(LanguageId.Markdown) =>
          applyMarkdownSelections(markdownItems, state.config.markdownViewMode)
        case buffer if buffer.typographyRole == TypographyRole.Code =>
          codeItems
        case buffer =>
          proseItems(state, buffer)
      }
      .getOrElse(Nil)

  def focusedCommand(
    toolbarState: ContextualToolbarState,
    state: AppState,
    registry: CommandRegistry
  ): Option[Command] =
    toolbarState
      .normalized(itemsFor(state))
      .focusedItem(itemsFor(state))
      .collect { case item: ContextualToolbarItem.Button => item }
      .flatMap(item => registry.findCommand(item.commandName))

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

  def rowGroups(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[List[ContextualToolbarItem]] =
    if items.isEmpty || contentWidth <= 0 then Nil
    else if estimatedRowWidth(items, mode) <= contentWidth then List(items)
    else if proseItemSegments(items).exists(_.length > 1) then
      packSegments(proseItemSegments(items), contentWidth, mode)
    else packItems(items, contentWidth, mode)

  def compactContentWidth(toolbarState: ContextualToolbarState, state: AppState, maxWidth: Int): Int =
    val items = itemsFor(state)
    val detailWidth = toolbarState.normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Dropdown(itemId, _)) =>
        dropdownItem(itemId, items).map(_.optionItem.options.map(_.label.length + 2).sum).getOrElse(0)
      case Some(ContextualToolbarDetailState.Input(itemId, text)) =>
        inputItem(itemId, items).map(item => item.label.length + text.length + 3).getOrElse(0)
      case None =>
        0
    estimatedRowWidth(items, toolbarState.displayMode)
      .max(detailWidth)
      .max(1)
      .min(maxWidth.max(1))

  def rowCount(
    toolbarState: ContextualToolbarState,
    state: AppState,
    contentWidth: Int
  ): Int =
    val items    = itemsFor(state)
    val topLevel = rowGroups(items, contentWidth, toolbarState.displayMode).length
    val detailCount =
      toolbarState.normalized(items).detailState match
        case Some(_: ContextualToolbarDetailState.Dropdown) =>
          detailRowGroups(toolbarState, items, contentWidth).length
        case Some(_: ContextualToolbarDetailState.Input) =>
          1
        case None =>
          0
    (topLevel + detailCount).max(1)

  def detailRowGroups(
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem],
    contentWidth: Int
  ): List[List[CommandOption]] =
    toolbarState.normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Dropdown(itemId, _)) =>
        dropdownItem(itemId, items)
          .map(_.optionItem.options)
          .map(options => optionRowGroups(options, contentWidth))
          .getOrElse(Nil)
      case _ =>
        Nil

  def detailInputItem(
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem]
  ): Option[(ContextualToolbarItem.Input, String)] =
    toolbarState.normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Input(itemId, text)) =>
        inputItem(itemId, items).map(_ -> text)
      case _ =>
        None

  def hitAt(
    rowIndex: Int,
    columnOffset: Int,
    contentWidth: Int,
    toolbarState: ContextualToolbarState,
    state: AppState
  ): Option[ContextualToolbarHit] =
    val items        = itemsFor(state)
    val topLevelRows = rowGroups(items, contentWidth, toolbarState.displayMode)
    topLevelRows.lift(rowIndex) match
      case Some(rowItems) =>
        Option.when(rowItems.nonEmpty) {
          val offset = topLevelRows.take(rowIndex).map(_.length).sum
          val localIndex =
            ((columnOffset.max(0) * rowItems.length) / contentWidth.max(1))
              .max(0)
              .min(rowItems.length - 1)
          ContextualToolbarHit.TopLevelItem(offset + localIndex)
        }
      case None =>
        val detailRowIndex = rowIndex - topLevelRows.length
        toolbarState.normalized(items).detailState match
          case Some(ContextualToolbarDetailState.Dropdown(itemId, _)) =>
            val optionGroups = detailRowGroups(toolbarState, items, contentWidth)
            optionGroups.lift(detailRowIndex).flatMap { rowOptions =>
              Option.when(rowOptions.nonEmpty) {
                val offset = optionGroups.take(detailRowIndex).map(_.length).sum
                val localIndex =
                  ((columnOffset.max(0) * rowOptions.length) / contentWidth.max(1))
                    .max(0)
                    .min(rowOptions.length - 1)
                ContextualToolbarHit.DropdownOption(itemId, offset + localIndex)
              }
            }
          case Some(ContextualToolbarDetailState.Input(itemId, _)) if detailRowIndex == 0 =>
            Some(ContextualToolbarHit.InputDetail(itemId))
          case _ =>
            None

  def dropdownItem(itemId: String, items: List[ContextualToolbarItem]): Option[ContextualToolbarItem.Dropdown] =
    items.collectFirst { case item: ContextualToolbarItem.Dropdown if item.id == itemId => item }

  def inputItem(itemId: String, items: List[ContextualToolbarItem]): Option[ContextualToolbarItem.Input] =
    items.collectFirst { case item: ContextualToolbarItem.Input if item.id == itemId => item }

  private def proseItemSegments(items: List[ContextualToolbarItem]): List[List[ContextualToolbarItem]] =
    val proseGroupIds = Map(
      "bold"             -> 0,
      "italic"           -> 0,
      "underline"        -> 0,
      "font-family"      -> 1,
      "font-family-text" -> 1,
      "font-size"        -> 1,
      "color"            -> 2,
      "color-hex"        -> 2,
      "paragraph-role"   -> 3,
      "align-left"       -> 3,
      "align-center"     -> 3,
      "align-right"      -> 3,
      "align-justify"    -> 3
    )
    items.foldLeft(List.empty[List[ContextualToolbarItem]]) { (segments, item) =>
      val nextGroupId = proseGroupIds.get(item.id)
      segments match
        case init :+ last
            if last.nonEmpty &&
              proseGroupIds.get(last.head.id) == nextGroupId &&
              nextGroupId.nonEmpty =>
          init :+ (last :+ item)
        case _ =>
          segments :+ List(item)
    }

  private def packSegments(
    segments: List[List[ContextualToolbarItem]],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[List[ContextualToolbarItem]] =
    segments.flatMap(segment =>
      if estimatedRowWidth(segment, mode) > contentWidth then packItems(segment, contentWidth, mode)
      else List(segment)
    )

  private def packItems(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[List[ContextualToolbarItem]] =
    val (currentRow, rows) =
      items.foldLeft((List.empty[ContextualToolbarItem], List.empty[List[ContextualToolbarItem]])) {
        case ((currentRow, acc), item) =>
          val nextWidth = estimatedRowWidth(currentRow :+ item, mode)
          if currentRow.nonEmpty && nextWidth > contentWidth then (List(item), acc :+ currentRow)
          else (currentRow :+ item, acc)
      }
    rows :+ currentRow

  private def estimatedRowWidth(items: List[ContextualToolbarItem], mode: ToolbarDisplayMode): Int =
    items.map(item => displayTextWidth(displayText(item, mode)) + 2).sum + items.drop(1).length

  private def displayTextWidth(text: String): Int =
    text.codePoints().toArray.count { codePoint =>
      val category = Character.getType(codePoint)
      category != Character.NON_SPACING_MARK &&
      category != Character.COMBINING_SPACING_MARK &&
      category != Character.ENCLOSING_MARK
    }

  private def optionRowGroups(options: List[CommandOption], contentWidth: Int): List[List[CommandOption]] =
    if options.isEmpty || contentWidth <= 0 then Nil
    else
      val (currentRow, rows) =
        options.foldLeft((List.empty[CommandOption], List.empty[List[CommandOption]])) {
          case ((currentRow, acc), option) =>
            val nextWidth = currentRow.map(_.label.length + 2).sum + option.label.length + 2 + currentRow.length
            if currentRow.nonEmpty && nextWidth > contentWidth then (List(option), acc :+ currentRow)
            else (currentRow :+ option, acc)
        }
      rows :+ currentRow

  private[models] def moveVerticalIndex[A](
    rowGroups: List[List[A]],
    currentIndex: Int,
    deltaRows: Int
  ): Int =
    if rowGroups.isEmpty || deltaRows == 0 then currentIndex
    else
      val rowLengths = rowGroups.map(_.length)
      val totalItems = rowLengths.sum
      if totalItems == 0 then currentIndex
      else
        val step = deltaRows.sign
        Iterator
          .fill(deltaRows.abs)(step)
          .foldLeft(currentIndex.max(0).min(totalItems - 1)) { (index, rowDelta) =>
            val (rowIndex, localIndex) = rowAndLocalIndex(rowLengths, index)
            val targetRowIndex         = (rowIndex + rowDelta).max(0).min(rowLengths.length - 1)
            if targetRowIndex == rowIndex then index
            else
              val targetLength = rowLengths(targetRowIndex)
              rowLengths.take(targetRowIndex).sum +
                proportionalIndex(localIndex, rowLengths(rowIndex), targetLength)
          }

  private def rowAndLocalIndex(rowLengths: List[Int], globalIndex: Int): (Int, Int) =
    rowLengths
      .foldLeft((0, 0, Option.empty[(Int, Int)])) {
        case ((offset, rowIndex, found), rowLength) =>
          found match
            case some @ Some(_) =>
              (offset + rowLength, rowIndex + 1, some)
            case None if globalIndex < offset + rowLength =>
              (offset + rowLength, rowIndex + 1, Some((rowIndex, globalIndex - offset)))
            case None =>
              (offset + rowLength, rowIndex + 1, None)
      }
      ._3
      .getOrElse {
        val lastRowIndex  = (rowLengths.length - 1).max(0)
        val lastRowLength = rowLengths.lift(lastRowIndex).getOrElse(1).max(1)
        (lastRowIndex, lastRowLength - 1)
      }

  private def proportionalIndex(currentIndex: Int, currentRowLength: Int, targetRowLength: Int): Int =
    (((currentIndex + 0.5d) * targetRowLength) / currentRowLength.max(1)).toInt
      .max(0)
      .min(targetRowLength - 1)

  private def applyMarkdownSelections(
    items: List[ContextualToolbarItem],
    mode: MarkdownViewMode
  ): List[ContextualToolbarItem] =
    items.map {
      case item: ContextualToolbarItem.Button =>
        val selected =
          item.commandName match
            case "markdown-view-source"      => mode == MarkdownViewMode.Source
            case "markdown-view-split"       => mode == MarkdownViewMode.SplitPreview
            case "markdown-view-inline-lens" => mode == MarkdownViewMode.InlineLens
            case _                           => false
        item.copy(selected = selected)
      case item =>
        item
    }

  private def proseItems(state: AppState, buffer: Buffer): List[ContextualToolbarItem] =
    val document         = richTextDocumentFor(buffer)
    val style            = activeStyle(buffer, document)
    val paragraph        = activeParagraph(buffer, document)
    val currentFamily    = style.fontFamily.orElse(Some(state.config.fontConfig.textFontFamily)).getOrElse("")
    val currentFontSize  = style.fontSize.getOrElse(state.config.fontConfig.textFontSize)
    val currentColor     = normalizedColor(style.color)
    val currentColorText = currentColor.getOrElse("#202020")
    val familyOptions = normalizedFontFamilies(currentFamily).map(family =>
      CommandOption(family, CommandIntent.SetRichTextFontFamily(family))
    )
    val familyIndex = familyOptions.indexWhere(_.label.equalsIgnoreCase(currentFamily)) match
      case -1    => 0
      case index => index
    val colorOptions = normalizedColorOptions(currentColor).map {
      case (label, color) =>
        CommandOption(label, CommandIntent.SetRichTextColor(color))
    }
    val colorIndex =
      colorOptions.indexWhere(_.intent == CommandIntent.SetRichTextColor(currentColorText)) match
        case -1    => 0
        case index => index
    val paragraphRole = paragraph.map(_.role).getOrElse(ParagraphRole.Body)
    val paragraphRoleOptions =
      CommandOption("Body", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Body)) ::
        (1 to 6).toList.map(level =>
          CommandOption(s"H$level", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(level)))
        )
    val paragraphRoleIndex = paragraphRole match
      case ParagraphRole.Body           => 0
      case ParagraphRole.Heading(level) => level.max(1).min(paragraphRoleOptions.length - 1)

    List(
      ContextualToolbarItem.Button(
        "bold",
        "Bold",
        "bold",
        "\ue238",
        selected = style.marks.contains(InlineMark.Bold)
      ),
      ContextualToolbarItem.Button(
        "italic",
        "Italic",
        "italic",
        "\ue23f",
        selected = style.marks.contains(InlineMark.Italic)
      ),
      ContextualToolbarItem.Button(
        "underline",
        "Underline",
        "underline",
        "\ue765",
        selected = style.marks.contains(InlineMark.Underline)
      ),
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
          isDecimal = false,
          parse = CommandRunnerSettingsInputItems.parseRichTextFontFamily,
          category = CommandCategory.Edit,
          acceptsFreeText = true
        )
      ),
      ContextualToolbarItem.Input(
        id = "font-size",
        label = "Size",
        icon = "\ue245",
        inputItem = CommandSurfaceItem.InputItem(
          id = "font-size",
          label = "Size",
          hint = "Points (1.0-144.0)",
          currentValue = formatFontSize(currentFontSize),
          isDecimal = true,
          parse = text =>
            text.toFloatOption
              .filter(size => size >= 1.0f && size <= 144.0f)
              .map(CommandIntent.SetRichTextFontSize(_)),
          category = CommandCategory.Edit
        )
      ),
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
          isDecimal = false,
          parse = CommandRunnerSettingsInputItems.parseRichTextColor,
          category = CommandCategory.Edit,
          acceptsFreeText = true
        )
      ),
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
      ),
      ContextualToolbarItem.Button(
        "align-left",
        "Left",
        "align-left",
        "\ue236",
        selected = paragraph.exists(_.alignment == ParagraphAlignment.Left)
      ),
      ContextualToolbarItem.Button(
        "align-center",
        "Center",
        "align-center",
        "\ue234",
        selected = paragraph.exists(_.alignment == ParagraphAlignment.Center)
      ),
      ContextualToolbarItem.Button(
        "align-right",
        "Right",
        "align-right",
        "\ue237",
        selected = paragraph.exists(_.alignment == ParagraphAlignment.Right)
      ),
      ContextualToolbarItem.Button(
        "align-justify",
        "Justify",
        "align-justify",
        "\ue235",
        selected = paragraph.exists(_.alignment == ParagraphAlignment.Justify)
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

  private def richTextDocumentFor(buffer: Buffer): RichTextDocument =
    val text = buffer.content.collect()
    buffer.richTextDocument
      .filter(_.matchesPlainText(text))
      .getOrElse(RichTextDocument.fromPlainText(text))

  private def activeParagraph(
    buffer: Buffer,
    document: RichTextDocument
  ): Option[com.serenity.richtext.RichTextParagraph] =
    val paragraphIndex = currentRange(buffer, document).start.paragraphIndex
    document.paragraphs.lift(paragraphIndex)

  private def activeStyle(
    buffer: Buffer,
    document: RichTextDocument
  ): RichTextStyle =
    buffer.primarySelection match
      case Some(selection) if selection.start != selection.end =>
        styleForSelection(selection, document)
      case _ =>
        buffer.cursors.headOption
          .flatMap(cursor => styleAtCursor(cursor, document))
          .getOrElse(RichTextStyle.empty)

  private def styleForSelection(
    selection: Selection,
    document: RichTextDocument
  ): RichTextStyle =
    val range = richTextRange(selection)
    document.paragraphs
      .lift(range.start.paragraphIndex)
      .flatMap(paragraph => styleAtParagraphOffset(paragraph, range.start.offset))
      .getOrElse(styleAtCursor(selection.focus, document).getOrElse(RichTextStyle.empty))

  private def styleAtCursor(
    cursor: CursorPosition,
    document: RichTextDocument
  ): Option[RichTextStyle] =
    val paragraphIndex = cursor.line.max(0).min(document.paragraphs.length - 1)
    document.paragraphs
      .lift(paragraphIndex)
      .flatMap(paragraph => styleAtParagraphOffset(paragraph, (cursor.column - 1).max(0)))

  private def styleAtParagraphOffset(
    paragraph: com.serenity.richtext.RichTextParagraph,
    offset: Int
  ): Option[RichTextStyle] =
    val clampedOffset = offset.max(0).min(paragraph.plainText.length)
    val targetOffset =
      if clampedOffset == paragraph.plainText.length && clampedOffset > 0 then clampedOffset - 1
      else clampedOffset
    paragraph.runs
      .foldLeft((0, Option.empty[RichTextStyle])) {
        case ((currentOffset, found), run) =>
          val nextOffset     = currentOffset + run.text.length
          val containsOffset = targetOffset >= currentOffset && targetOffset < nextOffset
          (nextOffset, found.orElse(Option.when(containsOffset)(run.style)))
      }
      ._2

  private def currentRange(
    buffer: Buffer,
    document: RichTextDocument
  ): RichTextRange =
    buffer.primarySelection
      .map(richTextRange)
      .orElse(
        buffer.cursors.headOption
          .map(cursor => RichTextRange(richTextPosition(cursor, document), richTextPosition(cursor, document)))
      )
      .getOrElse(RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 0)))

  private def richTextRange(selection: Selection): RichTextRange =
    RichTextRange(
      start = RichTextPosition(selection.start.line, selection.start.column),
      end = RichTextPosition(selection.end.line, selection.end.column)
    ).normalized

  private def richTextPosition(
    cursor: CursorPosition,
    document: RichTextDocument
  ): RichTextPosition =
    val paragraphIndex = cursor.line.max(0).min((document.paragraphs.length - 1).max(0))
    val offset = document.paragraphs
      .lift(paragraphIndex)
      .map(_.plainText.length)
      .map(length => cursor.column.max(0).min(length))
      .getOrElse(0)
    RichTextPosition(paragraphIndex, offset)

end ContextualToolbar
