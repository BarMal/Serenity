package com.serenity.state.models

import com.serenity.command.{Command, CommandRegistry}
import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*

enum ToolbarDisplayMode:
  case IconOnly
  case TextOnly
  case IconAndText

case class ContextualToolbarItem(
    id: String,
    label: String,
    commandName: String,
    icon: String,
    selected: Boolean = false
)

case class ContextualToolbarState(
    focusedIndex: Int = 0,
    displayMode: ToolbarDisplayMode = ToolbarDisplayMode.IconAndText
):

  def focusedItem(items: List[ContextualToolbarItem]): Option[ContextualToolbarItem] =
    items.lift(normalizedFocusedIndex(items))

  def moveFocus(delta: Int, items: List[ContextualToolbarItem]): ContextualToolbarState =
    if items.isEmpty then copy(focusedIndex = 0)
    else
      val raw = (normalizedFocusedIndex(items) + delta) % items.length
      copy(focusedIndex = if raw < 0 then raw + items.length else raw)

  def withFocusedIndex(index: Int, items: List[ContextualToolbarItem]): ContextualToolbarState =
    copy(focusedIndex = clampIndex(index, items))

  def normalized(items: List[ContextualToolbarItem]): ContextualToolbarState =
    copy(focusedIndex = clampIndex(focusedIndex, items))

  private def normalizedFocusedIndex(items: List[ContextualToolbarItem]): Int =
    clampIndex(focusedIndex, items)

  private def clampIndex(index: Int, items: List[ContextualToolbarItem]): Int =
    if items.isEmpty then 0 else index.max(0).min(items.length - 1)

object ContextualToolbar:

  val proseItems: List[ContextualToolbarItem] = List(
    ContextualToolbarItem("bold", "Bold", "bold", "B"),
    ContextualToolbarItem("italic", "Italic", "italic", "I"),
    ContextualToolbarItem("underline", "Underline", "underline", "U"),
    ContextualToolbarItem("rich-text-font-serif", "Serif", "rich-text-font-serif", "Sf"),
    ContextualToolbarItem("rich-text-font-sans", "Sans", "rich-text-font-sans", "Sa"),
    ContextualToolbarItem("rich-text-size-14", "14pt", "rich-text-size-14", "14"),
    ContextualToolbarItem("rich-text-size-18", "18pt", "rich-text-size-18", "18"),
    ContextualToolbarItem("rich-text-color-ink", "Ink", "rich-text-color-ink", "K"),
    ContextualToolbarItem("rich-text-color-blue", "Blue", "rich-text-color-blue", "B"),
    ContextualToolbarItem("paragraph-body", "Body", "paragraph-body", "P"),
    ContextualToolbarItem("heading-1", "H1", "heading-1", "1"),
    ContextualToolbarItem("heading-2", "H2", "heading-2", "2"),
    ContextualToolbarItem("heading-3", "H3", "heading-3", "3"),
    ContextualToolbarItem("align-left", "Left", "align-left", "L"),
    ContextualToolbarItem("align-center", "Center", "align-center", "C"),
    ContextualToolbarItem("align-right", "Right", "align-right", "R"),
    ContextualToolbarItem("align-justify", "Justify", "align-justify", "J")
  )

  val markdownItems: List[ContextualToolbarItem] = List(
    ContextualToolbarItem("markdown-preview", "Preview", "markdown-preview", "P"),
    ContextualToolbarItem("markdown-view-source", "Source", "markdown-view-source", "S"),
    ContextualToolbarItem("markdown-view-split", "Split", "markdown-view-split", "V"),
    ContextualToolbarItem("markdown-view-inline-lens", "Lens", "markdown-view-inline-lens", "L")
  )

  val codeItems: List[ContextualToolbarItem] = List(
    ContextualToolbarItem("project-build", "Build", "project-build", "B"),
    ContextualToolbarItem("project-test", "Test", "project-test", "T"),
    ContextualToolbarItem("project-run", "Run", "project-run", "R"),
    ContextualToolbarItem("project-debug", "Debug", "project-debug", "D")
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
          applyRichTextSelections(proseItems, buffer)
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
      .flatMap(item => registry.findCommand(item.commandName))

  def displayText(item: ContextualToolbarItem, mode: ToolbarDisplayMode): String =
    mode match
      case ToolbarDisplayMode.IconOnly    => item.icon
      case ToolbarDisplayMode.TextOnly    => item.label
      case ToolbarDisplayMode.IconAndText => s"${item.icon} ${item.label}"

  def rowGroups(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[List[ContextualToolbarItem]] =
    if items.isEmpty || contentWidth <= 0 then Nil
    else
      val (currentRow, rows) =
        items.foldLeft((List.empty[ContextualToolbarItem], List.empty[List[ContextualToolbarItem]])) {
          case ((currentRow, acc), item) =>
            val nextWidth = estimatedRowWidth(currentRow :+ item, mode)
            if currentRow.nonEmpty && nextWidth > contentWidth then (List(item), acc :+ currentRow)
            else (currentRow :+ item, acc)
        }
      rows :+ currentRow

  private def estimatedRowWidth(items: List[ContextualToolbarItem], mode: ToolbarDisplayMode): Int =
    items.map(item => displayText(item, mode).length + 2).sum + items.drop(1).length

  private def applyMarkdownSelections(
    items: List[ContextualToolbarItem],
    mode: MarkdownViewMode
  ): List[ContextualToolbarItem] =
    items.map { item =>
      val selected =
        item.commandName match
          case "markdown-view-source"      => mode == MarkdownViewMode.Source
          case "markdown-view-split"       => mode == MarkdownViewMode.SplitPreview
          case "markdown-view-inline-lens" => mode == MarkdownViewMode.InlineLens
          case _                           => false
      item.copy(selected = selected)
    }

  private def applyRichTextSelections(
    items: List[ContextualToolbarItem],
    buffer: Buffer
  ): List[ContextualToolbarItem] =
    val document  = richTextDocumentFor(buffer)
    val style     = activeStyle(buffer, document)
    val paragraph = activeParagraph(buffer, document)
    items.map { item =>
      val selected =
        item.commandName match
          case "bold"                 => style.marks.contains(InlineMark.Bold)
          case "italic"               => style.marks.contains(InlineMark.Italic)
          case "underline"            => style.marks.contains(InlineMark.Underline)
          case "rich-text-font-serif" => style.fontFamily.contains("Serif")
          case "rich-text-font-sans"  => style.fontFamily.contains("SansSerif")
          case "rich-text-size-14"    => style.fontSize.contains(14.0f)
          case "rich-text-size-18"    => style.fontSize.contains(18.0f)
          case "rich-text-color-ink"  => normalizedColor(style.color).contains("#202020")
          case "rich-text-color-blue" => normalizedColor(style.color).contains("#336699")
          case "paragraph-body"       => paragraph.exists(_.role == ParagraphRole.Body)
          case "heading-1"            => paragraph.exists(_.role == ParagraphRole.Heading(1))
          case "heading-2"            => paragraph.exists(_.role == ParagraphRole.Heading(2))
          case "heading-3"            => paragraph.exists(_.role == ParagraphRole.Heading(3))
          case "align-left"           => paragraph.exists(_.alignment == ParagraphAlignment.Left)
          case "align-center"         => paragraph.exists(_.alignment == ParagraphAlignment.Center)
          case "align-right"          => paragraph.exists(_.alignment == ParagraphAlignment.Right)
          case "align-justify"        => paragraph.exists(_.alignment == ParagraphAlignment.Justify)
          case _                      => false
      item.copy(selected = selected)
    }

  private def normalizedColor(color: Option[String]): Option[String] =
    color.map(_.trim.toLowerCase)

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
    document.paragraphs.lift(paragraphIndex).flatMap(paragraph => styleAtParagraphOffset(paragraph, cursor.column))

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
