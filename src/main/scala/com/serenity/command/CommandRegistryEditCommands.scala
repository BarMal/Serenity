package com.serenity.command

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole}

/** Text editing and rich-text formatting commands. Split out of `CommandRegistry.defaultCommands` to keep both under
  * the architecture size targets -- see that method's doc.
  */
private[command] object CommandRegistryEditCommands:

  private[command] def editCommands: List[Command] = List(
    Command.typed(
      "find",
      "Find text in the current file.",
      CommandIntent.Edit(EditIntent.FindInCurrentFile),
      CommandCategory.Edit,
      label = "Find"
    ),
    Command.typed(
      "find-all",
      "Show every match for text in the current file.",
      CommandIntent.Edit(EditIntent.FindAllInCurrentFile),
      CommandCategory.Edit,
      label = "Find All"
    ),
    Command.typed(
      "replace",
      "Find and replace text in the current file.",
      CommandIntent.Edit(EditIntent.ReplaceInCurrentFile),
      CommandCategory.Edit,
      label = "Replace"
    ),
    Command.typed(
      "replace-all",
      "Replace every match in the current file or active selection.",
      CommandIntent.Edit(EditIntent.ReplaceAllInCurrentFile),
      CommandCategory.Edit,
      label = "Replace All"
    ),
    Command.typed(
      "copy",
      "Copy the active selection or current line.",
      CommandIntent.Edit(EditIntent.Copy),
      CommandCategory.Edit,
      label = "Copy"
    ),
    Command.typed(
      "cut",
      "Cut the active selection or current line.",
      CommandIntent.Edit(EditIntent.Cut),
      CommandCategory.Edit,
      label = "Cut"
    ),
    Command.typed(
      "paste",
      "Paste clipboard text at the cursor.",
      CommandIntent.Edit(EditIntent.Paste),
      CommandCategory.Edit,
      label = "Paste"
    ),
    Command.typed(
      "select-all",
      "Select all text in the current file.",
      CommandIntent.Edit(EditIntent.SelectAll),
      CommandCategory.Edit,
      label = "Select All"
    ),
    Command.typed(
      "undo",
      "Undo the most recent edit.",
      CommandIntent.Edit(EditIntent.Undo),
      CommandCategory.Edit,
      label = "Undo"
    ),
    Command.typed(
      "redo",
      "Redo the most recently undone edit.",
      CommandIntent.Edit(EditIntent.Redo),
      CommandCategory.Edit,
      label = "Redo"
    ),
    Command.typed(
      "bold",
      "Toggle bold formatting on the active selection.",
      CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Bold)),
      CommandCategory.Edit,
      label = "Bold"
    )
  )

  private[command] def richTextCommands: List[Command] = List(
    Command.typed(
      "italic",
      "Toggle italic formatting on the active selection.",
      CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Italic)),
      CommandCategory.Edit,
      label = "Italic"
    ),
    Command.typed(
      "underline",
      "Toggle underline formatting on the active selection.",
      CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Underline)),
      CommandCategory.Edit,
      label = "Underline"
    ),
    Command.typed(
      "paragraph-body",
      "Set the active paragraph to body text.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Body)),
      CommandCategory.Edit,
      label = "Body Text"
    ),
    Command.typed(
      "heading-1",
      "Set the active paragraph to heading level 1.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(1))),
      CommandCategory.Edit,
      label = "Heading 1"
    ),
    Command.typed(
      "heading-2",
      "Set the active paragraph to heading level 2.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(2))),
      CommandCategory.Edit,
      label = "Heading 2"
    ),
    Command.typed(
      "heading-3",
      "Set the active paragraph to heading level 3.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(3))),
      CommandCategory.Edit,
      label = "Heading 3"
    ),
    Command.typed(
      "align-left",
      "Align the active paragraph to the left.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Left)),
      CommandCategory.Edit,
      label = "Align Left"
    ),
    Command.typed(
      "align-center",
      "Center the active paragraph.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Center)),
      CommandCategory.Edit,
      label = "Align Center"
    ),
    Command.typed(
      "align-right",
      "Align the active paragraph to the right.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Right)),
      CommandCategory.Edit,
      label = "Align Right"
    ),
    Command.typed(
      "align-justify",
      "Justify the active paragraph.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Justify)),
      CommandCategory.Edit,
      label = "Justify"
    )
  )
