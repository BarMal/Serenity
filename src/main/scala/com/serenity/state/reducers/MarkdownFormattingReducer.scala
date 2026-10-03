package com.serenity.state.reducers

import com.serenity.command.RichTextIntent
import com.serenity.markdown.{Emphasis, MarkdownFormatting, Reformatted}
import com.serenity.richtext.ParagraphRole
import com.serenity.state.models.*

/** Formatting commands on a Markdown file, applied by editing its source -- `**`, `*`, `<u>` and `#` -- as one undoable
  * edit, so the formatting is in the text the file saves rather than in a model a save would drop.
  */
private[reducers] object MarkdownFormattingReducer:
  import EditorEditSupport.*

  enum MarkdownSyntax:
    case Emphasize(emphasis: Emphasis)
    case Heading(level: Int)

  /** How Markdown spells `intent`, or why it can't. */
  def syntaxFor(intent: RichTextIntent): Either[String, MarkdownSyntax] =
    intent match
      case RichTextIntent.ToggleRichTextMark(mark) =>
        Right(MarkdownSyntax.Emphasize(MarkdownSelection.emphasisOf(mark)))
      case RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Body)           => Right(MarkdownSyntax.Heading(0))
      case RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(level)) => Right(MarkdownSyntax.Heading(level))
      case RichTextIntent.SetRichTextParagraphRole(ParagraphRole.DropCap(_)) =>
        Left("Markdown has no syntax for drop caps.")
      case RichTextIntent.SetRichTextFontFamily(_) | RichTextIntent.SetRichTextFontSize(_) |
          RichTextIntent.AdjustRichTextFontSize(_) =>
        Left("Markdown has no syntax for fonts or text sizes.")
      case RichTextIntent.SetRichTextColor(_)              => Left("Markdown has no syntax for text colour.")
      case RichTextIntent.SetRichTextParagraphAlignment(_) => Left("Markdown has no syntax for paragraph alignment.")
      case RichTextIntent.ConvertToRichText(_) => Left("Markdown files keep their formatting as Markdown syntax.")

  def reduce(intent: RichTextIntent, state: AppState, paneId: PaneId, buffer: Buffer): ReducerResult =
    syntaxFor(intent).fold(
      _ => ReducerResult.noEffects(state),
      syntax =>
        val source = buffer.document.content
        val ranges = MarkdownSelection.ranges(buffer)
        val reformatted = syntax match
          case MarkdownSyntax.Emphasize(emphasis) => MarkdownFormatting.toggle(source, ranges, emphasis)
          case MarkdownSyntax.Heading(level)      => MarkdownFormatting.setHeading(source, ranges, level)
        if reformatted.edits.isEmpty then ReducerResult.noEffects(state)
        else applied(state, paneId, buffer, reformatted)
    )

  private def applied(state: AppState, paneId: PaneId, buffer: Buffer, reformatted: Reformatted): ReducerResult =
    // Applied last to first so each edit's offsets still describe the text it was computed against.
    val edits = reformatted.edits.reverse.map(edit => MultiCursorEdit(0, edit.start, edit.end, edit.text))
    val (content, richTextDocument) = foldEditsWithRichText(buffer, edits) { (current, edit) =>
      insertOrUnchanged(deleteOrUnchanged(current, edit.start, edit.end), edit.start, edit.insertedText)
    }
    val cursors = reformatted.ranges.map { range =>
      val focus = content.offsetToCursorPosition(range.focus)
      Cursor(focus, Option.when(range.anchor != range.focus)(content.offsetToCursorPosition(range.anchor)))
    }
    val edited = buffer
      .withEditedContent(
        content = content,
        cursors = cursors.map(_.position),
        adjustedAnnotations = adjustAnnotations(
          buffer.annotations,
          buffer.document.content,
          content,
          edits
        ),
        richTextDocument = richTextDocument
      )
      .copy(editing = EditingState.fromCursors(cursors))
    ReducerResult(
      state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(buffer.id, edited))),
      undoBoundaryEffects(buffer.id, paneId, buffer, edits, groupable = false)
    )
