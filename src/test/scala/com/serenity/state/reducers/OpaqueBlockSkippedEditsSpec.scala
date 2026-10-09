package com.serenity.state.reducers

import java.nio.file.Paths

import com.serenity.command.RichTextIntent
import com.serenity.keystroke.events.{DeleteForward, DeleteToLineEnd, DeleteToLineStart, TabKey}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{LspPosition, LspRange, LspTextEdit}
import com.serenity.richtext.{DocumentFeature, ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.undo.HistoryEntry
import com.serenity.ui.layout.{Layout, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** An edit skipped because it would put text beside a block line (#1896) changed nothing, so the cursors, bookmarks and
  * the user's report of what was done must not count it.
  */
class OpaqueBlockSkippedEditsSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)
  private val path     = Paths.get("/tmp/serenity-block-edits/report.docx")

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("alpha"),
      RichTextParagraph.plain("beta"),
      RichTextParagraph.block("<w:tbl/>", DocumentFeature.Tables),
      RichTextParagraph.plain("gamma")
    )
  )

  private def buffer(editing: EditingState, bookmarks: List[CursorPosition] = Nil): Buffer =
    Buffer(
      id = bufferId,
      document = Document(content = Rope(document.plainText), filePath = Some(path)),
      editing = editing,
      richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 0L),
      annotations = Annotations(bookmarks = bookmarks)
    )

  private def editorState(buffer: Buffer): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        )
      )
    )

  private def cursorsAt(positions: CursorPosition*): EditingState =
    EditingState.fromCursors(positions.toList.map(Cursor(_)))

  private def undoEntries(effects: List[AppEffect]): Int =
    effects.count {
      case AppEffect.Undo(_: UndoEffect.RecordBoundary) => true
      case _                                            => false
    }

  "A refused deletion before or after a block" should "record no undo entry and leave the buffer clean" in {
    val beforeBlock = editorState(buffer(cursorsAt(CursorPosition(1, 4))))
    val afterBlock  = editorState(buffer(cursorsAt(CursorPosition(3, 0))))

    val refused = List(
      EditorEventReducer.reduce(DeleteToLineEnd, paneId, beforeBlock),
      EditorEventReducer.reduce(DeleteForward, paneId, beforeBlock),
      EditorEventReducer.reduce(DeleteToLineStart, paneId, afterBlock)
    )

    refused.map(result => undoEntries(result.effects)) shouldBe List(0, 0, 0)
    refused.map(_.state.persisted.buffers(bufferId).document.isDirty) shouldBe List(false, false, false)
  }

  "A multi-cursor deletion with one cursor refused" should "record exactly one undo entry whose snapshot is the text before" in {
    val state = editorState(buffer(cursorsAt(CursorPosition(0, 2), CursorPosition(1, 4))))

    val result = EditorEventReducer.reduce(DeleteToLineEnd, paneId, state)

    val entries = result.effects.collect { case AppEffect.Undo(UndoEffect.RecordBoundary(entry, _)) => entry }
    entries.size shouldBe 1
    entries.collect { case edit: HistoryEntry.BufferEdit => edit.snapshot.content.collect() } shouldBe
      List(document.plainText)
    result.state.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  "A rename whose only edit would join a block" should "record no undo entry" in {
    val joinsBlock = LspTextEdit(LspRange(LspPosition(1, 4), LspPosition(2, 0)), "")
    val state      = editorState(buffer(cursorsAt(CursorPosition(0, 0))))

    val result = RenameEditReducer(Map(path.toUri.toString -> List(joinsBlock)), CursorPosition(0, 0), state)

    undoEntries(result.effects) shouldBe 0
  }

  "A rename with an edit that would join a block" should "apply the others, count only those, and move cursors by them" in {
    val joinsBlock = LspTextEdit(LspRange(LspPosition(1, 4), LspPosition(2, 0)), "")
    val renames    = LspTextEdit(LspRange(LspPosition(0, 0), LspPosition(0, 5)), "alphabet")
    val state = editorState(buffer(cursorsAt(CursorPosition(0, 5), CursorPosition(3, 2)), List(CursorPosition(3, 1))))

    val result = RenameEditReducer(Map(path.toUri.toString -> List(joinsBlock, renames)), CursorPosition(0, 0), state)

    val after = result.state.persisted.buffers(bufferId)
    after.document.content.collect() shouldBe document.plainText.replace("alpha", "alphabet")
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 8), CursorPosition(3, 2))
    after.annotations.bookmarks shouldBe List(CursorPosition(3, 1))
    result.state.runtime.uiSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.QuickInfo(text), _, _) =>
        text
    }.value should startWith("Renamed 1 location(s) in this file.")
  }

  "Multi-cursor Delete with one cursor at the end of the line before a block" should "leave that cursor where it was" in {
    val state = editorState(buffer(cursorsAt(CursorPosition(0, 0), CursorPosition(1, 4), CursorPosition(3, 0))))

    val after = EditorEventReducer.reduce(DeleteForward, paneId, state).state.persisted.buffers(bufferId)

    after.document.content.collect() shouldBe "lpha\nbeta\n⁤\namma"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 0), CursorPosition(1, 4), CursorPosition(3, 0))
  }

  "Delete to line end at the end of the line before a block" should "be refused like Delete is" in {
    val state = editorState(buffer(cursorsAt(CursorPosition(1, 4))))

    val after = EditorEventReducer.reduce(DeleteToLineEnd, paneId, state).state.persisted.buffers(bufferId)

    after.document.content.collect() shouldBe document.plainText
    after.editing.cursorPositions shouldBe List(CursorPosition(1, 4))
  }

  "Delete to line start at the start of the line after a block" should "be refused" in {
    val state = editorState(buffer(cursorsAt(CursorPosition(3, 0))))

    val after = EditorEventReducer.reduce(DeleteToLineStart, paneId, state).state.persisted.buffers(bufferId)

    after.document.content.collect() shouldBe document.plainText
    after.editing.cursorPositions shouldBe List(CursorPosition(3, 0))
  }

  "Multi-cursor delete to line end with one cursor at the end of the line before a block" should "delete for the other and leave that cursor where it was" in {
    val state = editorState(buffer(cursorsAt(CursorPosition(0, 2), CursorPosition(1, 4))))

    val after = EditorEventReducer.reduce(DeleteToLineEnd, paneId, state).state.persisted.buffers(bufferId)

    after.document.content.collect() shouldBe "al\nbeta\n⁤\ngamma"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 2), CursorPosition(1, 4))
  }

  "Multi-cursor delete to line start with one cursor at the start of the line after a block" should "delete for the other and leave that cursor where it was" in {
    val state = editorState(buffer(cursorsAt(CursorPosition(0, 5), CursorPosition(3, 0))))

    val after = EditorEventReducer.reduce(DeleteToLineStart, paneId, state).state.persisted.buffers(bufferId)

    after.document.content.collect() shouldBe "\nbeta\n⁤\ngamma"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 0), CursorPosition(3, 0))
  }

  "Indenting a selection that covers a block line" should "leave a cursor on the block line where it was" in {
    val selection = Selection(CursorPosition(1, 2), CursorPosition(2, 0))
    val state     = editorState(buffer(EditingState.fromCursors(List(Cursor(selection)))))

    val after = EditorEventReducer.reduce(TabKey, paneId, state).state.persisted.buffers(bufferId)

    after.document.content.getLine(1) shouldBe Some("    beta")
    after.document.content.getLine(2) shouldBe Some("⁤")
    after.editing.cursorPositions shouldBe List(CursorPosition(2, 0))
  }

  private def markdown(editing: EditingState): AppState =
    val base = buffer(editing)
    editorState(
      base.copy(document =
        base.document.copy(filePath = Some(Paths.get("notes.md")), language = Some(LanguageId.Markdown))
      )
    )

  "A Markdown heading with a cursor on a block line" should "leave that cursor where it was when its edit is skipped" in {
    val state = markdown(cursorsAt(CursorPosition(0, 2), CursorPosition(2, 0)))

    val after = RichTextReducer
      .reduce(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(1)), state)
      .state
      .persisted
      .buffers(bufferId)

    after.document.content.getLine(0) shouldBe Some("# alpha")
    after.document.content.getLine(2) shouldBe Some("⁤")
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 4), CursorPosition(2, 0))
  }
