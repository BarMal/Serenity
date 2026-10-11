package com.serenity.state.manager

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.text.AllocationProbe
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Replace is an ordinary edit (#1960): the buffer's document carries the edit as a `ChangeSet`, so the consumers that
  * follow edits (LSP incremental sync, diagnostics, spell check) need not diff two texts, replace-all is one version
  * and one undo step, and no replace walks the whole document more than once.
  */
class ReplaceAsOrdinaryEditSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val surfaceId = SurfaceId("replace-workflow")
  private val bufferId  = BufferId(0)

  private def modelWith(
    content: Rope,
    workflow: ReplaceWorkflowState,
    editing: EditingState = EditingState.fromCursors(List(Cursor(CursorPosition(0, 0)))),
    annotations: Annotations = Annotations()
  ): Model =
    val buffer = Buffer(bufferId, Document(content = content), editing = editing, annotations = annotations)
    val base   = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    Model(base.copy(runtime = base.runtime.copy(uiSurfaces = List(surface))), UndoState())

  private def submitted(model: Model): Model = ReplaceWorkflowTransitions.submitted(model, surfaceId).value

  private def bufferOf(model: Model): Buffer = model.app.persisted.buffers(bufferId)

  private def workflow(action: ReplaceWorkflowAction, findText: String = "cat", replacement: String = "tiger") =
    ReplaceWorkflowState(findText = findText, replacementText = replacement, selectedAction = action)

  private val text = "cat sat, dog ran, cat ate"

  private def changedText(before: Buffer, after: Buffer): Option[String] =
    after.document.changeFrom(before.document).flatMap(_.applyTo(before.document.content)).map(_.collect())

  "Replace-next" should "hand its edit on as a ChangeSet from the old text to the new" in {
    val before = modelWith(Rope(text), workflow(ReplaceWorkflowAction.ReplaceNext))

    val after = submitted(before)

    changedText(bufferOf(before), bufferOf(after)) shouldBe Some("tiger sat, dog ran, cat ate")
    bufferOf(after).document.contentVersion shouldBe bufferOf(before).document.contentVersion + 1
  }

  it should "move bookmarks, comments and placeholders after the match by the length delta" in {
    val annotations = Annotations(
      bookmarks = List(CursorPosition(0, 9)),
      documentComments = List(DocumentComment(CursorPosition(0, 9), CursorPosition(0, 12), "note")),
      placeholders = List(Placeholder(CursorPosition(0, 13), "p"))
    )
    val before = modelWith(Rope(text), workflow(ReplaceWorkflowAction.ReplaceNext), annotations = annotations)

    val moved = bufferOf(submitted(before)).annotations

    moved.bookmarks shouldBe List(CursorPosition(0, 11))
    moved.documentComments.map(comment => (comment.anchor, comment.focus)) shouldBe
      List(CursorPosition(0, 11) -> CursorPosition(0, 14))
    moved.placeholders.map(_.position) shouldBe List(CursorPosition(0, 15))
  }

  it should "leave the find results at the matches that remain, past the replacement" in {
    val before = modelWith(Rope(text), workflow(ReplaceWorkflowAction.ReplaceNext))

    val results = bufferOf(submitted(before)).findState.value.results

    results.map(result => (result.line, result.column)) shouldBe Vector((0, 20))
  }

  it should "find a match that the replacement itself creates" in {
    val before = modelWith(Rope("aXb aXb"), workflow(ReplaceWorkflowAction.ReplaceNext, "aXb", "aXbaXb"))

    val results = bufferOf(submitted(before)).findState.value.results

    results.map(result => (result.line, result.column)) shouldBe Vector((0, 0), (0, 3), (0, 7))
  }

  it should "keep a selection-scoped selection around the replaced text" in {
    val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 12))
    val before = modelWith(
      Rope(text),
      workflow(ReplaceWorkflowAction.ReplaceNext).copy(selectedScope = ReplaceWorkflowScope.Selection),
      editing = EditingState.fromCursors(List(Cursor(selection)))
    )

    bufferOf(submitted(before)).primarySelection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(0, 14)))
  }

  "Replace-all" should "hand all its replacements on as one ChangeSet and one content version" in {
    val before = modelWith(Rope(text), workflow(ReplaceWorkflowAction.ReplaceAll))

    val after = submitted(before)

    changedText(bufferOf(before), bufferOf(after)) shouldBe Some("tiger sat, dog ran, tiger ate")
    bufferOf(after).document.contentVersion shouldBe bufferOf(before).document.contentVersion + 1
  }

  it should "move annotations after the replacements by their combined length delta" in {
    val annotations = Annotations(
      bookmarks = List(CursorPosition(0, 9), CursorPosition(0, 22)),
      placeholders = List(Placeholder(CursorPosition(0, 25), "end"))
    )
    val before = modelWith(Rope(text), workflow(ReplaceWorkflowAction.ReplaceAll), annotations = annotations)

    val moved = bufferOf(submitted(before)).annotations

    moved.bookmarks shouldBe List(CursorPosition(0, 11), CursorPosition(0, 26))
    moved.placeholders.map(_.position) shouldBe List(CursorPosition(0, 29))
  }

  it should "be undone in one step" in {
    val before = modelWith(Rope(text), workflow(ReplaceWorkflowAction.ReplaceAll))

    val after  = submitted(before)
    val undone = UndoRecording.undone(after).value

    after.undo.undoStack should have size 1
    bufferOf(undone).document.content.collect() shouldBe text
  }

  private val largeContent =
    Rope((1 to 60000).map(line => s"line $line lorem ipsum dolor sit amet").mkString("\n"))

  private val searchedTerm = "line 45000 "

  private def allocatedBy(model: Model): Long =
    AllocationProbe.allocatedBytes(submitted(model)).getOrElse(cancel("per-thread allocation is not supported"))

  private def allocatedBySearch: Long =
    AllocationProbe.allocatedBytes(largeContent.searchAll(searchedTerm)).getOrElse(cancel("not supported"))

  "Replace-next on a large document" should "search the document before and after the edit, not once per refreshed result" in {
    val model = modelWith(largeContent, workflow(ReplaceWorkflowAction.ReplaceNext, searchedTerm, "row 45000 "))

    allocatedBy(model) should be < 5 * allocatedBySearch / 2
  }

  it should "scope to a selection without copying the document to a String" in {
    val selection = Selection(CursorPosition(44000, 0), CursorPosition(46000, 0))
    val model = modelWith(
      largeContent,
      workflow(ReplaceWorkflowAction.ReplaceNext, searchedTerm, "row 45000 ")
        .copy(selectedScope = ReplaceWorkflowScope.Selection),
      editing = EditingState.fromCursors(List(Cursor(selection)))
    )

    allocatedBy(model) should be < 5 * allocatedBySearch / 2
  }
