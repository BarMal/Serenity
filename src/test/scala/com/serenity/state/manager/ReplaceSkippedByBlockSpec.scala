package com.serenity.state.manager

import com.serenity.richtext.{DocumentFeature, RichTextDocument, RichTextParagraph}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A replacement that would join a block line is skipped (#1896); everything that follows the text -- the cursor and
  * the bookmarks, comments and placeholders -- must move as if only the other replacements had been made.
  */
class ReplaceSkippedByBlockSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val surfaceId = SurfaceId("replace-workflow")
  private val bufferId  = BufferId(0)
  private val paneId    = PaneId(0)

  // "alpha\nbeta\n<block>\ngamma\ndelta": replacing each newline with a space joins alpha+beta and gamma+delta, while
  // the newlines on either side of the block would put text beside it.
  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("alpha"),
      RichTextParagraph.plain("beta"),
      RichTextParagraph.block("<w:tbl/>", DocumentFeature.Tables),
      RichTextParagraph.plain("gamma"),
      RichTextParagraph.plain("delta")
    )
  )

  private val annotations = Annotations(
    bookmarks = List(CursorPosition(1, 2), CursorPosition(4, 2)),
    documentComments = List(DocumentComment(CursorPosition(3, 1), CursorPosition(4, 3), "note")),
    placeholders = List(Placeholder(CursorPosition(4, 0), "write more"))
  )

  private def modelWith(workflow: ReplaceWorkflowState, cursor: CursorPosition = CursorPosition(0, 0)): Model =
    val buffer = Buffer(
      id = bufferId,
      document = Document(content = Rope(document.plainText)),
      editing = EditingState.fromCursors(List(Cursor(cursor))),
      richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 0L),
      annotations = annotations
    )
    val base = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    Model(base.copy(runtime = base.runtime.copy(uiSurfaces = List(surface))), UndoState())

  private def replaced(workflow: ReplaceWorkflowState): Buffer =
    ReplaceWorkflowTransitions
      .submitted(modelWith(workflow), surfaceId)
      .value
      .app
      .persisted
      .buffers(bufferId)

  private val newlineToSpace = ReplaceWorkflowState(findText = "\n", replacementText = " ")

  "Replace-all with matches beside a block" should "replace the others and leave the block line alone" in {
    val after = replaced(newlineToSpace)

    after.document.content.getLine(0) shouldBe Some("alpha beta")
    after.document.content.getLine(1) shouldBe Some(com.serenity.richtext.InlineAtom.BlockCharacter.toString)
    after.document.content.getLine(2) shouldBe Some("gamma delta")
    after.richText.richTextDocument.value.paragraphs.map(_.isOpaqueBlock) shouldBe List(false, true, false)
  }

  it should "put the cursor after the last replacement that was made" in {
    replaced(newlineToSpace).editing.cursorPositions shouldBe List(CursorPosition(2, 6))
  }

  it should "move bookmarks, comments and placeholders by the replacements that were made only" in {
    val after = replaced(newlineToSpace).annotations

    after.bookmarks shouldBe List(CursorPosition(0, 8), CursorPosition(2, 8))
    after.documentComments.map(comment => (comment.anchor, comment.focus)) shouldBe
      List(CursorPosition(2, 1) -> CursorPosition(2, 9))
    after.placeholders.map(_.position) shouldBe List(CursorPosition(2, 6))
  }

  "Replace-next on a match beside a block" should "change nothing and step past the match" in {
    val model = modelWith(
      newlineToSpace.copy(selectedAction = ReplaceWorkflowAction.ReplaceNext),
      cursor = CursorPosition(1, 0)
    )

    val after = ReplaceWorkflowTransitions.submitted(model, surfaceId).value.app.persisted.buffers(bufferId)

    after.document.content.collect() shouldBe document.plainText
    after.richText.richTextDocument shouldBe Some(document)
    after.annotations shouldBe annotations
    after.editing.cursorPositions shouldBe List(CursorPosition(2, 0))
  }

  it should "move the annotations after a replacement that is made" in {
    val model = modelWith(
      newlineToSpace.copy(selectedAction = ReplaceWorkflowAction.ReplaceNext, replacementText = "--"),
      cursor = CursorPosition(0, 0)
    )

    val after = ReplaceWorkflowTransitions.submitted(model, surfaceId).value.app.persisted.buffers(bufferId)

    after.document.content.getLine(0) shouldBe Some("alpha--beta")
    after.annotations.bookmarks shouldBe List(CursorPosition(0, 9), CursorPosition(3, 2))
  }
