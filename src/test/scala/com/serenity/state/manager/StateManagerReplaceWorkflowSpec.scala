package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.richtext.{InlineMark, RichTextDocument, RichTextPosition, RichTextRange}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[StateManagerReplaceWorkflow]] on its own (#1442), without a composed `StateManager`: the active editor
  * buffer lookup and the "tell the surface what to show" callback are plain closures, so each branch (no active buffer,
  * empty find text, no matches, a scope error, an actual replacement) can be checked directly rather than only
  * end-to-end via `ReplaceWorkflowStateManagerSpec`.
  */
class StateManagerReplaceWorkflowSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val surfaceId = SurfaceId("replace-workflow")
  private val bufferId  = BufferId(0)
  private val paneId    = PaneId(0)

  private def stateWith(workflow: ReplaceWorkflowState, content: String, hasActiveBuffer: Boolean = true): AppState =
    val buffer = Buffer.fromString(bufferId, content)
    val base   = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    base.copy(
      persisted = base.persisted.copy(
        layout =
          if hasActiveBuffer then base.persisted.layout
          else base.persisted.layout.copy(activeEditorPaneId = None)
      ),
      runtime = base.runtime.copy(uiSurfaces = List(surface))
    )

  final private class Harness(val modelRef: Ref[IO, Model], val workflow: StateManagerReplaceWorkflow):
    def currentState: AppState = modelRef.get.unsafeRunSync().app
    def undo: UndoState        = modelRef.get.unsafeRunSync().undo
    def lastSurfaceUpdate: ReplaceWorkflowState =
      workflow.replaceWorkflowSurface(currentState, surfaceId).map(_._2).getOrElse(fail("no replace prompt showing"))

  private def harness(initialState: AppState): Harness =
    val modelRef = Ref.of[IO, Model](Model(initialState, UndoState())).unsafeRunSync()
    def updateModelValidated(transition: Model => Option[Model]): IO[Unit] =
      modelRef.update(model =>
        transition(model).filter(next => AppStateValidation.validated(next.app).isRight).getOrElse(model)
      )
    new Harness(modelRef, new StateManagerReplaceWorkflow(updateModelValidated))

  "submitReplaceWorkflowEffect with no active buffer" should "report a status message and change nothing else" in {
    val before = stateWith(ReplaceWorkflowState(findText = "a"), "irrelevant", hasActiveBuffer = false)
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.lastSurfaceUpdate.statusMessage shouldBe Some("No active buffer")
    h.currentState.persisted.buffers(bufferId).document.content.collect() shouldBe "irrelevant"
  }

  it should "report a status message when find text is empty" in {
    val before = stateWith(ReplaceWorkflowState(findText = ""), "some text")
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.lastSurfaceUpdate.statusMessage shouldBe Some("Enter text to find")
  }

  it should "report a status message when there are no matches" in {
    val before = stateWith(ReplaceWorkflowState(findText = "missing"), "some text")
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.lastSurfaceUpdate.statusMessage shouldBe Some("No matches found")
  }

  it should "report a status message when Selection scope is chosen without an active selection" in {
    val before = stateWith(
      ReplaceWorkflowState(findText = "some", selectedScope = ReplaceWorkflowScope.Selection),
      "some text"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.lastSurfaceUpdate.statusMessage should not be None
    h.currentState.persisted.buffers(bufferId).document.content.collect() shouldBe "some text"
  }

  "ReplaceAll" should "replace every match, dismiss the surface, and record an undo entry" in {
    val before = stateWith(
      ReplaceWorkflowState(
        findText = "cat",
        replacementText = "dog",
        selectedAction = ReplaceWorkflowAction.ReplaceAll
      ),
      "cat sat, cat ran"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.currentState.persisted.buffers(bufferId).document.content.collect() shouldBe "dog sat, dog ran"
    h.currentState.runtime.uiSurfaces.exists(_.id == surfaceId) shouldBe false
    h.undo.undoStack should have size 1
    h.undo.undoStack.head shouldBe a[HistoryEntry.BufferEdit]
  }

  it should "move focus to the active editor pane after replacing" in {
    val before = stateWith(
      ReplaceWorkflowState(
        findText = "cat",
        replacementText = "dog",
        selectedAction = ReplaceWorkflowAction.ReplaceAll
      ),
      "cat"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.currentState.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  "ReplaceNext" should "replace only the first match at or after the cursor and keep the surface open" in {
    val before = stateWith(
      ReplaceWorkflowState(
        findText = "cat",
        replacementText = "dog",
        selectedAction = ReplaceWorkflowAction.ReplaceNext
      ),
      "cat sat, cat ran"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.currentState.persisted.buffers(bufferId).document.content.collect() shouldBe "dog sat, cat ran"
    h.currentState.runtime.uiSurfaces.exists(_.id == surfaceId) shouldBe true
    h.lastSurfaceUpdate.statusMessage shouldBe Some("Replaced next match")
  }

  it should "record an undo entry for the single replacement" in {
    val before = stateWith(
      ReplaceWorkflowState(
        findText = "cat",
        replacementText = "dog",
        selectedAction = ReplaceWorkflowAction.ReplaceNext
      ),
      "cat sat"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.undo.undoStack should have size 1
  }

  private val boldSat = RichTextRange(RichTextPosition(0, 4), RichTextPosition(0, 7))

  /** A replace prompt over "cat sat, cat ran" with "sat" in bold, its rich text in sync with the buffer. */
  private def formattedStateWith(action: ReplaceWorkflowAction): AppState =
    val workflow = ReplaceWorkflowState(findText = "cat", replacementText = "tiger", selectedAction = action)
    val state    = stateWith(workflow, "cat sat, cat ran")
    val buffer   = state.persisted.buffers(bufferId)
    val bold     = RichTextDocument.fromPlainText("cat sat, cat ran").toggleMark(boldSat, InlineMark.Bold)

    val formatted =
      buffer.copy(richText = buffer.richText.withSyncedDocument(Some(bold), buffer.document.contentVersion))
    state.copy(persisted = state.persisted.copy(buffers = Map(bufferId -> formatted)))

  private def boldText(buffer: Buffer): List[String] =
    buffer.richText.richTextDocument.toList
      .flatMap(_.paragraphs.flatMap(_.runs))
      .filter(_.style.marks.contains(InlineMark.Bold))
      .map(_.text)

  private def assertFormattingCarried(before: AppState, after: AppState, expectedText: String): Assertion =
    val previous = before.persisted.buffers(bufferId)
    val replaced = after.persisted.buffers(bufferId)
    replaced.document.content.collect() shouldBe expectedText
    replaced.document.contentVersion should be > previous.document.contentVersion
    replaced.richTextInSync shouldBe true
    replaced.richText.richTextDocument.map(_.plainText) shouldBe Some(expectedText)
    boldText(replaced) shouldBe List("sat")

  "ReplaceAll in a formatted buffer" should "carry its formatting onto the new text and advance contentVersion" in {
    val before = formattedStateWith(ReplaceWorkflowAction.ReplaceAll)
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    assertFormattingCarried(before, h.currentState, "tiger sat, tiger ran")
  }

  it should "restore the original formatting, in sync, when undone" in {
    val before = formattedStateWith(ReplaceWorkflowAction.ReplaceAll)
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()
    val undone = UndoRecording.undone(h.modelRef.get.unsafeRunSync()).getOrElse(fail("expected an undo step"))

    val restored = undone.app.persisted.buffers(bufferId)
    restored.document.content.collect() shouldBe "cat sat, cat ran"
    restored.richText.richTextDocument shouldBe before.persisted.buffers(bufferId).richText.richTextDocument
    restored.richTextInSync shouldBe true
  }

  "ReplaceNext in a formatted buffer" should "carry its formatting onto the new text and advance contentVersion" in {
    val before = formattedStateWith(ReplaceWorkflowAction.ReplaceNext)
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    assertFormattingCarried(before, h.currentState, "tiger sat, cat ran")
  }

  "submitReplaceWorkflowEffect when the surface isn't a replace-workflow surface" should "do nothing" in {
    val before = AppState.initial
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(SurfaceId("no-such-surface")).unsafeRunSync()

    h.currentState shouldBe before
  }

  "replaceWorkflowSurface" should "look up the workflow state for a live replace-workflow surface" in {
    val workflow = ReplaceWorkflowState(findText = "x")
    val state    = stateWith(workflow, "x")
    val h        = harness(state)

    h.workflow.replaceWorkflowSurface(state, surfaceId).map(_._2) shouldBe Some(workflow)
  }

  it should "report None for a surface id that isn't a replace-workflow surface" in {
    val state = AppState.initial
    val h     = harness(state)

    h.workflow.replaceWorkflowSurface(state, SurfaceId("missing")) shouldBe None
  }
