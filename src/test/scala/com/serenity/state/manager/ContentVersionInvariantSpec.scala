package com.serenity.state.manager

import com.serenity.keystroke.events.*
import com.serenity.richtext.RichTextDocument
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.{AppState, Buffer, BufferId, CursorPosition, PaneId, RichTextState}
import com.serenity.state.undo.BufferSnapshot
import com.serenity.testkit.Generators
import org.scalacheck.{Gen, Shrink}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** A buffer's `contentVersion` names one text, so every cache stamped with it (rich text, outline, the change log) can
  * trust it; `prepareCommit` therefore rejects a commit that changes a buffer's content without advancing it.
  */
class ContentVersionInvariantSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance                                     = Balance.default
  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 200)
  given [A]: Shrink[A]                              = Shrink.shrinkAny

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)
  private val editor   = EditorPaneComponent(paneId)

  private val genKey: Gen[TextEntryEvent] = Gen.frequency(
    12 -> Gen.oneOf(Gen.alphaNumChar, Gen.const(' '), Gen.const('\n')).map(InsertChar(_)),
    2  -> Gen.const(DeleteBackward),
    2  -> Gen.const(DeleteForward),
    1  -> Gen.const(DeleteWordBackward),
    1  -> Gen.const(DeleteWordForward),
    1  -> Gen.const(NewLine),
    1  -> Gen.const(Enter),
    1  -> Gen.const(TabKey),
    1  -> Gen.const(ReverseTabKey),
    1  -> Gen.const(Cut),
    1  -> Gen.const(Copy),
    1  -> Gen.const(Paste),
    1  -> Gen.oneOf(DeleteToLineStart, DeleteToLineEnd),
    1  -> Gen.const(CutToDarlings),
    1  -> Gen.const(RestoreDarling),
    1  -> Gen.const(SelectAll),
    4  -> Gen.oneOf(MoveLeft, MoveRight, MoveWordLeft, MoveWordRight, MoveToStart, MoveToEnd),
    2  -> Gen.oneOf(ExtendSelectionLeft, ExtendSelectionRight, ExtendSelectionWordLeft, ExtendSelectionToLineEnd)
  )

  private def startingState(text: String): AppState =
    val state  = AppState.initial
    val buffer = Buffer.fromString(bufferId, text).copy(editing = state.persisted.buffers(bufferId).editing)
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))

  private def reduced(event: TextEntryEvent, state: AppState): AppState =
    editor.processEvent(event, state) match
      case ComponentResult.ReducerUpdate(result) => result.state
      case _                                     => state

  private def contentChanged(before: AppState, after: AppState): Boolean =
    before.persisted.buffers(bufferId).document.content ne after.persisted.buffers(bufferId).document.content

  private def versionOf(state: AppState): Long = state.persisted.buffers(bufferId).document.contentVersion

  property("a reducer event that changes a buffer's content advances its contentVersion") {
    forAll(Generators.genMultilineText, Gen.listOfN(40, genKey)) { (text, events) =>
      events.foldLeft(startingState(text)) { (before, event) =>
        val after = reduced(event, before)
        if contentChanged(before, after) then versionOf(after) should be > versionOf(before)
        after
      }
    }
  }

  property("every state a reducer event produces is accepted by prepareCommit over the state before it") {
    forAll(Generators.genMultilineText, Gen.listOfN(40, genKey)) { (text, events) =>
      events.foldLeft(startingState(text)) { (before, event) =>
        val after = reduced(event, before)
        StateManagerOperationBoundary.prepareCommit(after, before) shouldBe a[Right[?, ?]]
        after
      }
    }
  }

  property("prepareCommit rejects a buffer whose content changed under the same version") {
    val before = startingState("abc")
    val edited = before.persisted.buffers(bufferId)
    val stale  = edited.copy(document = edited.document.copy(content = Rope("abcd")))
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, stale)))

    StateManagerOperationBoundary.prepareCommit(after, before).left.map(_.mkString) match
      case Left(message) => message should include("contentVersion")
      case Right(_)      => fail("expected the commit to be rejected")
  }

  property("prepareCommit rejects a buffer whose version moved backwards with new content") {
    val before = startingState("abc")
    val edited = before.persisted.buffers(bufferId).withEditedContent(Rope("abcd"), List(CursorPosition(0, 4)))
    val ahead =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, edited)))
    val rewound = ahead.persisted.buffers(bufferId)
    val behind  = rewound.copy(document = rewound.document.copy(content = Rope("xyz"), contentVersion = 0L))

    StateManagerOperationBoundary
      .prepareCommit(
        ahead.copy(persisted = ahead.persisted.copy(buffers = ahead.persisted.buffers.updated(bufferId, behind))),
        ahead
      )
      .isLeft shouldBe true
  }

  property("prepareCommit accepts a buffer that is new to the commit, whatever its version") {
    val before = startingState("abc")
    val added  = Buffer.fromString(BufferId(7), "other")
    val after =
      before.copy(
        persisted = before.persisted.copy(
          buffers = before.persisted.buffers.updated(added.id, added),
          bufferOrder = before.persisted.bufferOrder :+ added.id
        ),
        runtime = before.runtime.copy(nextBufferId = BufferId(8))
      )

    StateManagerOperationBoundary.prepareCommit(after, before) shouldBe a[Right[?, ?]]
  }

  property("a restored session advances the version of every buffer it replaces") {
    val current = startingState("abc")
    val edited  = current.persisted.buffers(bufferId).withEditedContent(Rope("abcd"), List(CursorPosition(0, 4)))
    val running =
      current.copy(persisted = current.persisted.copy(buffers = current.persisted.buffers.updated(bufferId, edited)))
    val restored = startingState("from the session")

    val committed = StateManagerOperationBoundary.prepareCommit(
      SessionWorkflowTransitions.restoredIntoViewport(restored, running),
      running
    )

    committed.map(_.persisted.buffers(bufferId).document.content.collect()) shouldBe Right("from the session")
  }

  property("a restored session keeps a rich-text document in sync with the text it was restored with") {
    val running = startingState("abc")
    val edited  = running.persisted.buffers(bufferId).withEditedContent(Rope("abcd"), List(CursorPosition(0, 4)))
    val restoredBuffer = Buffer
      .fromString(bufferId, "from the session")
      .copy(richText = RichTextState().withSyncedDocument(Some(RichTextDocument.fromPlainText("from the session")), 0L))
    val restored = running.copy(persisted =
      running.persisted.copy(buffers = running.persisted.buffers.updated(bufferId, restoredBuffer))
    )
    val applied = SessionWorkflowTransitions.restoredIntoViewport(
      restored,
      running.copy(persisted = running.persisted.copy(buffers = running.persisted.buffers.updated(bufferId, edited)))
    )

    applied.persisted.buffers(bufferId).richTextInSync shouldBe true
  }

  private def withBuffer(state: AppState, buffer: Buffer): AppState =
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(buffer.id, buffer)))

  property("replacing a buffer's whole text advances its version and is accepted by prepareCommit") {
    val before   = startingState("abc")
    val replaced = EditorTransitions.bufferContentReplaced(before, bufferId, "formatted").map(_.state)

    replaced.map(versionOf) should contain(versionOf(before) + 1)
    replaced.map(StateManagerOperationBoundary.prepareCommit(_, before)) should matchPattern { case Some(Right(_)) => }
  }

  property("restoring an undo snapshot advances the version past the edit it undoes and is accepted by prepareCommit") {
    val clean  = startingState("abc").persisted.buffers(bufferId)
    val edited = clean.withEditedContent(Rope("abcd"), List(CursorPosition(0, 4)))
    val undone = BufferSnapshot.fromBuffer(clean).restoreInto(edited)

    undone.document.content.collect() shouldBe "abc"
    undone.document.contentVersion should be > edited.document.contentVersion
    StateManagerOperationBoundary
      .prepareCommit(withBuffer(startingState("abc"), undone), withBuffer(startingState("abc"), edited)) shouldBe a[
      Right[?, ?]
    ]
  }

  property("a reload whose disk read carries a lower version than the edited buffer is accepted by prepareCommit") {
    val path    = java.nio.file.Paths.get("/tmp/reload.txt")
    val clean   = startingState("abc").persisted.buffers(bufferId)
    val edited  = clean.withEditedContent(Rope("abcd"), List(CursorPosition(0, 4))).withEditedContent(Rope("abc"), Nil)
    val running = withBuffer(startingState("abc"), edited.copy(document = edited.document.copy(filePath = Some(path))))
    val disk    = Buffer.fromString(bufferId, "from disk")

    val reloaded =
      FileResults.reloaded(running, bufferId, path, edited.document.content, disk)

    reloaded.persisted.buffers(bufferId).document.contentVersion should be > edited.document.contentVersion
    StateManagerOperationBoundary.prepareCommit(reloaded, running) shouldBe a[Right[?, ?]]
  }

  property("a buffer whose text is settled by a save leaves no stale change log behind") {
    val buffer  = startingState("abc").persisted.buffers(bufferId)
    val settled = buffer.withSettledContent(Rope("abc\n"))

    settled.document.changes.head shouldBe settled.document.contentVersion
    settled.document.changesSince(buffer.document.contentVersion) shouldBe None
  }

end ContentVersionInvariantSpec
