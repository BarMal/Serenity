package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.io.ProjectFileListing
import com.serenity.keystroke.events.*
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, ModalStateReducer, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A big picker matches what is typed in the background rather than inside the reducer on the dispatcher (#1926). */
class PickerSearchSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val root = Path.of("/work/project")

  private def listing(count: Int): ProjectFileListing =
    ProjectFileListing((0 until count).map(index => Path.of(s"src/module$index/File$index.scala")).toVector, false)

  private def filled(count: Int): (AppState, SurfaceId) =
    val (state, id) = FileFinderTransitions.withFinderOpened(AppState.initial, root).getOrElse(fail("no finder"))
    FileFinderTransitions.withFilesListed(state, id, root, Right(listing(count))) -> id

  private def picker(state: AppState, id: SurfaceId): ListPicker =
    state.runtime.uiSurfaces
      .find(_.id == id)
      .collect { case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) => picker }
      .getOrElse(fail("no picker"))

  private def labels(state: AppState, id: SurfaceId): Vector[String] =
    picker(state, id).items.toOption.fold(Vector.empty)(_.items.map(_.label))

  private def typing(state: AppState, char: Char) =
    ModalEventReducer.reduce(ModalType.ListPicker, ModalInsertChar(char), state)

  private val bigList = ListPickerSearch.CheapFileCount + 1

  "A keystroke in a large file finder" should "show the typed text and ask for the match instead of making it" in {
    val (state, id) = filled(bigList)
    val before      = labels(state, id)

    val result = typing(state, '7')

    picker(result.state, id).queryText shouldBe "7"
    labels(result.state, id) shouldBe before
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.RefreshPicker(id, "7")))
  }

  it should "match a small file list inside the reducer, as before" in {
    val (state, id) = filled(ListPickerSearch.CheapFileCount)

    val result = typing(state, '7')

    result.effects shouldBe Nil
    labels(result.state, id).nonEmpty shouldBe true
    labels(result.state, id) should not be labels(state, id)
  }

  "A background match" should "replace the items of the picker still holding its query" in {
    val (state, id) = filled(bigList)
    val typed       = typing(typing(state, '1').state, '2').state

    val searched = PickerSearchTransitions.searched(typed, id, "12").getOrElse(fail("no result"))
    val applied  = PickerSearchTransitions.applied(typed, id, "12", searched)

    labels(applied.state, id) shouldBe searched.items.toOption.fold(Vector.empty)(_.items.map(_.label))
    labels(applied.state, id) should not be labels(typed, id)
    picker(applied.state, id).queryText shouldBe "12"
  }

  it should "be discarded once the query has moved on" in {
    val (state, id) = filled(bigList)
    val first       = typing(state, '1').state
    val stale       = PickerSearchTransitions.searched(first, id, "1").getOrElse(fail("no result"))
    val second      = typing(first, '2').state

    PickerSearchTransitions.applied(second, id, "1", stale).state shouldBe second
    EffectResult.applyIfCurrent(second, EffectResult.PickerSearched(id, "1", stale)) shouldBe second
    PickerSearchTransitions.searched(second, id, "1") shouldBe None
  }

  it should "be discarded once the picker has closed" in {
    val (state, id) = filled(bigList)
    val typed       = typing(state, '1').state
    val searched    = PickerSearchTransitions.searched(typed, id, "1").getOrElse(fail("no result"))
    val dismissed   = WorkflowSurfaces.dismissedToPriorFocus(typed, id)

    EffectResult.applyIfCurrent(dismissed, EffectResult.PickerSearched(id, "1", searched)) shouldBe dismissed
  }

  private def withBuffer(characters: Int): AppState =
    val base   = AppState.initial
    val buffer = Buffer.newEmpty(BufferId(0))
    val loaded = buffer.copy(document = buffer.document.copy(content = Rope("a" * characters)))
    val opened =
      base.copy(persisted = base.persisted.copy(buffers = Map(loaded.id -> loaded), bufferOrder = List(loaded.id)))
    ModalStateReducer.show(Modal.ListPicker(BufferTextSearch.picker), opened).state

  "A buffer text search" should "stay in the reducer while the open buffers are small" in {
    typing(withBuffer(10), 'a').effects shouldBe Nil
  }

  it should "move to the background once the open buffers are large" in {
    val state = withBuffer(ListPickerSearch.CheapTextLength + 1)
    val id    = state.modalSurface.map(_.id).getOrElse(fail("no picker"))

    typing(state, 'a').effects shouldBe List(AppEffect.Workflow(WorkflowEffect.RefreshPicker(id, "a")))
  }
