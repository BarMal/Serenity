package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Leaf, Rope}
import com.serenity.state.manager.CursorViewport
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalFindReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def matchAt(line: Int, column: Int): FindResult =
    FindResult(line, column)

  // `Rope` is sealed, so a test double can no longer extend it directly; it delegates to a real `Leaf`/`Node` tree
  // while itself extending the still-open `Leaf` purely to satisfy the type system -- every method that matters for
  // this test forwards to `delegate` rather than using anything inherited from `Leaf`.
  final class NonCollectingRope(delegate: Rope) extends Leaf(delegate.collect()):
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override val newlineCount: Int =
      delegate.newlineCount

    override val lastLineLength: Int =
      delegate.lastLineLength

    override val endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def lineCount: Int =
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      delegate.getLine(lineIndex)

    override def lineColumnToOffset(line: Int, column: Int): Int =
      delegate.lineColumnToOffset(line, column)

    override def offsetToLineColumn(offset: Int): (Int, Int) =
      delegate.offsetToLineColumn(offset)

    override def collect(): String =
      throw AssertionError("find should not materialise the whole buffer")

  object NonCollectingRope:
    def apply(delegate: Rope): NonCollectingRope = new NonCollectingRope(delegate)

  private def stateWithFindModal(
    query: String,
    content: String,
    cursor: CursorPosition = CursorPosition(0, 0),
    viewport: Viewport = Viewport(0, 0, 24, 80)
  ): AppState =
    val bufferId = BufferId(0)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        focus = Focus.Surface(SurfaceId("find")),
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope(content)),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(cursor)),
              viewport = viewport
            )
        )
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("find"),
            SurfaceContent.ModalWorkflow(Modal.Find(query, Nil, 0)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def activeFindModal(state: AppState): Option[Modal] =
    state.modalSurface.flatMap {
      _.content match
        case SurfaceContent.ModalWorkflow(find @ Modal.Find(_, _, _)) => Some(find)
        case _                                                        => None
    }

  private def completeFind(state: AppState): AppState =
    activeFindModal(state) match
      case Some(Modal.Find(query, _, _)) =>
        val bufferId = BufferId(0)
        val content  = state.persisted.buffers(bufferId).document.content
        val reducedState = ModalEventReducer.applyFindSearchResults(
          state,
          FindSearchRequest(SurfaceId("find"), bufferId, query, content),
          FindSearch.results(content, query)
        )
        CursorViewport.ensureVisibleCursors(state, reducedState)
      case _ =>
        state

  "ModalEventReducer" should "apply clicked find results and replace controls through modal input events" in {
    val findSurface = UiSurface(
      SurfaceId("find"),
      SurfaceContent.ModalWorkflow(Modal.Find("needle", List(matchAt(0, 0), matchAt(1, 0)), 0)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val findModalBase = stateWithFindModal("needle", "needle\nneedle")
    val findState = findModalBase.copy(
      persisted = findModalBase.persisted.copy(focus = Focus.Surface(findSurface.id)),
      runtime = findModalBase.runtime.copy(uiSurfaces = List(findSurface))
    )
    val selectedFind = ModalEventReducer
      .reduce(ModalType.Find, ModalClick("find-result-1", Some("find-result-1")), findState)
      .state
    activeFindModal(selectedFind) shouldBe Some(Modal.Find("needle", List(matchAt(0, 0), matchAt(1, 0)), 1))

    val replaceSurface = UiSurface(
      SurfaceId("replace"),
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(ReplaceWorkflowState())),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val replaceState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(replaceSurface.id)),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(replaceSurface))
    )
    val selectedReplace = ModalEventReducer
      .reduce(ModalType.ReplaceWorkflow, ModalClick("replace-selection", Some("replace-selection")), replaceState)
      .state
    selectedReplace.modalSurface.flatMap(_.content match
      case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => Some(workflow.selectedScope)
      case _ => None) shouldBe Some(ReplaceWorkflowScope.Selection)

    val fileDialog = ModalDialog(
      SurfaceId("file"),
      Modal.FileWorkflow(
        FileWorkflowState(
          mode = FileWorkflowMode.Open,
          suggestions = List(FileWorkflowSuggestion("one"), FileWorkflowSuggestion("two"))
        )
      ),
      ModalPlacement.Centered
    )
    val fileState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Modal),
      runtime = AppState.initial.runtime.copy(modalStack = List(fileDialog))
    )
    val selectedFile = ModalEventReducer
      .reduce(ModalType.FileWorkflow, ModalClick("file-suggestion-1", Some("file-suggestion-1")), fileState)
      .state
    selectedFile.topModal.map(_.modal match
      case Modal.FileWorkflow(workflow) => workflow.selectedSuggestionIndex
      case other                        => fail(s"Expected file workflow, got $other")) shouldBe Some(1)
  }

  it should "keep find open and move the cursor to the first completed hit" in {
    val bufferId     = BufferId(0)
    val initialState = stateWithFindModal("needle", "x\nneedle here\ny\nneedle again")

    val updatedState = completeFind(initialState)

    activeFindModal(updatedState) shouldBe Some(Modal.Find("needle", List(matchAt(1, 0), matchAt(3, 0)), 0))
    updatedState.persisted.focus shouldBe Focus.Surface(SurfaceId("find"))
    updatedState.persisted.buffers(bufferId).findState shouldBe Some(
      FindState("needle", List(matchAt(1, 0), matchAt(3, 0)), 0)
    )
    updatedState.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(1, 0)
  }

  it should "leave buffer find state unchanged when an empty find query is submitted" in {
    val bufferId     = BufferId(0)
    val initialState = stateWithFindModal("", "alpha beta", CursorPosition(0, 5))

    val updatedState = ModalEventReducer.reduce(ModalType.Find, Enter, initialState).state

    activeFindModal(updatedState) shouldBe Some(Modal.Find("", Nil, 0))
    updatedState.persisted.buffers(bufferId).findState shouldBe None
    updatedState.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, 5)
  }

  it should "update the live find query without searching in the reducer" in {
    val bufferId     = BufferId(0)
    val initialState = stateWithFindModal("", "alpha needle beta\nneedle again")

    val withNeedle = "needle".foldLeft(initialState) { (state, char) =>
      ModalEventReducer.reduce(ModalType.Find, InsertChar(char), state).state
    }

    activeFindModal(withNeedle) shouldBe Some(Modal.Find("needle", Nil, 0))
    withNeedle.persisted.buffers(bufferId).findState shouldBe None
    withNeedle.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, 0)
    withNeedle.modalSurface shouldBe defined
  }

  it should "defer changed find queries and clear results that belong to the previous query" in {
    val bufferId = BufferId(0)
    val initialState = stateWithFindModal("need", "needle needle")
      .copy(
        persisted = stateWithFindModal("need", "needle needle").persisted.copy(
          buffers = AppState.initial.persisted.buffers.updated(
            bufferId,
            AppState.initial.persisted
              .buffers(bufferId)
              .copy(
                document = AppState.initial.persisted.buffers(bufferId).document.copy(content = Rope("needle needle")),
                findState = Some(FindState("need", List(matchAt(0, 0)), 0))
              )
          )
        )
      )

    val result = ModalEventReducer.reduce(ModalType.Find, InsertChar('l'), initialState)

    activeFindModal(result.state) shouldBe Some(Modal.Find("needl", Nil, 0))
    result.state.persisted.buffers(bufferId).findState shouldBe None
    result.effects should matchPattern {
      case List(AppEffect.Workflow(WorkflowEffect.RefreshFind(FindSearchRequest(_, `bufferId`, "needl", _)))) =>
    }
  }

  it should "ignore completed find results when the live query has changed" in {
    val initialState = stateWithFindModal("new", "new needle")
    val request = FindSearchRequest(
      SurfaceId("find"),
      BufferId(0),
      "old",
      initialState.persisted.buffers(BufferId(0)).document.content
    )

    ModalEventReducer.applyFindSearchResults(initialState, request, List(matchAt(0, 4))) shouldBe initialState
  }

  it should "ignore completed find results when the buffer content has changed" in {
    val initialState = stateWithFindModal("needle", "needle")
    val request = FindSearchRequest(
      SurfaceId("find"),
      BufferId(0),
      "needle",
      initialState.persisted.buffers(BufferId(0)).document.content
    )
    val editedState = initialState.copy(
      persisted = initialState.persisted.copy(
        buffers = initialState.persisted.buffers.updated(
          BufferId(0),
          initialState.persisted
            .buffers(BufferId(0))
            .copy(document = initialState.persisted.buffers(BufferId(0)).document.copy(content = Rope("other")))
        )
      )
    )

    ModalEventReducer.applyFindSearchResults(editedState, request, List(matchAt(0, 0))) shouldBe editedState
  }

  it should "update find queries without materialising the whole buffer" in {
    val bufferId = BufferId(0)
    val initialState = stateWithFindModal("", "alpha needle beta\nneedle again").copy(
      persisted = stateWithFindModal("", "alpha needle beta\nneedle again").persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(document =
              AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = NonCollectingRope(Rope("alpha needle beta\nneedle again")))
            )
        )
      )
    )

    val withNeedle = "needle".foldLeft(initialState) { (state, char) =>
      ModalEventReducer.reduce(ModalType.Find, InsertChar(char), state).state
    }

    activeFindModal(withNeedle) shouldBe Some(Modal.Find("needle", Nil, 0))
    withNeedle.persisted.buffers(bufferId).findState shouldBe None
    withNeedle.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, 0)
  }

  it should "ignore find queries that would split a grapheme cluster" in {
    val bufferId     = BufferId(0)
    val initialState = stateWithFindModal("", "cafe\u0301 needle", CursorPosition(0, 0))

    val withAccent = ModalEventReducer.reduce(ModalType.Find, InsertChar('\u0301'), initialState).state

    activeFindModal(withAccent) shouldBe Some(Modal.Find("\u0301", Nil, 0))
    withAccent.persisted.buffers(bufferId).findState shouldBe None
    withAccent.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, 0)
  }

  it should "navigate find results forward and backward while the overlay remains open" in {
    val bufferId     = BufferId(0)
    val initialState = completeFind(stateWithFindModal("needle", "needle one\nmiddle\nneedle two\nneedle three"))

    val second = ModalEventReducer.reduce(ModalType.Find, Enter, initialState).state
    val third  = ModalEventReducer.reduce(ModalType.Find, ModalNavigate(Direction.Down), second).state
    val secondAgain =
      ModalEventReducer.reduce(ModalType.Find, ModalNavigate(Direction.Up), third).state

    activeFindModal(second) shouldBe Some(Modal.Find("needle", List(matchAt(0, 0), matchAt(2, 0), matchAt(3, 0)), 1))
    second.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(2, 0)
    activeFindModal(third) shouldBe Some(Modal.Find("needle", List(matchAt(0, 0), matchAt(2, 0), matchAt(3, 0)), 2))
    third.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(3, 0)
    activeFindModal(secondAgain) shouldBe Some(
      Modal.Find("needle", List(matchAt(0, 0), matchAt(2, 0), matchAt(3, 0)), 1)
    )
    secondAgain.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(2, 0)
    secondAgain.modalSurface shouldBe defined
  }

  it should "navigate multiple find occurrences on the same line by column" in {
    val bufferId     = BufferId(0)
    val initialState = completeFind(stateWithFindModal("needle", "needle and needle"))

    val first  = initialState
    val second = ModalEventReducer.reduce(ModalType.Find, Enter, first).state

    activeFindModal(first) shouldBe Some(Modal.Find("needle", List(matchAt(0, 0), matchAt(0, "needle and ".length)), 0))
    first.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, 0)
    activeFindModal(second) shouldBe Some(
      Modal.Find("needle", List(matchAt(0, 0), matchAt(0, "needle and ".length)), 1)
    )
    second.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, "needle and ".length)
  }

  it should "track non-overlapping find results as distinct navigable matches" in {
    val bufferId     = BufferId(0)
    val initialState = completeFind(stateWithFindModal("aa", "aaaa"))

    val first  = initialState
    val second = ModalEventReducer.reduce(ModalType.Find, Enter, first).state

    val expectedResults = List(matchAt(0, 0), matchAt(0, 2))
    activeFindModal(first) shouldBe Some(Modal.Find("aa", expectedResults, 0))
    activeFindModal(second) shouldBe Some(Modal.Find("aa", expectedResults, 1))
    second.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, 2)
  }

  it should "advance find results when the explicit find-next event is submitted" in {
    val bufferId     = BufferId(0)
    val initialState = completeFind(stateWithFindModal("needle", "needle one\nneedle two\nneedle three"))

    val second = ModalEventReducer.reduce(ModalType.Find, ModalFindNext, initialState).state

    activeFindModal(second) shouldBe Some(Modal.Find("needle", List(matchAt(0, 0), matchAt(1, 0), matchAt(2, 0)), 1))
    second.persisted.buffers(bufferId).findState shouldBe Some(
      FindState("needle", List(matchAt(0, 0), matchAt(1, 0), matchAt(2, 0)), 1)
    )
    second.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(1, 0)
    second.persisted.focus shouldBe Focus.Surface(SurfaceId("find"))
  }

  it should "scroll wrapped text to the selected live find match visual row" in {
    val bufferId = BufferId(0)
    val prefix   = List.fill(80)("wrapped").mkString(" ")
    val content  = s"$prefix needle"
    val initialState = completeFind(
      stateWithFindModal("needle", content, viewport = Viewport(0, 0, visibleLines = 3, visibleColumns = 12))
    )

    val updatedState = initialState
    val buffer       = updatedState.persisted.buffers(bufferId)
    val cursor       = buffer.editing.cursors.head
    val font         = FontLoader.previewTextFont(updatedState.persisted.config.editorConfig.fontConfig)
    val wrapPx =
      TextLayoutSnapshot.gridWrapWidthPx(
        buffer.viewport.visibleColumns,
        updatedState.persisted.config.editorConfig.fontConfig
      )
    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, wrapPx, font)

    buffer.viewport.topLine shouldBe 0
    buffer.viewport.topVisualLine should be > 0
    withClue(
      s"viewport=${buffer.viewport} cursor=$cursor visualLines=${snapshot.visualLines.map(line => (line.startColumn, line.endColumn))}"
    ) {
      snapshot.visualLines.exists(line =>
        line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn
      ) shouldBe true
    }
  }

  it should "keep stale find state out when live query has no matches" in {
    val bufferId = BufferId(0)
    val initialState = stateWithFindModal("", "alpha beta")
      .copy(
        persisted = stateWithFindModal("", "alpha beta").persisted.copy(
          buffers = AppState.initial.persisted.buffers.updated(
            bufferId,
            AppState.initial.persisted
              .buffers(bufferId)
              .copy(
                document = AppState.initial.persisted
                  .buffers(bufferId)
                  .document
                  .copy(content = com.serenity.rope.Rope("alpha beta")),
                editing =
                  AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 5))),
                findState = Some(FindState("alpha", List(matchAt(0, 0)), 0))
              )
          )
        )
      )

    val noMatch = "zzz".foldLeft(initialState) { (state, char) =>
      ModalEventReducer.reduce(ModalType.Find, InsertChar(char), state).state
    }

    activeFindModal(noMatch) shouldBe Some(Modal.Find("zzz", Nil, 0))
    noMatch.persisted.buffers(bufferId).findState shouldBe None
    noMatch.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, 5)
  }

  it should "delete the previous word in find mode" in {
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("find"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("find"),
            SurfaceContent.ModalWorkflow(Modal.Find("alpha beta", Nil, 0)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val updatedState = ModalEventReducer.reduce(ModalType.Find, DeleteWordBackward, initialState).state

    updatedState.modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.Find("alpha ", Nil, 0)))
  }

  it should "leave an unchanged find query unsearched when delete-next-word has no text after it" in {
    val bufferId     = BufferId(0)
    val initialState = stateWithFindModal("alpha beta", "alpha beta\nalpha")

    val updatedState = ModalEventReducer.reduce(ModalType.Find, DeleteWordForward, initialState).state

    activeFindModal(updatedState) shouldBe Some(Modal.Find("alpha beta", Nil, 0))
    updatedState.persisted.buffers(bufferId).findState shouldBe None
    updatedState.persisted.buffers(bufferId).editing.cursors.head shouldBe CursorPosition(0, 0)
  }
