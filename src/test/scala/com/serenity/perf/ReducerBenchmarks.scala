package com.serenity.perf

import com.serenity.keystroke.events.{
  DeleteBackward,
  DeleteWordBackward,
  ExtendSelectionRight,
  InsertChar,
  MoveDown,
  MoveRight,
  ScrollDown
}
import com.serenity.perf.BenchmarkFixtures.{
  deepViewport,
  editorState,
  editorStateForRichDocument,
  largeFindDocument,
  largeMultilineDocument,
  largeRichTextDocument,
  scrolledToDeepViewport,
  withCursorsOnConsecutiveLines
}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{EditorEventReducer, ReducerResult}

/** Reducer benchmarks, so the per-family coverage #993 depends on is visible in one place. Builds its fixtures without
  * a Swing window, which lets a headless spec run exactly what the benchmark runner measures.
  */
private[perf] object ReducerBenchmarks:

  given Balance = Balance.default

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    val editingState = editorState(largeFindDocument(matches = 12_000), None)
    val cursoredState = editingState.copy(persisted =
      editingState.persisted.copy(buffers =
        editingState.persisted.buffers.view
          .mapValues(buffer => buffer.copy(editing = EditingState(List(CursorPosition(6_000, 12)))))
          .toMap
      )
    )
    val plainScrollState = scrolledToDeepViewport(editorState(largeMultilineDocument(lines = 15_000), None))
    val richScrollState  = scrolledToDeepViewport(editorStateForRichDocument(largeRichTextDocument(lines = 15_000)))
    reducerBenchmarks(cursoredState, plainScrollState, richScrollState)

  private def reducerBenchmarks(
    editingState: AppState,
    plainScrollState: AppState,
    richScrollState: AppState
  ): List[BenchmarkRunner.Benchmark] =
    val normalEditingResult = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), editingState)
    val backspaceResult     = EditorEventReducer.reduce(DeleteBackward, PaneId(0), editingState)
    val wordDeleteResult    = EditorEventReducer.reduce(DeleteWordBackward, PaneId(0), editingState)
    val moveRightResult     = EditorEventReducer.reduce(MoveRight, PaneId(0), editingState)
    val extendRightResult   = EditorEventReducer.reduce(ExtendSelectionRight, PaneId(0), editingState)
    val multiCursorState    = withCursorsOnConsecutiveLines(editingState, 50, fromLine = 5_000, column = 4)
    val multiCursorWrapState = multiCursorState.copy(persisted =
      multiCursorState.persisted.copy(config = multiCursorState.persisted.config.withWordWrap(true))
    )
    val multiInsertResult   = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), multiCursorState)
    val multiMoveResult     = EditorEventReducer.reduce(MoveRight, PaneId(0), multiCursorState)
    val multiMoveDownResult = com.serenity.VerticalNavSupport.dispatch(MoveDown, PaneId(0), multiCursorWrapState)
    val plainScrollResult   = EditorEventReducer.reduce(ScrollDown(40), PaneId(0), plainScrollState)
    val richScrollResult    = EditorEventReducer.reduce(ScrollDown(40), PaneId(0), richScrollState)
    val originalLine        = editingState.persisted.buffers.get(BufferId(1)).flatMap(_.document.content.getLine(6_000))
    val expectedEditedLine  = originalLine.map(_.patch(12, "x", 0))
    val expectedBackspacedLine = originalLine.map(_.patch(11, "", 1))

    def editedLine(result: ReducerResult): Option[String] =
      result.state.persisted.buffers.get(BufferId(1)).flatMap(_.document.content.getLine(6_000))

    def reducedBuffer(result: ReducerResult): Option[Buffer] =
      result.state.persisted.buffers.get(BufferId(1))

    def reducedCursor(result: ReducerResult): Option[CursorPosition] =
      reducedBuffer(result).flatMap(_.editing.cursorPositions.headOption)

    def reducedSelection(result: ReducerResult): Option[Selection] =
      reducedBuffer(result).flatMap(_.primarySelection)

    List(
      BenchmarkRunner.Benchmark(
        "reducer.normal_editing",
        3,
        BenchmarkIterationCounts.Reducer,
        () =>
          assert(
            expectedEditedLine.exists(line => editedLine(normalEditingResult).contains(line))
          ),
        () => EditorEventReducer.reduce(InsertChar('x'), PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.backspace",
        3,
        BenchmarkIterationCounts.Reducer,
        () =>
          assert(
            expectedBackspacedLine.exists(line => editedLine(backspaceResult).contains(line))
          ),
        () => EditorEventReducer.reduce(DeleteBackward, PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.delete_word_backward",
        3,
        BenchmarkIterationCounts.Reducer,
        () =>
          assert(
            editedLine(wordDeleteResult).exists(_.length < expectedBackspacedLine.fold(0)(_.length))
          ),
        () => EditorEventReducer.reduce(DeleteWordBackward, PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.arrow_navigation",
        3,
        BenchmarkIterationCounts.Reducer,
        () => assert(reducedCursor(moveRightResult).exists(_.column == 13)),
        () => EditorEventReducer.reduce(MoveRight, PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.extend_selection",
        3,
        BenchmarkIterationCounts.Reducer,
        () => assert(reducedSelection(extendRightResult).exists(_.focus.column == 13)),
        () => EditorEventReducer.reduce(ExtendSelectionRight, PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.multi_cursor_insert",
        3,
        BenchmarkIterationCounts.Reducer,
        () => assert(reducedBuffer(multiInsertResult).exists(_.editing.cursors.size == 50)),
        () => EditorEventReducer.reduce(InsertChar('x'), PaneId(0), multiCursorState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.multi_cursor_move",
        3,
        BenchmarkIterationCounts.Reducer,
        () => assert(reducedBuffer(multiMoveResult).exists(_.editing.cursorPositions.forall(_.column == 5))),
        () => EditorEventReducer.reduce(MoveRight, PaneId(0), multiCursorState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.multi_cursor_move_down",
        3,
        BenchmarkIterationCounts.Reducer,
        () => assert(reducedBuffer(multiMoveDownResult).exists(_.editing.cursors.size == 50)),
        () => com.serenity.VerticalNavSupport.dispatch(MoveDown, PaneId(0), multiCursorWrapState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.deep_scroll.plain",
        3,
        BenchmarkIterationCounts.Reducer,
        () => assert(reducedTopLine(plainScrollResult) == Some(deepViewport.topLine + 40)),
        () => EditorEventReducer.reduce(ScrollDown(40), PaneId(0), plainScrollState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.deep_scroll.rich_text",
        3,
        BenchmarkIterationCounts.Reducer,
        () => assert(reducedTopLine(richScrollResult) == Some(deepViewport.topLine + 40)),
        () => EditorEventReducer.reduce(ScrollDown(40), PaneId(0), richScrollState)
      )
    )

  private def reducedTopLine(result: ReducerResult): Option[Int] =
    result.state.persisted.buffers.get(BufferId(1)).map(_.viewport.topLine)

end ReducerBenchmarks
