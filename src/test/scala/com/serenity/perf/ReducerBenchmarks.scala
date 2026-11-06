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
import com.serenity.ui.layout.{LayoutMemos, WrappedLineCache}
import com.serenity.ui.renderer.ShapedTextCache

/** Reducer benchmarks, so the per-family coverage #993 depends on is visible in one place. Builds its fixtures without
  * a Swing window, which lets a headless spec run exactly what the benchmark runner measures.
  */
private[perf] object ReducerBenchmarks:

  given Balance = Balance.default

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    lazy val editingState = editorState(largeFindDocument(matches = 12_000), None)
    lazy val cursoredState = editingState.copy(persisted =
      editingState.persisted.copy(buffers =
        editingState.persisted.buffers.view
          .mapValues(buffer => buffer.copy(editing = EditingState(List(CursorPosition(6_000, 12)))))
          .toMap
      )
    )
    lazy val plainScrollState = scrolledToDeepViewport(editorState(largeMultilineDocument(lines = 15_000), None))
    lazy val richScrollState = scrolledToDeepViewport(editorStateForRichDocument(largeRichTextDocument(lines = 15_000)))
    reducerBenchmarks(cursoredState, plainScrollState, richScrollState)

  private def reducerBenchmarks(
    editingStateOf: => AppState,
    plainScrollStateOf: => AppState,
    richScrollStateOf: => AppState
  ): List[BenchmarkRunner.Benchmark] =
    lazy val editingState        = editingStateOf
    lazy val plainScrollState    = plainScrollStateOf
    lazy val richScrollState     = richScrollStateOf
    lazy val normalEditingResult = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), editingState)
    lazy val backspaceResult     = EditorEventReducer.reduce(DeleteBackward, PaneId(0), editingState)
    lazy val wordDeleteResult    = EditorEventReducer.reduce(DeleteWordBackward, PaneId(0), editingState)
    lazy val moveRightResult     = EditorEventReducer.reduce(MoveRight, PaneId(0), editingState)
    lazy val extendRightResult   = EditorEventReducer.reduce(ExtendSelectionRight, PaneId(0), editingState)
    lazy val multiCursorState    = withCursorsOnConsecutiveLines(editingState, 50, fromLine = 5_000, column = 4)
    lazy val multiCursorWrapState = multiCursorState.copy(persisted =
      multiCursorState.persisted.copy(config = multiCursorState.persisted.config.withWordWrap(true))
    )
    // One shaper and one set of font memos across iterations, and no wrap remembered: every wrap is measured, as when no
    // cache is given, while the measurements underneath stay warm, as they were when they were process-wide.
    lazy val moveDownShaping     = ShapedTextCache()
    lazy val moveDownMemos       = LayoutMemos()
    lazy val multiInsertResult   = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), multiCursorState)
    lazy val multiMoveResult     = EditorEventReducer.reduce(MoveRight, PaneId(0), multiCursorState)
    lazy val multiMoveDownResult = com.serenity.VerticalNavSupport.dispatch(MoveDown, PaneId(0), multiCursorWrapState)
    lazy val plainScrollResult   = EditorEventReducer.reduce(ScrollDown(40), PaneId(0), plainScrollState)
    lazy val richScrollResult    = EditorEventReducer.reduce(ScrollDown(40), PaneId(0), richScrollState)
    lazy val originalLine = editingState.persisted.buffers.get(BufferId(1)).flatMap(_.document.content.getLine(6_000))
    lazy val expectedEditedLine     = originalLine.map(_.patch(12, "x", 0))
    lazy val expectedBackspacedLine = originalLine.map(_.patch(11, "", 1))

    def editedLine(result: ReducerResult): Option[String] =
      result.state.persisted.buffers.get(BufferId(1)).flatMap(_.document.content.getLine(6_000))

    def reducedBuffer(result: ReducerResult): Option[Buffer] =
      result.state.persisted.buffers.get(BufferId(1))

    def reducedCursor(result: ReducerResult): Option[CursorPosition] =
      reducedBuffer(result).flatMap(_.editing.cursorPositions.headOption)

    def reducedSelection(result: ReducerResult): Option[Selection] =
      reducedBuffer(result).flatMap(_.primarySelection)

    BenchmarkRunner.Benchmark.sharing(
      (
        editingState,
        plainScrollState,
        richScrollState,
        normalEditingResult,
        backspaceResult,
        wordDeleteResult,
        moveRightResult,
        extendRightResult,
        multiCursorState,
        multiCursorWrapState,
        moveDownShaping,
        moveDownMemos,
        multiInsertResult,
        multiMoveResult,
        multiMoveDownResult,
        plainScrollResult,
        richScrollResult,
        originalLine,
        expectedEditedLine,
        expectedBackspacedLine
      )
    )(
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
          () =>
            com.serenity.VerticalNavSupport
              .dispatch(
                MoveDown,
                PaneId(0),
                multiCursorWrapState,
                WrappedLineCache.measuring(moveDownShaping, moveDownMemos)
              )
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
    )

  private def reducedTopLine(result: ReducerResult): Option[Int] =
    result.state.persisted.buffers.get(BufferId(1)).map(_.viewport.topLine)

end ReducerBenchmarks
