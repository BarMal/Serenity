package com.serenity.perf

import com.serenity.keystroke.events.InsertChar
import com.serenity.perf.BenchmarkFixtures.{
  editorState,
  largeFindDocument,
  largeMultilineDocument,
  withCursorsOnConsecutiveLines
}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer

/** What an edit costs when it carries many cursors or annotations (#1916), and what converting many offsets to line and
  * column costs (#1945). Needs no window: `sbt "Test/runMain com.serenity.perf.EditRemapBenchmarks"`.
  */
private[perf] object EditRemapBenchmarks:

  given Balance = Balance.default

  private val Lines       = 20_000
  private val Cursors     = 100
  private val Annotated   = 5_000
  private val FindMatches = 10_000

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    List(multiCursorTyping(), typingWithAnnotations(), findWithManyMatches(), offsetConversions())

  def main(args: Array[String]): Unit =
    BenchmarkRunner.printResults(BenchmarkRunner.runMatching(args.toList, benchmarks()))

  private def document: AppState = editorState(largeMultilineDocument(Lines), None)

  private def typed(state: AppState): AppState = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), state).state

  private def buffer(state: AppState): Buffer = state.persisted.buffers(BufferId(1))

  private def multiCursorTyping(): BenchmarkRunner.Benchmark =
    val state = withCursorsOnConsecutiveLines(document, count = Cursors, fromLine = 5_000, column = 4)
    BenchmarkRunner.Benchmark(
      "edit_remap.multi_cursor_100",
      3,
      BenchmarkIterationCounts.Reducer,
      () => assert(buffer(typed(state)).editing.cursors.size == Cursors),
      () => typed(state)
    )

  private def typingWithAnnotations(): BenchmarkRunner.Benchmark =
    val base      = withCursorsOnConsecutiveLines(document, count = 1, fromLine = 10_000, column = 4)
    val positions = (0 until Annotated).toList.map(index => CursorPosition(index * 4 % Lines, index % 20))
    val state = base.copy(persisted =
      base.persisted.copy(buffers =
        base.persisted.buffers.updated(
          BufferId(1),
          buffer(base).copy(annotations =
            buffer(base).annotations.copy(
              bookmarks = positions,
              placeholders = positions.map(Placeholder(_, "to write")),
              documentComments = positions.map(position => DocumentComment(position, position.moveRight, "note"))
            )
          )
        )
      )
    )
    BenchmarkRunner.Benchmark(
      "edit_remap.typing_5k_annotations",
      3,
      BenchmarkIterationCounts.Reducer,
      () => assert(buffer(typed(state)).annotations.placeholders.size == Annotated),
      () => typed(state)
    )

  private def findWithManyMatches(): BenchmarkRunner.Benchmark =
    val content = Rope(largeFindDocument(matches = 12_000))
    BenchmarkRunner.Benchmark(
      "edit_remap.find_10k_matches",
      3,
      20,
      () =>
        assert(FindSearch.search(content, "needle", FindOptions.default, 0, FindMatches).results.size == FindMatches),
      () => FindSearch.search(content, "needle", FindOptions.default, 0, FindMatches)
    )

  private def offsetConversions(): BenchmarkRunner.Benchmark =
    val content = Rope(largeMultilineDocument(Lines))
    val offsets = (0 until 10_000).map(index => index * (content.weight / 10_000))
    BenchmarkRunner.Benchmark(
      "edit_remap.offsets_to_line_column_10k",
      3,
      20,
      () => assert(offsets.map(content.offsetToLineColumn).last._1 > 0),
      () => offsets.map(content.offsetToLineColumn)
    )

end EditRemapBenchmarks
