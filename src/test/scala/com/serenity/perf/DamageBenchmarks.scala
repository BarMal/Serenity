package com.serenity.perf

import com.serenity.config.RenderDamageGranularity
import com.serenity.keystroke.events.{InsertChar, ScrollDown}
import com.serenity.lsp.config.LanguageId
import com.serenity.perf.BenchmarkFixtures.{
  deepViewport,
  editorState,
  largeMarkdownDocument,
  largeMultilineDocument,
  withCursorsOnConsecutiveLines
}
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.{AppState, BufferId, Damage, PaneId}
import com.serenity.state.reducers.EditorEventReducer

/** #997's Rows-vs-Cells damage-computation comparison, across the five scenarios the issue names. This measures the
  * cost of computing and representing a `Damage` value under each `renderDamageGranularity` setting -- not paint cost,
  * since the `Cells` paint path that would consume the extra precision belongs to #999, not here. Kept apart from
  * [[PerformanceBenchmarks]] for the same reason [[BenchmarkFixtures]] is: a fixture edit and a measurement edit stay
  * separate.
  */
private[perf] object DamageBenchmarks:

  given Balance = Balance.default

  /** One before/after `AppState` pair per scenario. */
  final private case class DamageScenarios(
      longLine: (AppState, AppState),
      shortLine: (AppState, AppState),
      multiCursor: (AppState, AppState),
      markdown: (AppState, AppState),
      scroll: (AppState, AppState)
  )

  private def scenarios(): DamageScenarios =
    def editedContentPair(text: String, insertAt: Int, insertion: String): (AppState, AppState) =
      val before = editorState(text, Some(LanguageId.Scala))
      val buffer = before.persisted.buffers(BufferId(1))
      val after =
        before.copy(persisted =
          before.persisted.copy(buffers =
            before.persisted.buffers.updated(
              BufferId(1),
              buffer.copy(document =
                buffer.document.copy(content = buffer.document.content.insert(insertAt, insertion))
              )
            )
          )
        )
      (before, after)

    val multiCursorBefore = withCursorsOnConsecutiveLines(
      editorState(largeMultilineDocument(lines = 2_000), Some(LanguageId.Scala)),
      count = 50,
      fromLine = 500,
      column = 4
    )
    val multiCursorAfter = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), multiCursorBefore).state

    val markdownDoc = largeMarkdownDocument(sections = 200).mkString("\n")
    val (markdownBefore, markdownAfter) =
      editedContentPair(markdownDoc, insertAt = markdownDoc.length / 2, insertion = "y")
    def asMarkdown(state: AppState): AppState =
      state.copy(persisted =
        state.persisted.copy(buffers =
          state.persisted.buffers.view
            .mapValues(buffer => buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Markdown))))
            .toMap
        )
      )

    val scrollBase = editorState(largeMultilineDocument(lines = 20_000), Some(LanguageId.Scala))
    val scrollDeep = scrollBase.copy(persisted =
      scrollBase.persisted.copy(buffers =
        scrollBase.persisted.buffers.view.mapValues(_.copy(viewport = deepViewport)).toMap
      )
    )
    val scrollAfter = EditorEventReducer.reduce(ScrollDown(1), PaneId(0), scrollDeep).state

    DamageScenarios(
      longLine = editedContentPair("x" * 4_000, insertAt = 2_000, insertion = "y"),
      shortLine = editedContentPair("hi", insertAt = 1, insertion = "y"),
      multiCursor = (multiCursorBefore, multiCursorAfter),
      markdown = (asMarkdown(markdownBefore), asMarkdown(markdownAfter)),
      scroll = (scrollDeep, scrollAfter)
    )

  private def pair(
    name: String,
    before: AppState,
    after: AppState,
    verifyRows: Damage => Unit,
    verifyCells: Damage => Unit
  ): List[BenchmarkRunner.Benchmark] =
    def withGranularity(state: AppState, granularity: RenderDamageGranularity): AppState =
      state.copy(persisted =
        state.persisted.copy(config = state.persisted.config.withRenderDamageGranularity(granularity))
      )
    val rowsBefore  = withGranularity(before, RenderDamageGranularity.Rows)
    val rowsAfter   = withGranularity(after, RenderDamageGranularity.Rows)
    val cellsBefore = withGranularity(before, RenderDamageGranularity.Cells)
    val cellsAfter  = withGranularity(after, RenderDamageGranularity.Cells)
    List(
      BenchmarkRunner.Benchmark(
        s"damage.$name.rows",
        3,
        30,
        () => verifyRows(DamageProducer.forTransition(rowsBefore, rowsAfter)),
        () => DamageProducer.forTransition(rowsBefore, rowsAfter)
      ),
      BenchmarkRunner.Benchmark(
        s"damage.$name.cells",
        3,
        30,
        () => verifyCells(DamageProducer.forTransition(cellsBefore, cellsAfter)),
        () => DamageProducer.forTransition(cellsBefore, cellsAfter)
      )
    )

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    val s = scenarios()

    pair(
      "single_char_long_line",
      s.longLine._1,
      s.longLine._2,
      verifyRows = damage => assert(damage == Damage.BufferRows(BufferId(1), Set(0))),
      verifyCells = damage => assert(damage == Damage.BufferCells(BufferId(1), 0, 2_000, Some(2_001)))
    ) ++ pair(
      "single_char_short_line",
      s.shortLine._1,
      s.shortLine._2,
      verifyRows = damage => assert(damage == Damage.BufferRows(BufferId(1), Set(0))),
      verifyCells = damage => assert(damage == Damage.BufferCells(BufferId(1), 0, 1, Some(2)))
    ) ++ pair(
      "multi_cursor",
      s.multiCursor._1,
      s.multiCursor._2,
      // 50 single-character inserts on consecutive rows merge into one offset range spanning every row between them --
      // Cells degrades to BufferRows here because the merged range is not confined to a single row, exactly the
      // multi-row fallback #997 calls out.
      verifyRows = damage => assert(Damage.coarsenToRows(BufferId(1), damage).size >= 50),
      verifyCells = damage => assert(Damage.coarsenToRows(BufferId(1), damage).size >= 50)
    ) ++ pair(
      "markdown",
      s.markdown._1,
      s.markdown._2,
      // MarkdownSource uses the text font (measured layout), so Cells must fall back to row granularity regardless of
      // the setting -- this is the "must show Cells correctly degrading to row behaviour" scenario #997 asks for.
      verifyRows = damage => assert(damage.isInstanceOf[Damage.BufferRows]),
      verifyCells = damage => assert(damage.isInstanceOf[Damage.BufferRows])
    ) ++ pair(
      "scroll",
      s.scroll._1,
      s.scroll._2,
      // A scroll changes no buffer content, only the viewport -- DamageProducer does not yet report viewport damage
      // (a real gap, out of scope for this comparison), so both settings report Nothing and this scenario measures
      // only the reference-identity fast path's cost, which is what #997 predicts ("both should be equivalent").
      verifyRows = damage => assert(damage == Damage.Nothing),
      verifyCells = damage => assert(damage == Damage.Nothing)
    )

end DamageBenchmarks
