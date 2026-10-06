package com.serenity.perf

import com.serenity.keystroke.events.InsertChar
import com.serenity.perf.BenchmarkFixtures.{editorState, largeFindDocument}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalEventReducer
import com.serenity.ui.widget.TextField

private[perf] object FindReplaceBenchmarks:

  given Balance = Balance.default

  def benchmarks(): List[BenchmarkRunner.Benchmark] = resultSetBenchmarks() ++ queryBenchmarks()

  private def resultSetBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val frequentTermDocument = Rope("the " * 500_000)
    val frequentTermMatches  = FindSearch.search(frequentTermDocument, "the", FindOptions.default, anchor = 0)
    val findResultSet = FindResultSet.normalized(
      "needle",
      (0 until 12_000).toVector.map(line => FindResult(line, 10)),
      requestedIndex = 6_000
    )
    val visibleFindResults = findResultSet.visibleResults(maxResults = 80)

    List(
      BenchmarkRunner.Benchmark(
        "find_replace.large_result_set",
        3,
        20,
        () => assert(visibleFindResults.size == 80 && visibleFindResults.exists(_._1 == FindResult(6_000, 10))),
        () => findResultSet.visibleResults(maxResults = 80)
      ),
      // A term in every word of a 2 MB document: the search stops at the match cap instead of collecting them all.
      BenchmarkRunner.Benchmark(
        "find_replace.frequent_term_search",
        3,
        20,
        () => assert(frequentTermMatches.capped && frequentTermMatches.results.length == FindSearch.MatchLimit),
        () => FindSearch.search(frequentTermDocument, "the", FindOptions.default, anchor = 0)
      )
    )

  private def queryBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val findState          = editorState(largeFindDocument(matches = 12_000), None)
    val findQuerySurfaceId = SurfaceId("benchmark-find")
    val findQueryState = findState.copy(
      persisted = findState.persisted.copy(focus = Focus.Surface(findQuerySurfaceId)),
      runtime = findState.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            findQuerySurfaceId,
            SurfaceContent.ModalWorkflow(Modal.Find(TextField.of("needle"), Vector.empty, 0)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val findQueryRequest = FindSearchRequest(
      findQuerySurfaceId,
      BufferId(1),
      "needle",
      findQueryState.persisted.buffers(BufferId(1)).document.content
    )
    def landFindQuery(): AppState =
      val matches = FindSearch.search(
        findQueryRequest.content,
        findQueryRequest.query,
        findQueryRequest.options,
        findQueryRequest.anchor
      )
      ModalEventReducer.applyFindSearchResults(findQueryState, findQueryRequest, matches.results, matches.capped)
    val completeFindQuery = landFindQuery()
    val findKeystrokeState = findQueryState.copy(runtime =
      findQueryState.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            findQuerySurfaceId,
            SurfaceContent.ModalWorkflow(Modal.Find(TextField.of("needl"), Vector.empty, 0)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val findKeystrokeResult = ModalEventReducer.reduce(ModalType.Find, InsertChar('e'), findKeystrokeState)

    List(
      BenchmarkRunner.Benchmark(
        "find_replace.large_query_update",
        3,
        20,
        () =>
          assert(
            completeFindQuery.persisted
              .buffers(BufferId(1))
              .findState
              .exists(found => found.results.length == FindSearch.MatchLimit && found.capped)
          ),
        () => landFindQuery()
      ),
      BenchmarkRunner.Benchmark(
        "find_replace.large_query_keystroke",
        3,
        20,
        () => assert(findKeystrokeResult.effects.nonEmpty),
        () => ModalEventReducer.reduce(ModalType.Find, InsertChar('e'), findKeystrokeState)
      )
    )

end FindReplaceBenchmarks
