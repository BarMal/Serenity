package com.serenity.perf

import com.serenity.keystroke.events.InsertChar
import com.serenity.perf.BenchmarkFixtures.{
  editorState,
  editorStateForRichDocument,
  editorStateWithBuffers,
  largeMultilineDocument,
  largeRichTextDocument
}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, PaneId}
import com.serenity.state.reducers.EditorEventReducer

/** Heap the app keeps resident for a realistic editing session, reported as `footprint.*` rows beside the timings. Each
  * scenario builds the state the app would hold -- an `AppState`, undo history included -- and nothing else.
  *
  * Run just these with `sbt "Test / runMain com.serenity.perf.FootprintBenchmarks"`.
  */
private[perf] object FootprintBenchmarks:

  given Balance = Balance.default

  final case class Scenario(name: String, build: () => AnyRef)

  val DocumentLines: Int = 15_000
  val EditCount: Int     = 1_000
  val OpenBuffers: Int   = 10

  private val Trials = 3

  def scenarios: List[Scenario] =
    List(
      Scenario("footprint.empty_editor", () => AppState.initial),
      Scenario(
        "footprint.plain_document_15k_lines",
        () => editorState(largeMultilineDocument(DocumentLines), None)
      ),
      Scenario(
        "footprint.rich_document",
        () => editorStateForRichDocument(largeRichTextDocument(lines = DocumentLines))
      ),
      Scenario("footprint.after_1000_edits", () => afterEdits(EditCount)),
      Scenario(
        "footprint.ten_buffers",
        () => editorStateWithBuffers(OpenBuffers, largeMultilineDocument(DocumentLines))
      )
    )

  private def afterEdits(edits: Int): AppState =
    (1 to edits).foldLeft(editorState(largeMultilineDocument(DocumentLines), None))((state, _) =>
      EditorEventReducer.reduce(InsertChar('x'), PaneId(0), state).state
    )

  /** Single-entry memos such as `RopeCharacterSource.lastUsed` keep the last document an edit touched alive after its
    * state is dropped. Left alone, a trial's baseline would include the previous trial's document and subtract it from
    * the next one's footprint, so a tiny edit displaces them first.
    */
  private def displaceMemos(): Unit =
    val _ = (1 to 3).foldLeft(editorState("x", None))((state, _) =>
      EditorEventReducer.reduce(InsertChar('x'), PaneId(0), state).state
    )

  private def retainedBytes(scenario: Scenario): Long =
    HeapFootprint.retainedBytes(scenario.build, () => displaceMemos())

  /** The median of a few trials after one discarded build, which pays for lazily initialised static caches. */
  def measure(scenario: Scenario): BenchmarkRunner.BenchmarkResult =
    retainedBytes(scenario)
    val trials = List.fill(Trials)(retainedBytes(scenario)).sorted
    BenchmarkRunner.BenchmarkResult(
      name = scenario.name,
      iterations = 0,
      warmupInvocations = 0,
      batch = 0,
      minMs = 0.0,
      p50Ms = 0.0,
      p95Ms = 0.0,
      maxMs = 0.0,
      allocationP50Bytes = None,
      allocationP95Bytes = None,
      retainedBytes = trials.lift(Trials / 2)
    )

  /** The scenarios whose names start with any of `prefixes`, or all of them when there are none. */
  def runMatching(prefixes: List[String]): List[BenchmarkRunner.BenchmarkResult] =
    scenarios.filter(s => prefixes.isEmpty || prefixes.exists(s.name.startsWith)).map(measure)

  def main(args: Array[String]): Unit =
    BenchmarkRunner.printResults(runMatching(args.toList))

end FootprintBenchmarks
