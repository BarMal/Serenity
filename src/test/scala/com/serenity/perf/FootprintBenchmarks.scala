package com.serenity.perf

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.keystroke.events.InsertChar
import com.serenity.perf.BenchmarkFixtures.{
  editorState,
  editorStateForRichDocument,
  editorStateWithBuffers,
  largeMultilineDocument,
  largeRichTextDocument
}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
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
      ),
      Scenario("footprint.prose_frame.stable_text", () => proseFrames(runtimes = 1, uniqueText = false)),
      Scenario("footprint.prose_frame.fresh_text", () => proseFrames(runtimes = 1, uniqueText = true)),
      Scenario("footprint.two_runtimes.prose_frame.stable_text", () => proseFrames(runtimes = 2, uniqueText = false))
    )

  /** The `prose_frame.*` rows draw one laptop-sized frame of word-wrapped prose per runtime and keep each runtime's
    * state and `RenderCaches`, which is what a drawn document leaves resident.
    *
    * `measure` discards its first build "to pay for static caches", so a process-wide cache is filled before the
    * baseline reading and never shows up in a retained figure: moving such a cache into state a runtime owns raises
    * `stable_text` and `two_runtimes.*` by what the cache holds (twice that for two runtimes, which stop sharing it).
    * `fresh_text` draws a document no earlier build drew, so a process-wide cache grows during the build and that
    * growth is counted as it would be once the cache is owned: the figure to compare across the move, where
    * `stable_text` shows the shift.
    */
  private def proseFrames(runtimes: Int, uniqueText: Boolean): AnyRef =
    val text = BenchmarkFixtures.loremIpsumProse(paragraphs = 300)
    val salted =
      if uniqueText then
        val salt = FreshTextSalt.incrementAndGet()
        text.linesIterator.map(line => if line.isEmpty then line else s"$line [$salt]").mkString("\n")
      else text
    List.fill(runtimes) {
      val state  = LaptopFrameBenchmarks.proseStateOf(salted)
      val caches = RenderCaches.create()
      val _      = LaptopFrameBenchmarks.renderedFrame(state, caches, LaptopFrameBenchmarks.frameImage())
      (state, caches)
    }

  private val FreshTextSalt = AtomicInteger(0)

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
