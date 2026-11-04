package com.serenity.perf

import java.nio.file.{Files, Path}

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import com.serenity.TestTemp
import com.serenity.lsp.client.LspFramer
import com.serenity.perf.BenchmarkFixtures.largeSingleLineJson
import com.serenity.project.{ProjectTaskDetector, ProjectTaskKind, ProjectTaskTerminal}
import com.serenity.rope.{Balance, Rope}
import com.serenity.ui.terminal.SwingWindow
import io.circe.Json

object PerformanceBenchmarks:

  given Balance = Balance.default

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      val (shard, names) = BenchmarkShard.fromArgs(args.toList).fold(error => sys.error(error), identity)
      val session        = WarmupExtension.Session.standard()
      val results = withSuite { all =>
        val selected = shard.fold(all)(_.select(all))
        val timed    = BenchmarkRunner.runMatching(names, selected, session)
        // The retained-heap rows belong to the first shard only, so they are measured once.
        if shard.forall(_.index == 1) then timed ++ FootprintBenchmarks.runMatching(names) else timed
      }
      BenchmarkRunner.printResults(results)

  /** The suite's names, in the order it runs. Declaring a benchmark builds no window and no fixture, so this needs
    * neither.
    */
  private[perf] def names: List[String] =
    import cats.effect.unsafe.implicits.global

    suite(
      unavailable("the benchmark window"),
      unavailable("the present window"),
      unavailable("the project task fixture")
    ).map(_.name)

  private def unavailable[A](what: String): A = sys.error(s"$what exists only while the suite runs")

  /** Hands the whole suite, in the order it runs, to `use` inside the windows it needs. Each benchmark builds its
    * fixture when it is run.
    */
  private[perf] def withSuite[A](use: List[BenchmarkRunner.Benchmark] => A): A =
    import cats.effect.unsafe.implicits.global

    SwingWindow
      .resource(metrics = RenderBenchmarks.cellMetrics, chromeMetrics = RenderBenchmarks.uiMetrics)
      .flatMap(window =>
        LaptopFrameBenchmarks
          .presentWindowResource(RenderBenchmarks.cellMetrics, RenderBenchmarks.uiMetrics)
          .map(window -> _)
      )
      .flatMap(windows => projectTaskFixtureResource.map(projectRoot => windows -> projectRoot))
      .use { case ((window, presentWindow), projectRoot) => IO(use(suite(window, presentWindow, projectRoot))) }
      .unsafeRunSync()

  private def suite(window: => SwingWindow, presentWindow: => SwingWindow, projectRoot: => Path)(using
    IORuntime
  ): List[BenchmarkRunner.Benchmark] =
    val all = benchmarks(window, projectRoot) ++ LaptopFrameBenchmarks.benchmarks(presentWindow) ++
      CacheOwnershipBenchmarks.benchmarks ++ RichDocumentOpenBenchmarks.benchmarks
    val unknown = BenchmarkIterationCounts.JitSettled -- all.map(_.name)
    require(unknown.isEmpty, s"JitSettled names no benchmark: ${unknown.mkString(", ")}")
    all

  private def benchmarks(cursorWindow: => SwingWindow, projectRoot: => Path): List[BenchmarkRunner.Benchmark] =
    ropeBenchmarks() ++
      RenderBenchmarks.frameBenchmarks(cursorWindow) ++
      LayoutSnapshotBenchmarks.benchmarks() ++
      CellGridBenchmarks.benchmarks() ++
      ReducerBenchmarks.benchmarks() ++
      ProseTypingBenchmarks.benchmarks() ++
      DamageBenchmarks.benchmarks() ++
      TuiBenchmarks.benchmarks() ++
      CommandRunnerBenchmarks.benchmarks() ++
      ModalWrapBenchmarks.benchmarks() ++
      EditorConfigBenchmarks.benchmarks() ++
      EqualsBenchmarks.benchmarks() ++
      SourceRowCountBenchmarks.benchmarks() ++
      FindReplaceBenchmarks.benchmarks() ++
      BoundaryCacheBenchmarks.benchmarks() ++
      RowCountBenchmarks.benchmarks() ++
      lspAndProjectBenchmarks(projectRoot) ++
      RenderBenchmarks.markdownBenchmarks()

  private[perf] def ropeBenchmarks(): List[BenchmarkRunner.Benchmark] =
    lazy val jsonText          = largeSingleLineJson(entries = 20_000)
    lazy val jsonSearchResults = Rope(jsonText).searchAll("\"k19999\"")
    lazy val jsonCursorOffset  = Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5)

    BenchmarkRunner.Benchmark.sharing((jsonText, jsonSearchResults, jsonCursorOffset))(
      List(
        BenchmarkRunner.Benchmark(
          "rope.large_json.search",
          3,
          12,
          () => assert(jsonSearchResults.nonEmpty),
          () => Rope(jsonText).searchAll("\"k19999\"")
        ),
        BenchmarkRunner.Benchmark(
          "rope.large_json.cursor_offset",
          BenchmarkIterationCounts.RopeCursorOffsetWarmups,
          20,
          () => assert(jsonCursorOffset == jsonText.length - 5),
          () => Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5),
          settleJit = true
        )
      )
    )

  private def lspAndProjectBenchmarks(projectRoot: => Path): List[BenchmarkRunner.Benchmark] =
    lazy val lspMessages = (1 to 250).toList.map { id =>
      Json.obj("jsonrpc" -> Json.fromString("2.0"), "id" -> Json.fromInt(id), "method" -> Json.fromString("benchmark"))
    }
    lazy val framedLspMessages       = lspMessages.flatMap(LspFramer.encode).toArray
    lazy val root                    = projectRoot
    lazy val projectTask             = ProjectTaskDetector.detect(root, ProjectTaskKind.Test)
    lazy val decodedLspMessages      = decodeLspMessages(framedLspMessages)
    lazy val projectTaskPresentation = projectTask.map(ProjectTaskTerminal.started)
    lazy val largeFrame = List(
      Json.obj(
        "jsonrpc" -> Json.fromString("2.0"),
        "id"      -> Json.fromInt(1),
        "result"  -> Json.fromString("y" * 1_048_576)
      )
    )
    lazy val framedLargeFrame  = largeFrame.flatMap(LspFramer.encode).toArray
    lazy val decodedLargeFrame = decodeLspMessages(framedLargeFrame, PipeReadBytes)

    BenchmarkRunner.Benchmark.sharing(
      (
        lspMessages,
        framedLspMessages,
        root,
        projectTask,
        decodedLspMessages,
        projectTaskPresentation,
        largeFrame,
        framedLargeFrame,
        decodedLargeFrame
      )
    )(
      List(
        BenchmarkRunner.Benchmark(
          "lsp.framer.large_batch",
          3,
          BenchmarkIterationCounts.LspFramer,
          () => assert(decodedLspMessages == lspMessages),
          () => decodeLspMessages(framedLspMessages)
        ),
        BenchmarkRunner.Benchmark(
          "lsp.framer.large_frame",
          3,
          20,
          () => assert(decodedLargeFrame == largeFrame),
          () => decodeLspMessages(framedLargeFrame, PipeReadBytes)
        ),
        BenchmarkRunner.Benchmark(
          "project_task.responsiveness",
          3,
          20,
          () =>
            assert(
              projectTask.exists(command => command.workingDirectory == root && command.executable == "sbt") &&
                projectTaskPresentation.exists(_.contains("Running test task"))
            ),
          () => ProjectTaskDetector.detect(root, ProjectTaskKind.Test).map(ProjectTaskTerminal.started)
        )
      )
    )

  private val PipeReadBytes = 8 * 1024

  private def decodeLspMessages(bytes: Array[Byte], chunkBytes: Int = Int.MaxValue): List[Json] =
    import cats.effect.unsafe.implicits.global

    fs2.Stream
      .chunk(fs2.Chunk.array(bytes))
      .chunkLimit(chunkBytes)
      .unchunks
      .through(LspFramer.decode)
      .compile
      .toList
      .unsafeRunSync()

  private def projectTaskFixtureResource: Resource[IO, Path] =
    Resource.make(
      IO.blocking {
        val root = TestTemp.directory("serenity-performance-project-")
        val _    = Files.writeString(root.resolve("build.sbt"), "// deterministic benchmark fixture\n")
        root
      }
    )(root =>
      IO.blocking {
        val _ = Files.deleteIfExists(root.resolve("build.sbt"))
        val _ = Files.deleteIfExists(root)
        ()
      }
    )

end PerformanceBenchmarks
