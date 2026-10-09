package com.serenity.perf

import java.nio.file.{Files, Path}

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
    import cats.effect.unsafe.implicits.global

    SwingWindow
      .resource(metrics = RenderBenchmarks.cellMetrics, chromeMetrics = RenderBenchmarks.uiMetrics)
      .flatMap(window =>
        LaptopFrameBenchmarks
          .presentWindowResource(RenderBenchmarks.cellMetrics, RenderBenchmarks.uiMetrics)
          .map(window -> _)
      )
      .flatMap(windows => projectTaskFixtureResource.map(projectRoot => windows -> projectRoot))
      .use {
        case ((window, presentWindow), projectRoot) =>
          IO {
            val all = benchmarks(window, projectRoot) ++ LaptopFrameBenchmarks.benchmarks(presentWindow) ++
              RichDocumentOpenBenchmarks.benchmarks
            val unknown = BenchmarkIterationCounts.JitSettled -- all.map(_.name)
            require(unknown.isEmpty, s"JitSettled names no benchmark: ${unknown.mkString(", ")}")
            val results = BenchmarkRunner.runMatching(args.toList, all)
            BenchmarkRunner.printResults(results)
          }
      }
      .unsafeRunSync()

  private def benchmarks(cursorWindow: SwingWindow, projectRoot: Path): List[BenchmarkRunner.Benchmark] =
    ropeBenchmarks() ++
      RenderBenchmarks.frameBenchmarks(cursorWindow) ++
      ReducerBenchmarks.benchmarks() ++
      DamageBenchmarks.benchmarks() ++
      CommandRunnerBenchmarks.benchmarks() ++
      EqualsBenchmarks.benchmarks() ++
      FindReplaceBenchmarks.benchmarks() ++
      lspAndProjectBenchmarks(projectRoot) ++
      RenderBenchmarks.markdownBenchmarks()

  private[perf] def ropeBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val jsonText          = largeSingleLineJson(entries = 20_000)
    val jsonSearchResults = Rope(jsonText).searchAll("\"k19999\"")
    val jsonCursorOffset  = Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5)

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

  private def lspAndProjectBenchmarks(projectRoot: Path): List[BenchmarkRunner.Benchmark] =
    val lspMessages = (1 to 250).toList.map { id =>
      Json.obj("jsonrpc" -> Json.fromString("2.0"), "id" -> Json.fromInt(id), "method" -> Json.fromString("benchmark"))
    }
    val framedLspMessages       = lspMessages.flatMap(LspFramer.encode).toArray
    val projectTask             = ProjectTaskDetector.detect(projectRoot, ProjectTaskKind.Test)
    val decodedLspMessages      = decodeLspMessages(framedLspMessages)
    val projectTaskPresentation = projectTask.map(ProjectTaskTerminal.started)

    List(
      BenchmarkRunner.Benchmark(
        "lsp.framer.large_batch",
        3,
        BenchmarkIterationCounts.LspFramer,
        () => assert(decodedLspMessages == lspMessages),
        () => decodeLspMessages(framedLspMessages)
      ),
      BenchmarkRunner.Benchmark(
        "project_task.responsiveness",
        3,
        20,
        () =>
          assert(
            projectTask.exists(command => command.workingDirectory == projectRoot && command.executable == "sbt") &&
              projectTaskPresentation.exists(_.contains("Running test task"))
          ),
        () => ProjectTaskDetector.detect(projectRoot, ProjectTaskKind.Test).map(ProjectTaskTerminal.started)
      )
    )

  private def decodeLspMessages(bytes: Array[Byte]): List[Json] =
    import cats.effect.unsafe.implicits.global

    fs2.Stream
      .chunk(fs2.Chunk.array(bytes))
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
