package com.serenity.perf

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.io.EditorConfigResolver
import com.serenity.state.models.IndentationHint

/** What resolving a document's indentation from its `.editorconfig` files costs on a warm cache. Each lookup reads and
  * hashes the file of every directory it climbs through (#2348), so the cost is that read; `shallow` has one file one
  * level up, `nested` a repository-sized tree with files at three levels.
  */
private[perf] object EditorConfigBenchmarks:

  private val settings = "root = true\n[*]\nindent_style = space\nindent_size = 2\n[*.md]\nindent_size = 4\n"

  private def write(path: Path, text: String): Path =
    val _ = Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def benchmark(name: String, documentPath: => Path): BenchmarkRunner.Benchmark =
    val resolver      = EditorConfigResolver()
    lazy val document = documentPath
    BenchmarkRunner.Benchmark.building(document)(
      BenchmarkRunner.Benchmark(
        name,
        warmups = 3,
        iterations = 20,
        verify = () => assert(resolver.indentationFor(document).unsafeRunSync() != IndentationHint.Empty),
        run = () => resolver.indentationFor(document).unsafeRunSync(),
        settleJit = true
      )
    )

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    lazy val shallow =
      val shallowRoot = TestTemp.directory("editorconfig-bench-shallow")
      val _           = write(shallowRoot.resolve(".editorconfig"), settings)
      write(shallowRoot.resolve("a.md"), "x")

    lazy val nested =
      val nestedRoot = TestTemp.directory("editorconfig-bench-nested")
      val _          = write(nestedRoot.resolve(".editorconfig"), settings)
      val _          = write(nestedRoot.resolve("src/.editorconfig"), "[*.scala]\nindent_size = 2\n")
      val _ = write(nestedRoot.resolve("src/main/scala/com/example/.editorconfig"), "[*.scala]\nindent_size = 4\n")
      write(nestedRoot.resolve("src/main/scala/com/example/deep/Main.scala"), "x")

    List(
      benchmark("editorconfig.indentation.shallow", shallow),
      benchmark("editorconfig.indentation.nested", nested)
    )
