package com.serenity.perf

import cats.effect.unsafe.IORuntime
import com.serenity.keystroke.events.{ModalInsertChar, ModalSubmit, OpenGotoLine}

object GoToLineBenchmarks:

  /** Go to Line through its prompt, jumping between the start and the end of a 3000-paragraph document. */
  private[perf] def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    lazy val (stateManager, proseBufferId) =
      LaptopFrameBenchmarks.proseStateManager(paragraphs = 3000, cursorLine = 100)
    def cursorLine: Option[Int] = stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .buffers
      .get(proseBufferId)
      .flatMap(_.editing.cursorPositions.headOption)
      .map(_.line)
    def goTo(line: Int): Unit =
      val typed = line.toString.toList.map(ModalInsertChar(_))
      (OpenGotoLine :: typed ::: List(ModalSubmit)).foreach(event => stateManager.applyEvent(event).unsafeRunSync())
    def jumpAndBack(): Unit =
      goTo(2900)
      goTo(100)
    BenchmarkRunner.Benchmark.sharing((stateManager, proseBufferId))(
      List(
        BenchmarkRunner.Benchmark(
          "laptop.input.state_manager.go_to_line_3000_paragraphs",
          BenchmarkIterationCounts.GoToLineWarmups,
          BenchmarkIterationCounts.GoToLine,
          () =>
            goTo(2900)
            val jumped = cursorLine
            goTo(100)
            assert(jumped.contains(2899) && cursorLine.contains(99), s"go to line landed on $jumped then $cursorLine")
          ,
          () => jumpAndBack(),
          minBatch = BenchmarkIterationCounts.GoToLineBatch
        )
      )
    )
