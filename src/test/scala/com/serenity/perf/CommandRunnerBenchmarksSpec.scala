package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The CI perf job is the only place these benchmarks run, so their fixtures are checked here under plain `sbt test`.
  */
class CommandRunnerBenchmarksSpec extends AnyFlatSpec with Matchers:

  "The command runner benchmarks" should "cover a keystroke and an arrow press in both the palette and settings" in {
    CommandRunnerBenchmarks.benchmarks().map(_.name) shouldBe List(
      "command_runner.palette.keystroke",
      "command_runner.palette.arrow_press",
      "command_runner.settings.keystroke",
      "command_runner.settings.arrow_press"
    )
  }

  they should "search a font catalogue the size of a typical installation" in {
    val catalog = CommandRunnerBenchmarks.fontCatalog
    (catalog.monospace ++ catalog.text ++ catalog.ui).distinct.size should be >= 600
  }

  they should "each pass their own result check" in
    CommandRunnerBenchmarks.benchmarks().foreach(_.verify())

end CommandRunnerBenchmarksSpec
