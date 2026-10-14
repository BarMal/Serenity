package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FootprintBenchmarksSpec extends AnyFlatSpec with Matchers:

  private def retained(name: String): Long =
    val scenario = FootprintBenchmarks.scenarios.find(_.name == name)
    withClue(s"scenario $name: ")(scenario should not be empty)
    scenario.flatMap(FootprintBenchmarks.measure(_).retainedBytes).getOrElse(-1L)

  "the footprint scenarios" should "cover the editor states the app actually holds" in {
    FootprintBenchmarks.scenarios.map(_.name) shouldBe List(
      "footprint.empty_editor",
      "footprint.plain_document_15k_lines",
      "footprint.rich_document",
      "footprint.after_1000_edits",
      "footprint.ten_buffers"
    )
  }

  "a 15,000-line document" should "retain more heap than the empty editor" in {
    retained("footprint.plain_document_15k_lines") should be > retained("footprint.empty_editor")
  }

  it should "retain at least the text it holds" in {
    retained("footprint.plain_document_15k_lines") should be > 15_000L * 60L
  }

  "ten open buffers" should "retain more heap than one such document" in {
    retained("footprint.ten_buffers") should be > retained("footprint.plain_document_15k_lines")
  }

  "a thousand edits" should "retain the edited document, not only the empty editor" in {
    retained("footprint.after_1000_edits") should be > retained("footprint.plain_document_15k_lines") / 2
  }

  "a measurement" should "report no time columns and a retained value in its CSV row" in {
    val result = FootprintBenchmarks.measure(FootprintBenchmarks.scenarios.head)
    val fields = BenchmarkRunner.csvRow(result).split(",", -1)
    fields(0) shouldBe "footprint.empty_editor"
    fields.length shouldBe 11
    fields(10).toLong should be >= 0L
  }

  "retainedBytes" should "see the size of a held array and ignore garbage built along the way" in {
    val bytes = HeapFootprint.retainedBytes { () =>
      val _ = Array.fill(200)(new Array[Byte](1_000))
      new Array[Byte](4_000_000)
    }
    bytes should (be >= 3_900_000L and be <= 4_400_000L)
  }

end FootprintBenchmarksSpec
