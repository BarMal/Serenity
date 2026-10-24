package com.serenity.ui.benchmark

import java.awt.GraphicsEnvironment
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.Files

import scala.jdk.CollectionConverters.*

import cats.effect.ExitCode
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.testkit.DisplayExclusive
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RenderBenchmarkSpec extends AnyFlatSpec with Matchers:

  private val small = RenderBenchmarkOptions(widthPx = 640, heightPx = 400, frames = 2, warmup = 1)

  "RenderBenchmark.run" should "print the report and keep it as Markdown and JSON in the output directory" in {
    assume(!GraphicsEnvironment.isHeadless, "The benchmark opens real windows, which needs a display")
    assume(System.getProperty("os.name", "").toLowerCase.contains("linux"), "left to the owner's machine elsewhere")
    val directory = TestTemp.directory("render-benchmark")
    val captured  = new ByteArrayOutputStream()
    val code = DisplayExclusive(
      RenderBenchmark.run(small, directory, new PrintStream(captured, true), startup = "X11 (test)").unsafeRunSync()
    )
    code shouldBe ExitCode.Success
    val printed = captured.toString
    printed should include("# Render benchmark")
    printed should include("X11 (test)")
    Scenario.values.foreach(scenario => printed should include(scenario.label))
    val files = Files.list(directory).iterator().asScala.toList.map(_.getFileName.toString)
    files.exists(name => name.startsWith("render-benchmark-") && name.endsWith(".md")) shouldBe true
    val jsonName = files.find(name => name.startsWith("render-benchmark-") && name.endsWith(".json"))
    val json =
      parse(Files.readString(directory.resolve(jsonName.getOrElse(fail("no JSON file"))))).getOrElse(fail("not JSON"))
    json.hcursor.downField("scenarios").as[Vector[io.circe.Json]].map(_.size) shouldBe Right(Scenario.values.length)
  }

  it should "say it needs a display, and fail, on a headless machine" in {
    assume(GraphicsEnvironment.isHeadless, "only meaningful without a display")
    val captured = new ByteArrayOutputStream()
    RenderBenchmark
      .run(small, TestTemp.directory("render-benchmark"), new PrintStream(captured, true))
      .unsafeRunSync() shouldBe ExitCode.Error
    captured.toString should include("display")
  }
