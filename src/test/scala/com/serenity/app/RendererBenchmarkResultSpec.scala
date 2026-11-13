package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.app.RendererBenchmarkResult.{Key, Standing, Stored}
import com.serenity.app.RendererChoice.{Candidate, CandidateRuns, Outcome, Workload}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererBenchmarkResultSpec extends AnyFlatSpec with Matchers:

  private val machine = Key(
    glRenderer = "Mesa Intel(R) UHD Graphics 620 (KBL GT2)",
    os = "Linux 6.18",
    toolkit = "wayland",
    display = "3840x2160@2.0",
    jvm = "25.0.5+8",
    skiko = "0.153.0",
    powerState = "ac"
  )

  private val results = List(
    CandidateRuns(
      Candidate.LayerImage,
      List(
        Outcome.Measured(Workload(31.5, 48.0, 2.0, 1.2, Map("page-down" -> 12.25, "full-redraw" -> 40.0))),
        Outcome.Measured(Workload(33.0, 52.5, 2.0, 1.4, Map("page-down" -> 12.5, "full-redraw" -> 41.0)))
      )
    ),
    CandidateRuns(Candidate.SkiaRaster, List(Outcome.Measured(Workload(44.0, 90.0, 2.0, 1.1, Map("page-down" -> 20.0))))),
    CandidateRuns(Candidate.LayerPicture, List(Outcome.Failed("every typing sample timed out (150)")))
  )

  private val stored = Stored(machine, Instant.parse("2026-11-11T17:30:00Z"), results)

  private def tempDir(): Path = TestTemp.directory("render-benchmark-spec")

  "RendererBenchmarkResult" should "read back exactly what it wrote, measured and failed runs alike" in {
    RendererBenchmarkResult.decode(RendererBenchmarkResult.encode(stored)) shouldBe Some(stored)
  }

  it should "read a result with no runs for a candidate" in {
    val bare = stored.copy(results = List(CandidateRuns(Candidate.LayerImage, Nil)))
    RendererBenchmarkResult.decode(RendererBenchmarkResult.encode(bare)) shouldBe Some(bare)
  }

  it should "read a corrupt file as absent, whatever way it is corrupt" in {
    val good = RendererBenchmarkResult.encode(stored)
    List(
      "",
      "not json at all",
      "{}",
      "[1, 2, 3]",
      good.take(good.length / 2),
      good.replace("layer+image", "layer+nonsense"),
      good.replace("\"typingP50Ms\"", "\"typing\""),
      good.replace("\"typingP95Ms\"", "\"typingP95\""),
      good.replace("\"scale\"", "\"zoom\""),
      good.replace("\"clockResolutionMs\"", "\"resolution\""),
      good.replace("2026-11-11T17:30:00Z", "yesterday")
    ).foreach(text => withClue(text.take(40))(RendererBenchmarkResult.decode(text) shouldBe None))
  }

  it should "read a file written before it carried the p95 and the scale as absent, so the machine is measured again" in {
    val old = RendererBenchmarkResult
      .encode(stored)
      .replace("\"schema\"", "\"schemaWas\"")
    RendererBenchmarkResult.decode(old) shouldBe None
    val earlier = RendererBenchmarkResult.encode(stored).replace("\"schema\" : 3", "\"schema\" : 2")
    RendererBenchmarkResult.decode(earlier) shouldBe None
  }

  "RendererBenchmarkResult.standing" should "be absent when nothing was stored" in {
    RendererBenchmarkResult.standing(machine, None) shouldBe Standing.Absent
  }

  it should "apply a result stored under the same key" in {
    RendererBenchmarkResult.standing(machine, Some(stored)) shouldBe Standing.Applies(stored)
  }

  it should "read a result as stale, naming what changed, when any part of the key changed" in {
    val changed = List(
      "GL renderer" -> machine.copy(glRenderer = "llvmpipe"),
      "OS"          -> machine.copy(os = "Linux 6.19"),
      "toolkit"     -> machine.copy(toolkit = "x11"),
      "display"     -> machine.copy(display = "1920x1080@1.0"),
      "JVM"         -> machine.copy(jvm = "25.0.6+7"),
      "Skiko"       -> machine.copy(skiko = "0.154.0"),
      "power state" -> machine.copy(powerState = "battery")
    )
    changed.foreach { (what, current) =>
      withClue(what)(
        RendererBenchmarkResult.standing(current, Some(stored)) shouldBe Standing.Stale(stored, List(what))
      )
    }
  }

  it should "name every part that changed, in a fixed order" in {
    val current = machine.copy(skiko = "0.154.0", glRenderer = "llvmpipe", powerState = "battery")
    RendererBenchmarkResult.standing(current, Some(stored)) shouldBe
      Standing.Stale(stored, List("GL renderer", "Skiko", "power state"))
  }

  "RendererBenchmarkResult.load" should "find nothing in a directory that has no result file" in {
    RendererBenchmarkResult.load(tempDir().resolve("render-benchmark.json")).unsafeRunSync() shouldBe None
  }

  it should "find nothing when the file is corrupt" in {
    val file = tempDir().resolve("render-benchmark.json")
    Files.write(file, "{ this is not json".getBytes(StandardCharsets.UTF_8))
    RendererBenchmarkResult.load(file).unsafeRunSync() shouldBe None
  }

  it should "find nothing when the path is a directory" in {
    RendererBenchmarkResult.load(tempDir()).unsafeRunSync() shouldBe None
  }

  "RendererBenchmarkResult.save" should "write a file that load reads back, creating the directory" in {
    val file = tempDir().resolve("nested").resolve("render-benchmark.json")
    RendererBenchmarkResult.save(file, stored).unsafeRunSync()
    RendererBenchmarkResult.load(file).unsafeRunSync() shouldBe Some(stored)
  }

  it should "replace an earlier result and leave nothing else beside it" in {
    val dir   = tempDir()
    val file  = dir.resolve("render-benchmark.json")
    val newer = stored.copy(measuredAt = Instant.parse("2026-11-12T08:00:00Z"), results = results.take(1))
    RendererBenchmarkResult.save(file, stored).unsafeRunSync()
    RendererBenchmarkResult.save(file, newer).unsafeRunSync()
    RendererBenchmarkResult.load(file).unsafeRunSync() shouldBe Some(newer)
    Files.list(dir).iterator().asScala.map(_.getFileName.toString).toList shouldBe List("render-benchmark.json")
  }

  "RendererBenchmarkResult.defaultPath" should "sit beside the config in the user's .serenity directory" in {
    val path = RendererBenchmarkResult.defaultPath
    path.getFileName.toString shouldBe "render-benchmark.json"
    path.getParent.getFileName.toString shouldBe ".serenity"
  }
