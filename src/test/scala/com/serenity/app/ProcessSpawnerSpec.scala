package com.serenity.app

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import com.serenity.app.RendererChoice.Candidate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProcessSpawnerSpec extends AnyFlatSpec with Matchers:

  private val runtime = ProcessSpawner.Runtime(
    java = "/opt/jdk/bin/java",
    jvmArguments = List("-Xmx6g", "--enable-native-access=ALL-UNNAMED"),
    classPath = "/opt/serenity/Serenity.jar",
    mainClass = "Main"
  )
  private val document = Path.of("/tmp/serenity-trial-doc.txt")

  "ProcessSpawner.command" should "start the same java with the same JVM flags and class path, in safe mode, on the trial document" in {
    val command = ProcessSpawner.command(runtime, Candidate.SkiaRaster, document)
    command.take(3) shouldBe List("/opt/jdk/bin/java", "-Xmx6g", "--enable-native-access=ALL-UNNAMED")
    command should contain inOrder ("-cp", "/opt/serenity/Serenity.jar", "Main")
    command.dropWhile(_ != "Main").drop(1) shouldBe
      List("--safe-mode", "--renderer=skia-raster", "--renderer-trial=skia-raster", "--open", "/tmp/serenity-trial-doc.txt")
  }

  it should "choose the layer's image frames explicitly for layer+image, so a changed default cannot change what is measured" in {
    val command = ProcessSpawner.command(runtime, Candidate.LayerImage, document)
    command should contain("-Dserenity.skia.layerFrames=image")
    command should contain("--renderer=skia")
    command should contain("--renderer-trial=layer+image")
  }

  it should "choose the layer's picture frames for layer+picture" in {
    val command = ProcessSpawner.command(runtime, Candidate.LayerPicture, document)
    command should contain("-Dserenity.skia.layerFrames=picture")
    command should contain("--renderer-trial=layer+picture")
  }

  it should "put no frame property on the raster surface, which has no frame form to choose" in {
    ProcessSpawner.command(runtime, Candidate.SkiaRaster, document).exists(_.startsWith("-Dserenity.skia.layerFrames")) shouldBe false
  }

  "ProcessSpawner" should "answer everything the child printed" in {
    val spawner = ProcessSpawner(_ => List("bash", "-c", "echo one; echo two 1>&2"), 10.seconds)
    val text    = spawner.run(Candidate.SkiaRaster).unsafeRunSync()
    text should (include("one") and include("two"))
  }

  it should "stop a child that outlives its timeout and answer what it had printed" in {
    val spawner = ProcessSpawner(_ => List("bash", "-c", "echo started; sleep 60"), 1.second)
    val started = System.nanoTime()
    val text    = spawner.run(Candidate.SkiaRaster).unsafeRunSync()
    text should include("started")
    ((System.nanoTime() - started) / 1_000_000_000L) should be < 30L
  }

  it should "fail when the child cannot be started, so the coordinator counts a failed trial" in {
    val spawner = ProcessSpawner(_ => List("/nonexistent/serenity-java"), 5.seconds)
    spawner.run(Candidate.SkiaRaster).attempt.unsafeRunSync().isLeft shouldBe true
  }
