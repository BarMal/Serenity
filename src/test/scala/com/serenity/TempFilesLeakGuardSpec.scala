package com.serenity

import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A run of the suite used to leave tens of thousands of folders in the system temp directory. These pin the three ways
  * that happened: a helper that never removes what it made, one that skips the removal when the test fails or is
  * cancelled, and a new call site that goes round the helper.
  */
class TempFilesLeakGuardSpec extends AnyFlatSpec with Matchers:

  private def populate(directory: Path): Unit =
    val nested = Files.createDirectories(directory.resolve("session").resolve("sessions"))
    val _      = Files.writeString(nested.resolve("session.json"), "{}")

  "TestTemp.scoped" should "remove the folder and everything in it once the test is done" in {
    val used = TestTemp.scoped("leak-guard-success").use(dir => IO.blocking(populate(dir)).as(dir))

    Files.exists(used.unsafeRunSync()) shouldBe false
  }

  it should "remove the folder when the test fails" in {
    val seen = Ref.unsafe[IO, Option[Path]](None)
    val failing = TestTemp
      .scoped("leak-guard-failure")
      .use(dir => seen.set(Some(dir)) >> IO.blocking(populate(dir)) >> IO.raiseError[Unit](new IllegalStateException))

    failing.attempt.unsafeRunSync().isLeft shouldBe true
    seen.get.unsafeRunSync().map(Files.exists(_)) shouldBe Some(false)
  }

  it should "remove the folder when the test is cancelled" in {
    val seen = Ref.unsafe[IO, Option[Path]](None)
    val hanging = TestTemp
      .scoped("leak-guard-cancel")
      .use(dir => seen.set(Some(dir)) >> IO.blocking(populate(dir)) >> IO.never[Unit])

    hanging.timeout(300.millis).attempt.unsafeRunSync().isLeft shouldBe true
    seen.get.unsafeRunSync().map(Files.exists(_)) shouldBe Some(false)
  }

  "TestTemp.within" should "remove the folder when the body throws" in {
    val created = new java.util.concurrent.atomic.AtomicReference[Option[Path]](None)
    an[IllegalStateException] should be thrownBy TestTemp.within("leak-guard-within") { dir =>
      created.set(Some(dir))
      populate(dir)
      throw new IllegalStateException
    }

    created.get.map(Files.exists(_)) shouldBe Some(false)
  }

  "TestTemp.directory and file" should "never land loose in the system temp directory" in {
    val systemTemp = Paths.get(System.getProperty("java.io.tmpdir")).toRealPath()
    val directory  = TestTemp.directory("leak-guard-loose")
    val file       = TestTemp.file("leak-guard-loose", ".txt")

    directory.toRealPath().getParent should not be systemTemp
    file.toRealPath().getParent should not be systemTemp
  }

  "The sources" should "not create temp files or folders that nothing removes" in {
    val offenders =
      sourcesUnder("src/test/scala").filterNot(_.getFileName.toString == "TestTemp.scala").flatMap(violations)

    withClue(offenders.mkString("Route these through TestTemp:\n", "\n", "\n")) {
      offenders shouldBe empty
    }
  }

  it should "limit production temp folders to the ones that remove themselves" in {
    val removesItself = Set(
      "StateManager.scala",
      "SafeMode.scala",
      "StartupWarmUp.scala",
      "SingleInstance.scala"
    )
    val offenders = sourcesUnder("src/main/scala")
      .filterNot(path => removesItself(path.getFileName.toString))
      .flatMap(violations)
      .filterNot(_.contains("AtomicFileWriter.scala"))
      .filterNot(_.contains("ConfigManager.scala"))

    withClue(offenders.mkString("Make the folder a Resource released with DirectoryTree:\n", "\n", "\n")) {
      offenders shouldBe empty
    }
  }

  private def sourcesUnder(directory: String): List[Path] =
    Using.resource(Files.walk(Paths.get(directory)))(_.iterator().asScala.filter(_.toString.endsWith(".scala")).toList)

  private val CreateTempFolder = """Files\.createTempDirectory\(""".r
  private val CreateTempFile   = """Files\.createTempFile\(([^)]*)\)""".r

  // `createTempFile(directory, prefix, suffix)` lands inside a folder the caller already owns; the two-argument form
  // lands in the system temp directory.
  private def violations(source: Path): List[String] =
    Files.readAllLines(source).asScala.toList.zipWithIndex.collect {
      case (line, index)
          if !line.trim.startsWith("//") && !line.trim.startsWith("*") &&
            (CreateTempFolder.findFirstIn(line).isDefined ||
              CreateTempFile.findAllMatchIn(line).exists(_.group(1).count(_ == ',') < 2)) =>
        s"$source:${index + 1}: ${line.trim}"
    }
