package com.serenity.app

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupProbeSpec extends AnyFlatSpec with Matchers:

  private final class Console:
    private val buffer = new ByteArrayOutputStream()
    val out            = new PrintStream(buffer, true, StandardCharsets.UTF_8)
    def text: String   = buffer.toString(StandardCharsets.UTF_8)

  "StartupProbe.run" should "inject once, wait for the paint, then print the marker and release quit" in {
    val console = Console()

    val (injections, quitReleased, printedBeforePaint) = (for
      quit     <- Deferred[IO, Unit]
      painted  <- Deferred[IO, Unit]
      injected <- Ref.of[IO, Int](0)
      probe <- StartupProbe
        .run(injected.update(_ + 1), painted.get, quit, 5.seconds, console.out)
        .start
      _      <- IO.sleep(100.millis)
      before <- IO(console.text)
      _      <- painted.complete(())
      _      <- probe.joinWithNever
      count  <- injected.get
      freed  <- quit.tryGet
    yield (count, freed.isDefined, before)).unsafeRunSync()

    injections shouldBe 1
    printedBeforePaint shouldBe empty
    quitReleased shouldBe true
    console.text.trim shouldBe StartupProbe.interactiveLine
  }

  it should "print the marker before the quit signal can be observed" in {
    val console = Console()

    val printedWhenQuitSeen = (for
      quit <- Deferred[IO, Unit]
      seen <- quit.get.map(_ => console.text).start
      _    <- StartupProbe.run(IO.unit, IO.unit, quit, 5.seconds, console.out)
      text <- seen.joinWithNever
    yield text).unsafeRunSync()

    printedWhenQuitSeen.trim shouldBe StartupProbe.interactiveLine
  }

  it should "report a timeout, and still release quit, when the key is never painted" in {
    val console = Console()

    val released = (for
      quit  <- Deferred[IO, Unit]
      _     <- StartupProbe.run(IO.unit, IO.never, quit, 200.millis, console.out)
      freed <- quit.tryGet
    yield freed.isDefined).unsafeRunSync()

    released shouldBe true
    console.text.trim shouldBe StartupProbe.timedOutLine
  }

  it should "report a failed injection as a timeout line and still release quit" in {
    val console = Console()

    val broken = IO.raiseError[Unit](new RuntimeException("no window"))

    val released = (for
      quit  <- Deferred[IO, Unit]
      _     <- StartupProbe.run(broken, IO.never, quit, 5.seconds, console.out)
      freed <- quit.tryGet
    yield freed.isDefined).unsafeRunSync()

    released shouldBe true
    console.text.trim shouldBe StartupProbe.failedLine
  }

  "StartupProbe" should "use marker lines that differ from the smoke test's, so each is found on its own" in {
    val lines =
      Set(StartupProbe.interactiveLine, StartupProbe.timedOutLine, StartupProbe.failedLine, SmokeTest.readyLine)
    lines.size shouldBe 4
  }

  "StartupProbe.refusal" should "let the probe run in safe mode, whatever the home is" in {
    val options = LaunchOptions(startupProbe = true, safeMode = true)

    StartupProbe.refusal(options, TestTemp.directory("probe-real-home")) shouldBe None
  }

  it should "refuse the probe outside safe mode when the home carries no marker, as a real home does not" in {
    val options = LaunchOptions(startupProbe = true)

    val refusal = StartupProbe.refusal(options, TestTemp.directory("probe-real-home"))

    refusal.exists(_.contains("--safe-mode")) shouldBe true
    refusal.exists(_.contains(StartupProbe.IsolatedHomeMarker)) shouldBe true
  }

  it should "let the probe run outside safe mode in a home the harness marked as its own" in {
    val home = TestTemp.directory("probe-marked-home")
    Files.writeString(home.resolve(StartupProbe.IsolatedHomeMarker), "")

    StartupProbe.refusal(LaunchOptions(startupProbe = true), home) shouldBe None
  }

  it should "not accept a directory in place of the marker file" in {
    val home = TestTemp.directory("probe-marker-dir")
    Files.createDirectory(home.resolve(StartupProbe.IsolatedHomeMarker))

    StartupProbe.refusal(LaunchOptions(startupProbe = true), home).isDefined shouldBe true
  }

  it should "say nothing when the probe is not asked for" in {
    StartupProbe.refusal(LaunchOptions(), Path.of("/nonexistent/home")) shouldBe None
  }
