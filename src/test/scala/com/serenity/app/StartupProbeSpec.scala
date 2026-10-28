package com.serenity.app

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupProbeSpec extends AnyFlatSpec with Matchers:

  private final class Console:
    private val buffer = new ByteArrayOutputStream()
    val out            = new PrintStream(buffer, true, StandardCharsets.UTF_8)
    def text: String   = buffer.toString(StandardCharsets.UTF_8)

  "StartupProbe.run" should "inject the key once, wait for it to be painted, then print the marker and release quit" in {
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

    val released = (for
      quit  <- Deferred[IO, Unit]
      _     <- StartupProbe.run(IO.raiseError(new RuntimeException("no window")), IO.never, quit, 5.seconds, console.out)
      freed <- quit.tryGet
    yield freed.isDefined).unsafeRunSync()

    released shouldBe true
    console.text.trim shouldBe StartupProbe.failedLine
  }

  "StartupProbe" should "use marker lines that differ from the smoke test's, so each is found on its own" in {
    Set(StartupProbe.interactiveLine, StartupProbe.timedOutLine, StartupProbe.failedLine, SmokeTest.readyLine).size shouldBe 4
  }
