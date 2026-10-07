package com.serenity.app

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmokeTestSpec extends AnyFlatSpec with Matchers:

  "SmokeTest.announceReady" should "print the ready line and then release the quit signal" in {
    val buffer = new ByteArrayOutputStream()
    val out    = new PrintStream(buffer, true, StandardCharsets.UTF_8)

    val quitWasReleased = (for
      quit     <- Deferred[IO, Unit]
      _        <- SmokeTest.announceReady(quit, out)
      released <- quit.tryGet
    yield released.isDefined).unsafeRunSync()

    quitWasReleased shouldBe true
    buffer.toString(StandardCharsets.UTF_8).trim shouldBe SmokeTest.readyLine
  }

  it should "print the ready line before the quit signal can be observed" in {
    val buffer = new ByteArrayOutputStream()
    val out    = new PrintStream(buffer, true, StandardCharsets.UTF_8)

    val printedWhenQuitSeen = (for
      quit <- Deferred[IO, Unit]
      seen <- quit.get.map(_ => buffer.toString(StandardCharsets.UTF_8)).start
      _    <- SmokeTest.announceReady(quit, out)
      text <- seen.joinWithNever
    yield text).unsafeRunSync()

    printedWhenQuitSeen.trim shouldBe SmokeTest.readyLine
  }
