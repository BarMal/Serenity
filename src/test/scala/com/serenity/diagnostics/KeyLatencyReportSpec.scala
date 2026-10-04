package com.serenity.diagnostics

import scala.concurrent.duration.*

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeyLatencyReportSpec extends AnyFlatSpec with Matchers:

  private def typeOneKey(trace: KeyLatencyTrace): IO[Unit] =
    IO {
      trace.keyReceived(System.currentTimeMillis())
      trace.keyEnqueued()
      trace.keysDequeued(1)
      trace.dispatchStarted()
      trace.keysApplied(1)
      trace.damageEmitted()
      trace.frameWoke()
      trace.frameDeadlineReached()
      trace.frameModelRead()
      trace.framePublished()
      trace.frameEnded()
      trace.paintStarted()
      trace.paintFinished()
    }

  "KeyLatencyReport.stream" should "leave the trace off and schedule no wakeups while the setting is off" in {
    val trace = KeyLatencyTrace()
    val program = for
      enabled <- SignallingRef.of[IO, Boolean](false)
      _       <- KeyLatencyReport.stream(trace, enabled.discrete, _ => IO.unit).compile.drain
    yield ()

    val parked = (for
      control  <- TestControl.execute(program)
      _        <- control.tickFor(1.minute)
      finished <- control.results
      idle     <- control.isDeadlocked
    yield (finished, idle)).unsafeRunSync()

    parked shouldBe ((None, true))
    trace.isEnabled shouldBe false
  }

  it should "log each painted keystroke, then a summary, every interval only while the setting is on" in {
    val trace = KeyLatencyTrace()
    val program = for
      enabled   <- SignallingRef.of[IO, Boolean](false)
      lines     <- Ref.of[IO, Vector[String]](Vector.empty)
      fiber     <- KeyLatencyReport.stream(trace, enabled.discrete, line => lines.update(_ :+ line)).compile.drain.start
      _         <- IO.sleep(1.second)
      _         <- typeOneKey(trace)
      _         <- IO.sleep(KeyLatencyReport.Interval * 2)
      whileOff  <- lines.get
      offBefore <- IO(trace.isEnabled)
      _         <- enabled.set(true)
      _         <- IO.sleep(1.second)
      onAfter   <- IO(trace.isEnabled)
      _         <- typeOneKey(trace)
      _         <- IO.sleep(KeyLatencyReport.Interval)
      whileOn   <- lines.get
      _         <- enabled.set(false)
      _         <- IO.sleep(1.second)
      offAfter  <- IO(trace.isEnabled)
      _         <- typeOneKey(trace)
      _         <- IO.sleep(KeyLatencyReport.Interval * 2)
      afterOff  <- lines.get
      _         <- fiber.cancel
    yield (whileOff, offBefore, onAfter, whileOn, offAfter, afterOff)

    val (whileOff, offBefore, onAfter, whileOn, offAfter, afterOff) = runVirtual(program)
    whileOff shouldBe empty
    offBefore shouldBe false
    onAfter shouldBe true
    whileOn should have size 2
    whileOn.headOption.exists(_.startsWith("[LATENCY] seq=0 ")) shouldBe true
    whileOn.lastOption.exists(_.startsWith("[LATENCY] summary window=")) shouldBe true
    offAfter shouldBe false
    afterOff shouldBe whileOn
  }
