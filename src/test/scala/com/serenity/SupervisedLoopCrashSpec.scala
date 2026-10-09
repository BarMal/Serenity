package com.serenity

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

class SupervisedLoopCrashSpec extends AnyFlatSpec with Matchers:

  private given Logger[IO] = NoOpLogger.impl[IO]

  "AppRuntimeRenderLoops.superviseLoop" should "record the failure before it forces the safe shutdown" in {
    val program = for
      events <- Ref.of[IO, Vector[String]](Vector.empty)
      _ <- AppRuntimeRenderLoops.superviseLoop(
        "render loop",
        events.update(_ :+ "forceQuit"),
        (summary, error) => events.update(_ :+ s"recorded:$summary:${error.getMessage}")
      )(
        IO.raiseError(
          AppRuntime.RuntimeFailure("render loop", "idle.cursor-render", "viewport=120x40", RuntimeException("boom"))
        )
      )
      seen <- events.get
    yield seen

    val seen = program.unsafeRunSync()
    seen.size shouldBe 2
    seen.headOption.getOrElse("") should startWith("recorded:[RUNTIME] render loop failed")
    seen.headOption.getOrElse("") should endWith(":boom")
    seen.lastOption shouldBe Some("forceQuit")
  }

  it should "still force the shutdown when recording itself fails" in {
    val program = for
      forced <- Ref.of[IO, Boolean](false)
      _ <- AppRuntimeRenderLoops.superviseLoop(
        "render loop",
        forced.set(true),
        (_, _) => IO.raiseError(RuntimeException("disk full"))
      )(IO.raiseError(RuntimeException("boom")))
      result <- forced.get
    yield result

    program.unsafeRunSync() shouldBe true
  }
