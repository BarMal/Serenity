package com.serenity.ui.renderer

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PaintExecutionContextSpec extends AnyFlatSpec with Matchers:

  "PaintExecutionContext" should "run submitted work on a single dedicated thread" in {
    val program = for threadNames <- PaintExecutionContext.resource.use { ec =>
          val onEc = IO(Thread.currentThread().getName).evalOn(ec)
          (onEc, onEc).mapN((a, b) => List(a, b))
        }
    yield threadNames

    val names = program.unsafeRunSync()
    names.toSet shouldBe Set("serenity-paint")
  }

  it should "shut down its executor when the resource is released" in {
    val program = PaintExecutionContext.resource.allocated.flatMap {
      case (ec, release) =>
        release.as(ec)
    }

    // ExecutionContext.fromExecutorService returns an ExecutionContextExecutorService, which extends
    // java.util.concurrent.ExecutorService directly -- so the shutdown state is observable without submitting
    // new work (submitting after shutdown is inherently racy to assert on: whether it's rejected synchronously
    // or silently dropped depends on the executor implementation).
    val ec = program.unsafeRunSync()
    ec.asInstanceOf[java.util.concurrent.ExecutorService].isShutdown shouldBe true
  }
