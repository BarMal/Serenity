package com.serenity.diagnostics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IoAppReadinessSpec extends AnyFlatSpec with Matchers:

  private def frame(owner: String, method: String): StackTraceElement =
    new StackTraceElement(owner, method, null, -1)

  private val waitingForProgram = List(
    frame("jdk.internal.misc.Unsafe", "park"),
    frame("java.util.concurrent.ArrayBlockingQueue", "take"),
    frame("cats.effect.IOApp", "main"),
    frame("com.serenity.diagnostics.ShutdownBoundChild", "main")
  )

  "IoAppReadiness.hookInstalled" should "hold once the main thread waits for the program, which it reaches only after registering the hook" in {
    IoAppReadiness.hookInstalled(waitingForProgram) shouldBe true
  }

  it should "not hold while the main thread is still starting the program" in {
    IoAppReadiness.hookInstalled(
      List(
        frame("cats.effect.IOApp", "main"),
        frame("com.serenity.diagnostics.ShutdownBoundChild", "main")
      )
    ) shouldBe false
  }

  it should "not hold for a queue wait that is not IOApp's" in {
    IoAppReadiness.hookInstalled(
      List(
        frame("java.util.concurrent.ArrayBlockingQueue", "take"),
        frame("com.serenity.Elsewhere", "run")
      )
    ) shouldBe false
  }

  it should "not hold with no main thread to look at" in {
    IoAppReadiness.hookInstalled(Nil) shouldBe false
  }
