package com.serenity.testkit

import cats.effect.IO
import cats.effect.testkit.TestControl

/** Runs an `IO` program to completion on `TestControl`'s mocked scheduler instead of the real one.
  *
  * `IO.sleep` and timeouts resolve as soon as every other runnable fiber is blocked on time, so a program whose
  * correctness depends on wall-clock races (an assertion that only holds if some other fiber wins a real-time timeout)
  * becomes deterministic: the virtual clock only advances when nothing is left to run at the current instant, so a real
  * "still nothing after 3 seconds of CI scheduling pressure" flake cannot occur here -- the "3 seconds" pass instantly
  * and only once every other fiber is stuck.
  *
  * Not a fit for programs with real async dependencies (a real socket, a real thread, `IO.blocking`, a genuine
  * subprocess): those fall outside the mocked runtime and `executeEmbed` reports them as non-terminating.
  */
object VirtualTime:

  def runVirtual[A](program: IO[A]): A =
    TestControl.executeEmbed(program).unsafeRunSync()(using cats.effect.unsafe.implicits.global)
