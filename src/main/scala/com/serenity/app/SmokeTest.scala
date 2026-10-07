package com.serenity.app

import java.io.PrintStream

import cats.effect.{Deferred, IO}

/** What `--smoke-test` reports: a packaged build that reached its first frame prints [[readyLine]] and quits, so a
  * script can tell "started and painted" from "started and hung" without a person looking at the window.
  */
object SmokeTest:

  val readyLine: String = "SERENITY_SMOKE_READY"

  def announceReady(quit: Deferred[IO, Unit], out: PrintStream = System.out): IO[Unit] =
    IO { out.println(readyLine); out.flush() } >> quit.complete(()).void
