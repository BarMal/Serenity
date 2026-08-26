package com.serenity.input

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InProcessClipboardSpec extends AnyFlatSpec with Matchers:

  "InProcessClipboard" should "report no text before anything is written" in {
    val clipboard = InProcessClipboard[IO].unsafeRunSync()
    clipboard.readText.unsafeRunSync() shouldBe None
  }

  it should "read back exactly the last text written" in {
    val clipboard = InProcessClipboard[IO].unsafeRunSync()
    clipboard.writeText("first").unsafeRunSync()
    clipboard.writeText("second").unsafeRunSync()
    clipboard.readText.unsafeRunSync() shouldBe Some("second")
  }

  it should "not share state between separate instances" in {
    val a = InProcessClipboard[IO].unsafeRunSync()
    val b = InProcessClipboard[IO].unsafeRunSync()
    a.writeText("only in a").unsafeRunSync()
    b.readText.unsafeRunSync() shouldBe None
  }
