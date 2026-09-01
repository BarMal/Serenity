package com.serenity.input

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

class Osc52ClipboardSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given Logger[IO]        = LoggerFactory[IO].getLogger(using LoggerName("Osc52ClipboardSpec"))

  "Osc52Clipboard.writeText" should "send the OSC 52 sequence through the terminal writer and mirror to fallback" in {
    val written   = Ref.unsafe[IO, List[String]](Nil)
    val fallback  = InProcessClipboard[IO].unsafeRunSync()
    val clipboard = Osc52Clipboard[IO](sequence => written.update(_ :+ sequence), fallback)

    clipboard.writeText("hello").unsafeRunSync()

    written.get.unsafeRunSync() shouldBe List(Osc52.encode("hello").toOption.get)
    fallback.readText.unsafeRunSync() shouldBe Some("hello")
  }

  it should "fall back to the given clipboard, without writing, when the payload is oversized" in {
    val written   = Ref.unsafe[IO, List[String]](Nil)
    val fallback  = InProcessClipboard[IO].unsafeRunSync()
    val clipboard = Osc52Clipboard[IO](sequence => written.update(_ :+ sequence), fallback, maxEncodedBytes = 1)

    clipboard.writeText("hello").unsafeRunSync()

    written.get.unsafeRunSync() shouldBe Nil
    fallback.readText.unsafeRunSync() shouldBe Some("hello")
  }

  "Osc52Clipboard.readText" should "always delegate to the fallback clipboard" in {
    val fallback = InProcessClipboard[IO].unsafeRunSync()
    fallback.writeText("already there").unsafeRunSync()
    val clipboard = Osc52Clipboard[IO](_ => IO.unit, fallback)

    clipboard.readText.unsafeRunSync() shouldBe Some("already there")
  }
