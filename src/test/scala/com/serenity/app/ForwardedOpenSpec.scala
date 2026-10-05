package com.serenity.app

import java.nio.file.{Path, Paths}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.state.manager.FileOpener
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** #2023: files a later launch hands over are opened in the running editor. */
class ForwardedOpenSpec extends AnyFlatSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private given Logger[IO]        = LoggerFactory[IO].getLogger(using LoggerName("ForwardedOpenSpec"))

  "AppRuntime.openForwarded" should "open every forwarded file, carrying on past one that fails" in {
    val unreadable = Paths.get("/work/unreadable.md")
    val wanted     = List(Paths.get("/work/a.md"), unreadable, Paths.get("/work/b.md"))

    val opened = for
      log <- Ref.of[IO, List[Path]](Nil)
      opener = FileOpener(path =>
        if path == unreadable then IO.raiseError(new java.io.IOException("denied")) else log.update(_ :+ path)
      )
      _      <- AppRuntime.openForwarded(opener)(wanted)
      result <- log.get
    yield result

    opened.unsafeRunSync() shouldBe List(Paths.get("/work/a.md"), Paths.get("/work/b.md"))
  }
