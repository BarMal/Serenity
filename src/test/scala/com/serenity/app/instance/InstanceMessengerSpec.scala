package com.serenity.app.instance

import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.DurationInt

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** #2023: the local socket a second launch hands its files to. */
class InstanceMessengerSpec extends AnyFlatSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger: Logger[IO]  = LoggerFactory[IO].getLogger(using LoggerName("InstanceMessengerSpec"))

  // Unix socket paths are limited to about 100 bytes, so these stay short and directly under the temp directory.
  private def socketPath(): Path = Files.createTempDirectory("sock").resolve("i.sock")

  "InstanceMessenger" should "deliver forwarded paths to the running instance in order" in {
    val socket = socketPath()
    val first  = Paths.get("/work/notes.md")
    val second = Paths.get("/work/with space, comma and {braces}.md")

    val program = InstanceMessenger.serve(socket, logger).use { requests =>
      for
        delivered <- InstanceMessenger.forward(socket, List(first, second))
        empty     <- InstanceMessenger.forward(socket, Nil)
        received  <- requests.take(2).compile.toList.timeout(10.seconds)
      yield (delivered, empty, received)
    }

    program.unsafeRunSync() shouldBe (Delivery.Delivered, Delivery.Delivered, List(List(first, second), Nil))
  }

  it should "report an instance that is not listening as unreachable" in {
    InstanceMessenger.forward(socketPath(), Nil).unsafeRunSync() shouldBe Delivery.Unreachable
  }

  it should "replace a socket file left behind by a crashed instance, and remove its own on exit" in {
    val socket = socketPath()
    Files.writeString(socket, "left by a crash")

    val delivered = InstanceMessenger.serve(socket, logger).use(_ => InstanceMessenger.forward(socket, Nil))

    delivered.unsafeRunSync() shouldBe Delivery.Delivered
    Files.exists(socket) shouldBe false
  }

  "InstanceMessenger.decode" should "reject anything that is not a request" in {
    InstanceMessenger.decode("GET / HTTP/1.1") shouldBe None
    InstanceMessenger.decode(InstanceMessenger.encode(List(Paths.get("/a")))) shouldBe Some(List(Paths.get("/a")))
  }
