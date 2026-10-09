package com.serenity.app.instance

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, SocketChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.DurationInt

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** #2023: the local socket a second launch hands its files to. */
class InstanceMessengerSpec extends AnyFlatSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger: Logger[IO]  = LoggerFactory[IO].getLogger(using LoggerName("InstanceMessengerSpec"))

  // Unix socket paths are limited to about 100 bytes, so these stay short and directly under the temp directory.
  private def socketPath(): Path = TestTemp.directory("sock").resolve("i.sock")

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

  // Half-closing an AF_UNIX socket is not something to depend on across operating systems (Windows' AF_UNIX support is
  // the youngest), so a request has to be complete, and answered, without its sender ever shutting down its output.
  it should "acknowledge a request that ends at its newline, without waiting for the sender to half-close" in {
    val socket  = socketPath()
    val notes   = Paths.get("/work/notes.md")
    val request = (InstanceMessenger.encode(List(notes)) + "\n").getBytes(StandardCharsets.UTF_8)

    def sendWithoutHalfClose: IO[String] =
      Resource
        .fromAutoCloseable(IO.blocking(SocketChannel.open(StandardProtocolFamily.UNIX)))
        .use { channel =>
          IO.interruptible {
            val _ = channel.connect(UnixDomainSocketAddress.of(socket))
            Channels.newOutputStream(channel).write(request)
            new String(Channels.newInputStream(channel).readNBytes(2), StandardCharsets.UTF_8)
          }
        }

    val program = InstanceMessenger.serve(socket, logger).use { requests =>
      for
        reply    <- sendWithoutHalfClose.timeout(5.seconds)
        received <- requests.take(1).compile.toList.timeout(5.seconds)
      yield (reply, received)
    }

    program.unsafeRunSync() shouldBe ("ok", List(List(notes)))
  }

  "InstanceMessenger.decode" should "reject anything that is not a request" in {
    InstanceMessenger.decode("GET / HTTP/1.1") shouldBe None
    InstanceMessenger.decode(InstanceMessenger.encode(List(Paths.get("/a")))) shouldBe Some(List(Paths.get("/a")))
  }
