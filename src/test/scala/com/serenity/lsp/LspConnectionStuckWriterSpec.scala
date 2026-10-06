package com.serenity.lsp

import java.io.OutputStream
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.client.{LspConnection, LspMethod, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A server's stdin pipe that is full blocks the writer's `write` until the process dies, on every operating system and
  * on Windows (small pipe buffers) soonest. Releasing must end the process before it waits on anything the stuck writer
  * holds, or the release can only finish when the server happens to exit on its own.
  */
class LspConnectionStuckWriterSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspConnectionStuckWriterSpec"))

  /** Writes block while the pipe is "full"; the process dying frees them, as it does for a real pipe. */
  final private class FullablePipe(underlying: OutputStream) extends OutputStream:
    private val processDied = new CountDownLatch(1)
    private val full        = new AtomicBoolean(false)

    def fill(): Unit         = full.set(true)
    def processExits(): Unit = processDied.countDown()

    override def write(byte: Int): Unit =
      if full.get then processDied.await()
      underlying.write(byte)

    override def write(bytes: Array[Byte], off: Int, len: Int): Unit =
      if full.get then processDied.await()
      underlying.write(bytes, off, len)

    override def flush(): Unit = underlying.flush()

    override def close(): Unit = underlying.close()

  "LspConnection" should "end the server process before waiting on a writer stuck on a full stdin pipe" in {
    val release = MockLspServer
      .resource(Map("initialize" -> Json.obj("capabilities" -> Json.obj())), logger)
      .use { server =>
        val pipe = new FullablePipe(server.clientOut)
        LspConnection
          .connect(
            LanguageId.Scala,
            server.clientIn,
            pipe,
            WorkspaceRootUri("file:///workspace"),
            logger,
            endServer = IO(pipe.processExits())
          )
          .use(connection =>
            IO(pipe.fill()) >> connection.sendNotification(LspMethod("textDocument/didChange"), Json.obj())
          )
          .as(true)
          .timeoutTo(10.seconds, IO.pure(false))
      }

    release.unsafeRunSync() shouldBe true
  }
