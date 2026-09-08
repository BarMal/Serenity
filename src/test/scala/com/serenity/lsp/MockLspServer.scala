package com.serenity.lsp

import java.io.{InputStream, OutputStream}
import java.util.concurrent.LinkedBlockingQueue

import cats.effect.std.Queue
import cats.effect.{IO, Resource}
import com.serenity.lsp.client.LspFramer
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import org.typelevel.log4cats.Logger

/** A duplex byte pipe backed by an in-memory queue, standing in for one direction of a real subprocess's stdin/stdout.
  * Deliberately not `java.io.PipedInputStream`/`PipedOutputStream`: those record the identity of the last thread to
  * read or write and raise "Pipe broken"/"Write end dead" if that thread is no longer the one calling next, which
  * cats-effect's blocking pool doesn't guarantee across separate `IO.blocking` calls -- the exact flakiness class #1358
  * already named for a different real-pipe pairing. A `LinkedBlockingQueue` has no notion of thread identity at all.
  */
final private[lsp] class QueueBytePipe:
  private val queue            = new LinkedBlockingQueue[Integer]()
  private val EofSentinel: Int = -1

  val in: InputStream = new InputStream:
    override def read(): Int =
      val next = queue.take().intValue()
      if next == EofSentinel then
        queue.put(EofSentinel) // leave EOF latched for any subsequent read
        -1
      else next

    // `InputStream`'s default read(byte[], off, len) blocks on `read()` for every one of the `len` bytes requested,
    // so it wouldn't return a short message until the caller's full read buffer (8192 bytes for fs2's readInputStream)
    // filled up -- a real socket or pipe instead returns as soon as at least one byte is available, which is the
    // contract every blocking reader upstream (fs2 included) actually relies on.
    override def read(b: Array[Byte], off: Int, len: Int): Int =
      if len <= 0 then 0
      else
        val first = queue.take().intValue()
        if first == EofSentinel then
          queue.put(EofSentinel)
          -1
        else
          b(off) = first.toByte
          1 + fillAvailable(b, off + 1, len - 1)

    private def fillAvailable(b: Array[Byte], off: Int, remaining: Int): Int =
      if remaining <= 0 then 0
      else
        Option(queue.poll()) match
          case None => 0
          case Some(v) if v.intValue() == EofSentinel =>
            queue.put(EofSentinel)
            0
          case Some(v) =>
            b(off) = v.byteValue()
            1 + fillAvailable(b, off + 1, remaining - 1)

    // A blocked read (genuinely waiting on `queue.take()`, on its own dedicated blocking-pool thread) has to be
    // unblocked here, exactly as closing a real socket or pipe unblocks a thread parked in its `read()`: fs2's
    // `readInputStream` is not itself interruptible on cancellation, so without this, cancelling the fiber that owns
    // this read (as every `Resource` teardown here does) would wait forever for a read that nothing will ever satisfy.
    override def close(): Unit = queue.put(EofSentinel)

  val out: OutputStream = new OutputStream:
    override def write(b: Int): Unit = queue.put(b & 0xff)

    override def write(bytes: Array[Byte], off: Int, len: Int): Unit =
      if len > 0 then
        queue.put(bytes(off) & 0xff)
        write(bytes, off + 1, len - 1)

    override def close(): Unit = queue.put(EofSentinel)

/** An in-memory stand-in for a real LSP server: two [[QueueBytePipe]]s stand in for the duplex byte stream a real
  * subprocess's stdin/stdout would give `LspConnection.connect`, so this test double never opens a real
  * `ServerSocket`/`Socket` (or the OS-level port binding, accept, and TIME_WAIT churn that comes with one across
  * thousands of test runs).
  */
class MockLspServer private (
    fixtures: Map[String, Json],
    closeOnMethods: Set[String],
    received: Queue[IO, Json],
    outQueue: Queue[IO, Option[Json]],
    serverIn: InputStream,
    serverOut: OutputStream,
    logger: Logger[IO],
    val clientIn: InputStream,
    val clientOut: OutputStream
):

  def shutdown(): IO[Unit] =
    logger.info("[test] mock server shutdown").attempt.void >>
      outQueue.offer(None).attempt.void >>
      closeQuietly(serverOut) >>
      closeQuietly(clientOut) >>
      closeQuietly(serverIn) >>
      closeQuietly(clientIn)

  def push(notification: Json): IO[Unit] =
    logger
      .debug(
        s"[test] mock push ${notification.hcursor.downField("method").as[String].toOption.getOrElse("<response>")}"
      )
      .attempt
      .void >>
      outQueue.offer(Some(notification))

  def writeRaw(bytes: Array[Byte]): IO[Unit] =
    IO.blocking {
      serverOut.write(bytes)
      serverOut.flush()
    }

  def takeReceived: IO[Json] =
    received.take

  def drainReceived(n: Int): IO[List[Json]] =
    Stream.repeatEval(received.take).take(n.toLong).compile.toList

  private[lsp] def writerLoop: IO[Unit] =
    Stream
      .fromQueueNoneTerminated(outQueue)
      .evalMap(json =>
        logger
          .debug(s"[test] mock write ${json.hcursor.downField("method").as[String].toOption.getOrElse("<response>")}")
          .attempt
          .void >>
          IO.blocking {
            serverOut.write(LspFramer.encode(json))
            serverOut.flush()
          }
      )
      .compile
      .drain

  private[lsp] def readerLoop: IO[Unit] =
    fs2.io
      .readInputStream(IO.pure(serverIn), 8192)
      .through(LspFramer.decode)
      .evalMap(json =>
        logger
          .debug(s"[test] mock read ${json.hcursor.downField("method").as[String].toOption.getOrElse("<response>")}")
          .attempt
          .void >>
          received.offer(json) >>
          handleMessage(json)
      )
      .handleErrorWith(_ => Stream.empty)
      .compile
      .drain

  private def handleMessage(json: Json): IO[Unit] =
    val hasId     = json.hcursor.downField("id").succeeded
    val hasMethod = json.hcursor.downField("method").succeeded
    if hasId && hasMethod then
      val id     = json.hcursor.downField("id").as[Long].getOrElse(0L)
      val method = json.hcursor.downField("method").as[String].getOrElse("")
      if closeOnMethods.contains(method) then closeQuietly(serverOut)
      else
        val result = fixtures.getOrElse(method, Json.obj())
        outQueue.offer(
          Some(
            Json.obj(
              "jsonrpc" -> "2.0".asJson,
              "id"      -> id.asJson,
              "result"  -> result
            )
          )
        )
    else IO.unit

  private def closeQuietly(closeable: AutoCloseable): IO[Unit] =
    IO.blocking(closeable.close()).attempt.void

object MockLspServer:

  def create(
    fixtures: Map[String, Json],
    logger: Logger[IO],
    closeOnMethods: Set[String] = Set.empty
  ): IO[MockLspServer] =
    for
      received <- Queue.unbounded[IO, Json]
      outQueue <- Queue.unbounded[IO, Option[Json]]
      // client -> server: the client's writes land on the server's input.
      clientToServer <- IO(new QueueBytePipe)
      // server -> client: the server's writes land on the client's input.
      serverToClient <- IO(new QueueBytePipe)
    yield new MockLspServer(
      fixtures,
      closeOnMethods,
      received,
      outQueue,
      serverIn = clientToServer.in,
      serverOut = serverToClient.out,
      logger,
      clientIn = serverToClient.in,
      clientOut = clientToServer.out
    )

  def resource(
    fixtures: Map[String, Json],
    logger: Logger[IO],
    closeOnMethods: Set[String] = Set.empty
  ): Resource[IO, MockLspServer] =
    for
      server <- Resource.eval(create(fixtures, logger, closeOnMethods))
      _ <- Resource.make {
        for
          writerFiber <- server.writerLoop.start
          readerFiber <- server.readerLoop.start
        yield (writerFiber, readerFiber)
      } {
        case (writerFiber, readerFiber) =>
          server.shutdown() >>
            writerFiber.cancel >>
            readerFiber.cancel
      }
    yield server
