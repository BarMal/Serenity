package com.serenity.lsp

import java.io.{InputStream, OutputStream}
import java.net.{InetAddress, ServerSocket, Socket}

import cats.effect.std.Queue
import cats.effect.{IO, Resource}
import com.serenity.lsp.client.LspFramer
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import org.typelevel.log4cats.Logger

class MockLspServer private (
    fixtures: Map[String, Json],
    closeOnMethods: Set[String],
    received: Queue[IO, Json],
    outQueue: Queue[IO, Option[Json]],
    serverSocket: ServerSocket,
    serverConnection: Socket,
    clientSocket: Socket,
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
      closeQuietly(clientIn) >>
      closeQuietly(serverConnection) >>
      closeQuietly(clientSocket) >>
      closeQuietly(serverSocket)

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
      transport <- IO.blocking {
        val serverSocket     = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
        val clientSocket     = new Socket(InetAddress.getLoopbackAddress, serverSocket.getLocalPort)
        val serverConnection = serverSocket.accept()
        (
          serverSocket,
          serverConnection,
          clientSocket,
          serverConnection.getInputStream,
          serverConnection.getOutputStream,
          clientSocket.getInputStream,
          clientSocket.getOutputStream
        )
      }
    yield
      val (serverSocket, serverConnection, clientSocket, serverIn, serverOut, clientIn, clientOut) = transport
      new MockLspServer(
        fixtures,
        closeOnMethods,
        received,
        outQueue,
        serverSocket,
        serverConnection,
        clientSocket,
        serverIn,
        serverOut,
        logger,
        clientIn,
        clientOut
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
