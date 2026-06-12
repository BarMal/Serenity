package com.serenity.lsp.client

import java.io.{BufferedInputStream, BufferedOutputStream}

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import com.serenity.lsp.config.{LanguageId, LspServerConfig}
import com.serenity.lsp.model.Diagnostic
import fs2.Stream
import fs2.io.readInputStream
import io.circe.Json
import org.typelevel.log4cats.Logger

class LspConnection private (
    val languageId: LanguageId,
    sendQueue: Queue[IO, Option[Json]],
    idRef: Ref[IO, Long],
    pendingRef: Ref[IO, Map[Long, Deferred[IO, Either[Throwable, Json]]]],
    notifQueue: Queue[IO, Option[Json]],
    logger: Logger[IO]
):

  def sendRequest(method: String, params: Json): IO[Json] =
    for
      id       <- idRef.updateAndGet(_ + 1)
      deferred <- Deferred[IO, Either[Throwable, Json]]
      _        <- pendingRef.update(_ + (id -> deferred))
      _ <- sendQueue.offer(
        Some(
          Json.obj(
            "jsonrpc" -> io.circe.Json.fromString("2.0"),
            "id"      -> io.circe.Json.fromLong(id),
            "method"  -> io.circe.Json.fromString(method),
            "params"  -> params
          )
        )
      )
      result <- deferred.get.flatMap(IO.fromEither)
    yield result

  def sendNotification(method: String, params: Json): IO[Unit] =
    sendQueue
      .offer(
        Some(
          Json.obj(
            "jsonrpc" -> io.circe.Json.fromString("2.0"),
            "method"  -> io.circe.Json.fromString(method),
            "params"  -> params
          )
        )
      )
      .void

  def processIncoming(onDiagnostics: (String, List[Diagnostic]) => IO[Unit]): IO[Unit] =
    Stream
      .fromQueueNoneTerminated(notifQueue)
      .evalMap { json =>
        LspProtocol.notificationMethod(json) match
          case Some("textDocument/publishDiagnostics") =>
            LspProtocol.parseDiagnostics(json) match
              case Some((uri, diags)) => onDiagnostics(uri, diags)
              case None               => logger.warn("[LSP] Could not parse publishDiagnostics")
          case Some(method) =>
            logger.debug(s"[LSP] Notification: $method")
          case None => IO.unit
      }
      .compile
      .drain

  private[lsp] def handleIncomingJson(json: Json): IO[Unit] =
    if LspProtocol.isResponse(json) then
      LspProtocol.responseId(json) match
        case Some(id) =>
          pendingRef.modify { pending =>
            pending.get(id) match
              case Some(d) => (pending - id, d.complete(Right(json)).void)
              case None    => (pending, IO.unit)
          }.flatten
        case None => IO.unit
    else if LspProtocol.isNotification(json) then notifQueue.offer(Some(json))
    else IO.unit

  private[lsp] def takeOutgoing: IO[Option[Json]] =
    sendQueue.take

  private[lsp] def outgoingMessages: Stream[IO, Json] =
    Stream.fromQueueNoneTerminated(sendQueue)

  private[lsp] def closeQueues: IO[Unit] =
    failPending(new RuntimeException(s"LSP connection closed for ${languageId.id}")) >>
      sendQueue.offer(None).attempt.void >>
      notifQueue.offer(None).attempt.void

  private[lsp] def completeNotifications: IO[Unit] =
    notifQueue.offer(None).attempt.void

  private def failPending(cause: Throwable): IO[Unit] =
    pendingRef
      .modify(pending => (Map.empty, pending.values.toList))
      .flatMap(_.traverse_(_.complete(Left(cause)).void))

object LspConnection:

  private case class ConnectionFibers(
      writer: Fiber[IO, Throwable, Unit],
      reader: Fiber[IO, Throwable, Unit]
  )

  private[lsp] def create(
    languageId: LanguageId,
    logger: Logger[IO]
  ): IO[LspConnection] =
    for
      sendQueue  <- Queue.bounded[IO, Option[Json]](256)
      idRef      <- Ref.of[IO, Long](0L)
      pendingRef <- Ref.of[IO, Map[Long, Deferred[IO, Either[Throwable, Json]]]](Map.empty)
      notifQueue <- Queue.bounded[IO, Option[Json]](256)
    yield new LspConnection(languageId, sendQueue, idRef, pendingRef, notifQueue, logger)

  // Package-visible entry point — accepts pre-opened streams; used by tests via MockLspServer.
  private[lsp] def connect(
    languageId: LanguageId,
    rawIn: java.io.InputStream,
    rawOut: java.io.OutputStream,
    rootUri: String,
    logger: Logger[IO]
  ): Resource[IO, LspConnection] =
    for
      conn <- Resource.eval(create(languageId, logger))
      in  = new BufferedInputStream(rawIn)
      out = new BufferedOutputStream(rawOut)
      _ <- Resource.make {
        for
          writerFiber <- conn.outgoingMessages
            .evalMap(json => IO.blocking { out.write(LspFramer.encode(json)); out.flush() })
            .compile
            .drain
            .start
          readerFiber <- readInputStream(IO.pure(in), 8192)
            .through(LspFramer.decode)
            .evalMap(conn.handleIncomingJson)
            .compile
            .drain
            .guarantee(conn.completeNotifications)
            .start
        yield ConnectionFibers(writer = writerFiber, reader = readerFiber)
      } {
        case ConnectionFibers(writerFiber, readerFiber) =>
          closeQuietly(out) >>
            closeQuietly(in) >>
            conn.closeQueues >>
            writerFiber.cancel >>
            readerFiber.cancel
      }
      _ <- Resource.eval(initHandshake(conn, rootUri, logger))
    yield conn

  def apply(
    config: LspServerConfig,
    rootUri: String,
    logger: Logger[IO]
  ): Resource[IO, LspConnection] =
    for
      process <- Resource.make(
        IO.blocking(
          new java.lang.ProcessBuilder(
            (config.command :: config.defaultArgs).toArray*
          ).start()
        )
      )(proc => IO.blocking(proc.destroyForcibly()).void)
      conn <- connect(config.languageId, process.getInputStream, process.getOutputStream, rootUri, logger)
    yield conn

  private def initHandshake(conn: LspConnection, rootUri: String, logger: Logger[IO]): IO[Unit] =
    for
      pid <- IO(ProcessHandle.current().pid().toInt)
      _   <- logger.info(s"[LSP] initialize ${conn.languageId.id} rootUri=$rootUri")
      _ <- conn
        .sendRequest("initialize", LspProtocol.initializeParams(pid, rootUri))
        .handleErrorWith(ex => logger.error(ex)("[LSP] initialize failed") >> IO.raiseError(ex))
      _ <- conn.sendNotification("initialized", LspProtocol.initializedParams)
      _ <- logger.info(s"[LSP] Handshake complete: ${conn.languageId.id}")
    yield ()

  private def closeQuietly(closeable: AutoCloseable): IO[Unit] =
    IO.blocking(closeable.close()).attempt.void
