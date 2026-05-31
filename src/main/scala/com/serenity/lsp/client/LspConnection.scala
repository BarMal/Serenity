package com.serenity.lsp.client

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref, Resource}
import fs2.Stream
import io.circe.Json
import com.serenity.lsp.config.{LanguageId, LspServerConfig}
import com.serenity.lsp.model.Diagnostic
import org.typelevel.log4cats.Logger

import java.io.{BufferedInputStream, BufferedOutputStream}

class LspConnection private (
  val languageId: LanguageId,
  val incoming:   Stream[IO, Json],
  sendQueue:      Queue[IO, Option[Json]],
  idRef:          Ref[IO, Long],
  pendingRef:     Ref[IO, Map[Long, Deferred[IO, Json]]],
  logger:         Logger[IO]
):

  def sendRequest(method: String, params: Json): IO[Json] =
    for
      id       <- idRef.updateAndGet(_ + 1)
      deferred <- Deferred[IO, Json]
      _        <- pendingRef.update(_ + (id -> deferred))
      _        <- sendQueue.offer(Some(Json.obj(
                    "jsonrpc" -> io.circe.Json.fromString("2.0"),
                    "id"      -> io.circe.Json.fromLong(id),
                    "method"  -> io.circe.Json.fromString(method),
                    "params"  -> params
                  )))
      result   <- deferred.get
    yield result

  def sendNotification(method: String, params: Json): IO[Unit] =
    sendQueue.offer(Some(Json.obj(
      "jsonrpc" -> io.circe.Json.fromString("2.0"),
      "method"  -> io.circe.Json.fromString(method),
      "params"  -> params
    ))).void

  def processIncoming(onDiagnostics: (String, List[Diagnostic]) => IO[Unit]): IO[Unit] =
    incoming.evalMap { json =>
      if LspProtocol.isResponse(json) then
        LspProtocol.responseId(json) match
          case Some(id) =>
            pendingRef.modify { pending =>
              pending.get(id) match
                case Some(d) => (pending - id, d.complete(json).void)
                case None    => (pending, IO.unit)
            }.flatten
          case None => IO.unit
      else if LspProtocol.isNotification(json) then
        LspProtocol.notificationMethod(json) match
          case Some("textDocument/publishDiagnostics") =>
            LspProtocol.parseDiagnostics(json) match
              case Some((uri, diags)) => onDiagnostics(uri, diags)
              case None               => logger.warn("[LSP] Could not parse publishDiagnostics")
          case Some(method) =>
            logger.debug(s"[LSP] Notification: $method")
          case None => IO.unit
      else IO.unit
    }.compile.drain


object LspConnection:

  def apply(
    config:  LspServerConfig,
    rootUri: String,
    logger:  Logger[IO]
  ): Resource[IO, LspConnection] =
    val cmd = (config.binary.command :: config.defaultArgs).toArray
    for
      process    <- Resource.make(
                      IO.blocking(new java.lang.ProcessBuilder(cmd*).start())
                    )(proc => IO.blocking(proc.destroyForcibly()).void)
      sendQueue  <- Resource.eval(Queue.bounded[IO, Option[Json]](256))
      idRef      <- Resource.eval(Ref.of[IO, Long](0L))
      pendingRef <- Resource.eval(Ref.of[IO, Map[Long, Deferred[IO, Json]]](Map.empty))
      out         = new BufferedOutputStream(process.getOutputStream)
      in          = new BufferedInputStream(process.getInputStream)
      inStream    = fs2.io.readInputStream(IO.pure(in), chunkSize = 8192)
                      .through(LspFramer.decode)
      conn        = new LspConnection(config.languageId, inStream, sendQueue, idRef, pendingRef, logger)
      // writer fiber: drain outgoing queue → stdin
      _ <- Resource.make(
             Stream.fromQueueNoneTerminated(sendQueue)
               .evalMap(json => IO.blocking { out.write(LspFramer.encode(json)); out.flush() })
               .compile.drain
               .start
           )(fiber => sendQueue.offer(None) >> fiber.join.void)
      _ <- Resource.eval(initHandshake(conn, rootUri, logger))
    yield conn

  private def initHandshake(conn: LspConnection, rootUri: String, logger: Logger[IO]): IO[Unit] =
    for
      pid <- IO(ProcessHandle.current().pid().toInt)
      _   <- logger.info(s"[LSP] initialize ${conn.languageId.id} rootUri=$rootUri")
      _   <- conn.sendRequest("initialize", LspProtocol.initializeParams(pid, rootUri))
               .handleErrorWith(ex => logger.error(ex)("[LSP] initialize failed") >> IO.raiseError(ex))
      _   <- conn.sendNotification("initialized", LspProtocol.initializedParams)
      _   <- logger.info(s"[LSP] Handshake complete: ${conn.languageId.id}")
    yield ()
