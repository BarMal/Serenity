package com.serenity.lsp.client

import java.io.{BufferedInputStream, BufferedOutputStream}

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import com.serenity.lsp.config.{LanguageId, LspServerConfig}
import com.serenity.lsp.model.{Diagnostic, TextDocumentSyncKind}
import fs2.Stream
import fs2.io.readInputStream
import io.circe.Json
import org.typelevel.log4cats.Logger

class LspConnection private (
    val languageId: LanguageId,
    sendQueue: Queue[IO, Option[Json]],
    idRef: Ref[IO, Long],
    pendingRef: Ref[IO, Map[RequestId, Deferred[IO, Either[Throwable, Json]]]],
    notifQueue: Queue[IO, Option[Json]],
    requestTimeout: FiniteDuration,
    logger: Logger[IO],
    syncKindRef: Ref[IO, TextDocumentSyncKind]
):

  /** The server's negotiated `textDocumentSync` capability, read off the `initialize` response during the handshake
    * (see `initHandshake`). Defaults to `Full` -- this client's behavior before that capability was read at all --
    * for any connection that never runs the real handshake (e.g. one built directly via `create` in tests).
    */
  def syncKind: IO[TextDocumentSyncKind] = syncKindRef.get

  private[lsp] def setSyncKind(kind: TextDocumentSyncKind): IO[Unit] = syncKindRef.set(kind)

  def sendRequest(method: LspMethod, params: Json): IO[Json] =
    sendRequest(method, params, requestTimeout)

  def sendRequest(method: LspMethod, params: Json, timeout: FiniteDuration): IO[Json] =
    for
      rawId <- idRef.updateAndGet(_ + 1)
      id = RequestId(rawId)
      deferred <- Deferred[IO, Either[Throwable, Json]]
      _        <- pendingRef.update(_ + (id -> deferred))
      result <-
        val cleanup      = pendingRef.update(_ - id)
        val request      = LspProtocol.request(id, method, params)
        val timeoutError = LspConnection.LspRequestTimeout(languageId, method, timeout)

        // Giving up locally used to leave the server computing an answer nobody would read. On a large project
        // completion and hover are expensive and are reissued on every keystroke, so the abandoned work piles up
        // exactly when the server is already slow.
        //
        // tryOffer rather than offer: sendQueue is bounded, and the path that is already giving up must not itself
        // block behind a wedged writer. A dropped cancellation costs the server some wasted work, not correctness.
        //
        // Only where the connection is still usable. onError is left alone: the failure there may be the connection
        // dying, and closeQueues/failPending own that case.
        val abandon = sendQueue.tryOffer(Some(LspProtocol.cancelRequest(id))).void >> cleanup

        (sendQueue.offer(Some(request)) >> deferred.get.flatMap(IO.fromEither))
          .timeoutTo(timeout, abandon >> IO.raiseError(timeoutError))
          .onCancel(abandon)
          .onError(_ => cleanup)
    yield result

  def sendNotification(method: LspMethod, params: Json): IO[Unit] =
    sendQueue.offer(Some(LspProtocol.notification(method, params))).void

  def processIncoming(onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit]): IO[Unit] =
    Stream
      .fromQueueNoneTerminated(notifQueue)
      .evalMap { json =>
        LspProtocol.notificationMethod(json).map(_.value) match
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
    LspProtocol.classify(json) match
      case JsonRpcMessage.Response(id, result) =>
        completePending(id, Right(result))
      case JsonRpcMessage.ResponseError(id, code, message) =>
        logger.warn(s"[LSP] ${languageId.id} response error $code: $message") >>
          completePending(id, Left(LspConnection.LspResponseError(languageId, code, message)))
      case JsonRpcMessage.Notification(_, _) =>
        notifQueue.offer(Some(json))
      case JsonRpcMessage.Malformed(raw) =>
        logger.warn(s"[LSP] ${languageId.id} received an unrecognized JSON-RPC message: ${raw.noSpaces}")

  private def completePending(id: RequestId, result: Either[Throwable, Json]): IO[Unit] =
    pendingRef.modify { pending =>
      pending.get(id) match
        case Some(d) => (pending - id, d.complete(result).void)
        case None    => (pending, IO.unit)
    }.flatten

  private[lsp] def takeOutgoing: IO[Option[Json]] =
    sendQueue.take

  /** Non-blocking: `None` means nothing is queued right now, distinguishing "no message was ever sent" from "one is on
    * its way" without a time-based guess at how long to wait.
    */
  private[lsp] def tryTakeOutgoing: IO[Option[Option[Json]]] =
    sendQueue.tryTake

  private[lsp] def pendingRequestCount: IO[Int] =
    pendingRef.get.map(_.size)

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

  val DefaultRequestTimeout: FiniteDuration = 10.seconds

  final case class LspRequestTimeout(languageId: LanguageId, method: LspMethod, timeout: FiniteDuration)
      extends RuntimeException(s"LSP request timed out: ${languageId.id} ${method.value} after ${timeout.toMillis} ms")

  /** Surfaces a JSON-RPC error response (`{"error": {...}}`, no `result`) to whichever caller is awaiting that
    * request's `Deferred`, the same mechanism [[LspRequestTimeout]] uses -- rather than the previous behavior of
    * silently treating an error response the same as "no result".
    */
  final case class LspResponseError(languageId: LanguageId, code: Int, message: String)
      extends RuntimeException(s"LSP request failed: ${languageId.id} error $code: $message")

  final private case class ConnectionFibers(
      writer: Fiber[IO, Throwable, Unit],
      reader: Fiber[IO, Throwable, Unit]
  )

  private[lsp] def create(
    languageId: LanguageId,
    logger: Logger[IO],
    requestTimeout: FiniteDuration = DefaultRequestTimeout
  ): IO[LspConnection] =
    for
      sendQueue   <- Queue.bounded[IO, Option[Json]](256)
      idRef       <- Ref.of[IO, Long](0L)
      pendingRef  <- Ref.of[IO, Map[RequestId, Deferred[IO, Either[Throwable, Json]]]](Map.empty)
      notifQueue  <- Queue.bounded[IO, Option[Json]](256)
      syncKindRef <- Ref.of[IO, TextDocumentSyncKind](TextDocumentSyncKind.Full)
    yield new LspConnection(
      languageId,
      sendQueue,
      idRef,
      pendingRef,
      notifQueue,
      requestTimeout,
      logger,
      syncKindRef
    )

  // Package-visible entry point — accepts pre-opened streams; used by tests via MockLspServer.
  private[lsp] def connect(
    languageId: LanguageId,
    rawIn: java.io.InputStream,
    rawOut: java.io.OutputStream,
    rootUri: WorkspaceRootUri,
    logger: Logger[IO],
    requestTimeout: FiniteDuration = DefaultRequestTimeout
  ): Resource[IO, LspConnection] =
    for
      conn <- Resource.eval(create(languageId, logger, requestTimeout))
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
            .handleErrorWith(error => Stream.eval(logger.warn(error)("[LSP] reader stopped")))
            .compile
            .drain
            .guarantee(conn.closeQueues)
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
    rootUri: WorkspaceRootUri,
    logger: Logger[IO],
    requestTimeout: FiniteDuration = DefaultRequestTimeout
  ): Resource[IO, LspConnection] =
    for
      process <- Resource.make(
        IO.blocking(
          new java.lang.ProcessBuilder(
            (config.command :: config.defaultArgs).toArray*
          ).start()
        )
      )(proc => IO.blocking(proc.destroyForcibly()).void)
      conn <- connect(
        config.languageId,
        process.getInputStream,
        process.getOutputStream,
        rootUri,
        logger,
        requestTimeout
      )
    yield conn

  private def initHandshake(conn: LspConnection, rootUri: WorkspaceRootUri, logger: Logger[IO]): IO[Unit] =
    for
      pid <- IO(ProcessHandle.current().pid().toInt)
      _   <- logger.info(s"[LSP] initialize ${conn.languageId.id} rootUri=${rootUri.value}")
      initializeResult <- conn
        .sendRequest(LspMethod("initialize"), LspProtocol.initializeParams(pid, rootUri))
        .handleErrorWith(ex => logger.error(ex)("[LSP] initialize failed") >> IO.raiseError(ex))
      _ <- conn.setSyncKind(TextDocumentSyncKind.fromInitializeResult(initializeResult))
      _ <- conn.sendNotification(LspMethod("initialized"), LspProtocol.initializedParams)
      _ <- logger.info(s"[LSP] Handshake complete: ${conn.languageId.id}")
    yield ()

  private def closeQuietly(closeable: AutoCloseable): IO[Unit] =
    IO.blocking(closeable.close()).attempt.void
