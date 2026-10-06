package com.serenity.lsp.client

import java.io.{BufferedInputStream, BufferedOutputStream}

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import com.serenity.lsp.config.{LanguageId, LspServerConfig}
import com.serenity.lsp.model.{Diagnostic, SemanticTokensLegend, TextDocumentSyncKind}
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
    legendRef: Ref[IO, Option[SemanticTokensLegend]],
    requestTimeout: FiniteDuration,
    logger: Logger[IO],
    syncKindRef: Ref[IO, TextDocumentSyncKind],
    failNextNotificationRef: Ref[IO, Boolean],
    terminatedSignal: Deferred[IO, Unit],
    hooksRef: Ref[IO, LspClientHooks]
):

  /** The server's negotiated `textDocumentSync` capability, read off the `initialize` response during the handshake
    * (see `initHandshake`). Defaults to `Full` -- this client's behavior before that capability was read at all -- for
    * any connection that never runs the real handshake (e.g. one built directly via `create` in tests).
    */
  def syncKind: IO[TextDocumentSyncKind] = syncKindRef.get

  private[lsp] def setSyncKind(kind: TextDocumentSyncKind): IO[Unit] = syncKindRef.set(kind)

  /** Test support: makes the next `sendNotification` fail instead of reaching the wire, to exercise callers' handling
    * of a failed notification (e.g. `LspManager`'s didChange mirror) without a real transport failure. Consumed on
    * first use.
    */
  private[lsp] def failNextNotification: IO[Unit] = failNextNotificationRef.set(true)

  /** Completes once the connection can no longer carry messages: the server exited or closed its end, the writer
    * failed, or the connection was released.
    */
  def terminated: IO[Unit] = terminatedSignal.get

  private def failIfTerminated: IO[Unit] =
    terminatedSignal.tryGet.flatMap(_.fold(IO.unit)(_ => IO.raiseError(LspConnection.LspConnectionClosed(languageId))))

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

        // Checked after the id is pending, so a close racing this request either fails it here or in failPending.
        (failIfTerminated >> sendQueue.offer(Some(request)) >> deferred.get.flatMap(IO.fromEither))
          .timeoutTo(timeout, abandon >> IO.raiseError(timeoutError))
          .onCancel(abandon)
          .onError(_ => cleanup)
    yield result

  def sendNotification(method: LspMethod, params: Json): IO[Unit] =
    failNextNotificationRef.getAndSet(false).flatMap {
      case true  => IO.raiseError(new RuntimeException(s"Simulated notification failure for ${languageId.id}"))
      case false => enqueueNotification(LspProtocol.notification(method, params))
    }

  /** Fails instead of waiting for room: `LspManager` sends notifications from the one fiber that serves every document,
    * so a server that stopped reading must cost its own documents a notification, not stall everyone else's.
    */
  private def enqueueNotification(notification: Json): IO[Unit] =
    failIfTerminated >>
      sendQueue
        .tryOffer(Some(notification))
        .flatMap(IO.raiseUnless(_)(LspConnection.LspOutgoingQueueFull(languageId)))

  /** Installed before the first server message can be read, so a request that arrives straight after the handshake is
    * answered by the application rather than by the placeholder hooks.
    */
  def useHooks(hooks: LspClientHooks): IO[Unit] = hooksRef.set(hooks)

  def processIncoming(
    onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit],
    hooks: LspClientHooks = LspClientHooks.ignoring
  ): IO[Unit] =
    useHooks(hooks) >>
      Stream
        .fromQueueNoneTerminated(notifQueue)
        .evalMap(notification => handleNotification(notification, onDiagnostics, hooks))
        .compile
        .drain

  private def handleNotification(
    json: Json,
    onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit],
    hooks: LspClientHooks
  ): IO[Unit] =
    val params = json.hcursor.downField("params").focus.getOrElse(Json.Null)
    LspProtocol.notificationMethod(json).map(_.value) match
      case Some("textDocument/publishDiagnostics") =>
        LspProtocol.parseDiagnostics(json) match
          case Some((uri, diags)) => onDiagnostics(uri, diags)
          case None               => logger.warn("[LSP] Could not parse publishDiagnostics")
      case Some("window/showMessage") =>
        LspClientHooks.parseMessage(params).traverse_(hooks.onMessage)
      case Some("window/logMessage") =>
        LspClientHooks.parseMessage(params).map(_.copy(shownToUser = false)).traverse_(hooks.onMessage)
      case Some("$/progress") =>
        LspClientHooks.parseProgress(params).traverse_(hooks.onProgress)
      case Some(method) =>
        logger.debug(s"[LSP] Notification: $method")
      case None => IO.unit

  private[lsp] def handleIncomingJson(json: Json): IO[Unit] =
    LspProtocol.classify(json) match
      case JsonRpcMessage.Response(id, result) =>
        completePending(id, Right(result))
      case JsonRpcMessage.ResponseError(id, code, message) =>
        logger.warn(s"[LSP] ${languageId.id} response error $code: $message") >>
          completePending(id, Left(LspConnection.LspResponseError(languageId, code, message)))
      case JsonRpcMessage.Notification(_, _) =>
        notifQueue.offer(Some(json))
      case JsonRpcMessage.ServerRequest(id, method, params) =>
        logger.debug(s"[LSP] ${languageId.id} server request: ${method.value}") >>
          handleServerRequest(id, method, params)
      case JsonRpcMessage.Malformed(raw) =>
        logger.warn(s"[LSP] ${languageId.id} received an unrecognized JSON-RPC message: ${raw.noSpaces}")

  private def handleServerRequest(id: ServerRequestId, method: LspMethod, params: Json): IO[Unit] =
    method.value match
      case "workspace/applyEdit" =>
        // Applying an edit waits on the application, so it must not hold up the reader that delivers every response.
        applyEdit(id, params).start.void
      case "window/showMessageRequest" =>
        // The user may take a while to answer, and every response is read by the same reader.
        showMessageRequest(id, params).start.void
      case _ =>
        answerServerRequest(LspServerRequests.reply(id, method, params))

  private def showMessageRequest(id: ServerRequestId, params: Json): IO[Unit] =
    val chosen = LspClientHooks.parseMessageRequest(params).fold(IO.pure(Option.empty[Json])) { question =>
      hooksRef.get
        .flatMap(_.onMessageRequest(question))
        .timeoutTo(requestTimeout, IO.pure(None))
        .handleError(_ => None)
        .map(_.flatMap(question.actions.lift).map(_.item))
    }
    chosen.flatMap(item => answerServerRequest(LspProtocol.response(id, item.getOrElse(Json.Null))))

  private def applyEdit(id: ServerRequestId, params: Json): IO[Unit] =
    val outcome = LspClientHooks.parseApplyEdit(params) match
      case None => IO.pure(LspApplyEditResult(applied = false, Some("Malformed workspace/applyEdit parameters")))
      case Some(request) =>
        hooksRef.get
          .flatMap(_.onApplyEdit(request))
          .timeoutTo(
            requestTimeout,
            IO.pure(LspApplyEditResult(applied = false, Some("Timed out applying the edit")))
          )
          .handleError(error =>
            LspApplyEditResult(applied = false, Some(s"Applying the edit failed: ${error.getMessage}"))
          )
    outcome.flatMap(result => answerServerRequest(LspProtocol.response(id, LspClientHooks.applyEditResponse(result))))

  private def answerServerRequest(reply: Json): IO[Unit] =
    sendQueue
      .offer(Some(reply))
      .timeoutTo(
        requestTimeout,
        logger.warn(s"[LSP] ${languageId.id} could not answer a server request: writer stalled")
      )

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

  /** The server's semantic tokens legend, captured off its `initialize` result during the handshake (see
    * [[LspConnection.initHandshake]]) -- `None` when the server never declared `semanticTokensProvider`, or (in tests)
    * when nothing has recorded one yet.
    */
  private[lsp] def semanticTokensLegend: IO[Option[SemanticTokensLegend]] =
    legendRef.get

  private[lsp] def recordSemanticTokensLegend(legend: Option[SemanticTokensLegend]): IO[Unit] =
    legendRef.set(legend)

  private[lsp] def outgoingMessages: Stream[IO, Json] =
    Stream.fromQueueNoneTerminated(sendQueue)

  /** Idempotent. The end-of-stream markers are offered without waiting, since a full queue here belongs to a writer or
    * consumer that is already stuck and is about to be cancelled.
    */
  private[lsp] def closeQueues: IO[Unit] =
    terminatedSignal.complete(()).void >>
      failPending(LspConnection.LspConnectionClosed(languageId)) >>
      sendQueue.tryOffer(None).void >>
      notifQueue.tryOffer(None).void

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

  final case class LspConnectionClosed(languageId: LanguageId)
      extends RuntimeException(s"LSP connection closed for ${languageId.id}")

  final case class LspOutgoingQueueFull(languageId: LanguageId)
      extends RuntimeException(s"LSP outgoing queue full for ${languageId.id}: the server is not reading")

  /** How long a release waits for already-queued messages (such as `exit`) to reach the server before closing its
    * streams.
    */
  private val OutgoingFlushTimeout: FiniteDuration = 1.second

  private val StderrDrainShutdown: FiniteDuration = 1.second

  /** Total time a release gives a server to answer `shutdown` and take `exit` before the process is destroyed. */
  private val ServerShutdownTimeout: FiniteDuration = 2.seconds

  /** How long a destroyed process gets to exit before it is destroyed forcibly. */
  private val ServerDestroyGrace: FiniteDuration = 1.second

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
      sendQueue               <- Queue.bounded[IO, Option[Json]](256)
      idRef                   <- Ref.of[IO, Long](0L)
      pendingRef              <- Ref.of[IO, Map[RequestId, Deferred[IO, Either[Throwable, Json]]]](Map.empty)
      notifQueue              <- Queue.bounded[IO, Option[Json]](256)
      legendRef               <- Ref.of[IO, Option[SemanticTokensLegend]](None)
      syncKindRef             <- Ref.of[IO, TextDocumentSyncKind](TextDocumentSyncKind.Full)
      failNextNotificationRef <- Ref.of[IO, Boolean](false)
      terminatedSignal        <- Deferred[IO, Unit]
      hooksRef                <- Ref.of[IO, LspClientHooks](LspClientHooks.ignoring)
    yield new LspConnection(
      languageId,
      sendQueue,
      idRef,
      pendingRef,
      notifQueue,
      legendRef,
      requestTimeout,
      logger,
      syncKindRef,
      failNextNotificationRef,
      terminatedSignal,
      hooksRef
    )

  // Package-visible entry point — accepts pre-opened streams; used by tests via MockLspServer.
  private[lsp] def connect(
    languageId: LanguageId,
    rawIn: java.io.InputStream,
    rawOut: java.io.OutputStream,
    rootUri: WorkspaceRootUri,
    logger: Logger[IO],
    requestTimeout: FiniteDuration = DefaultRequestTimeout,
    askServerToExit: LspConnection => IO[Unit] = _ => IO.unit,
    endServer: IO[Unit] = IO.unit
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
            .handleErrorWith(error => logger.warn(error)("[LSP] writer stopped"))
            .guarantee(conn.closeQueues)
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
          askServerToExit(conn) >>
            conn.closeQueues >>
            writerFiber.join.void.timeoutTo(OutgoingFlushTimeout, IO.unit) >>
            closeQuietly(out) >>
            endServer >>
            closeQuietly(in) >>
            writerFiber.cancel >>
            readerFiber.cancel
      }
      _ <- Resource.eval(initHandshake(conn, rootUri, logger))
    yield conn

  def apply(
    config: LspServerConfig,
    rootUri: WorkspaceRootUri,
    logger: Logger[IO],
    requestTimeout: FiniteDuration = DefaultRequestTimeout,
    stderrLogDirectory: java.nio.file.Path = LspStderrLog.defaultDirectory,
    stderrLogMaxBytes: Long = LspStderrLog.DefaultMaxBytes
  ): Resource[IO, LspConnection] =
    for
      server <- Resource.make(
        IO.blocking(new java.lang.ProcessBuilder((config.command :: config.defaultArgs).toArray*).start())
          .flatMap(process =>
            LspStderrLog
              .drain(process.getErrorStream, config.languageId, stderrLogDirectory, stderrLogMaxBytes, logger)
              .handleErrorWith(error => logger.warn(error)(s"[LSP] ${config.languageId.id} stderr reader stopped"))
              .start
              .map(drain => RunningServer(process, drain))
          )
      )(stop)
      conn <- connect(
        config.languageId,
        server.process.getInputStream,
        server.process.getOutputStream,
        rootUri,
        logger,
        requestTimeout,
        askServerToExit = askToExit,
        endServer = endProcess(server.process)
      )
    yield conn

  final private case class RunningServer(process: java.lang.Process, stderrDrain: Fiber[IO, Throwable, Unit])

  /** The process is destroyed before the drain is cancelled: the drain sits in a blocking read that only the process
    * dying ends. A grandchild that inherited stderr can keep the pipe open past that, so the wait is bounded and the
    * drain left to finish on its own.
    */
  private def stop(server: RunningServer): IO[Unit] =
    IO.blocking(server.process.destroyForcibly()).void >>
      server.stderrDrain.cancel.timeoutTo(StderrDrainShutdown, IO.unit)

  /** `shutdown` then `exit`, together bounded by [[ServerShutdownTimeout]] so a server that never answers cannot hold
    * the release.
    */
  private def askToExit(conn: LspConnection): IO[Unit] =
    (conn.sendRequest(LspMethod("shutdown"), Json.Null, ServerShutdownTimeout).attempt >>
      conn.sendNotification(LspMethod("exit"), Json.Null).attempt).void
      .timeoutTo(ServerShutdownTimeout, IO.unit)

  /** Ends the process so the reader's blocking read of its stdout returns: a well-behaved server has already exited
    * after `exit`; otherwise it is destroyed, then destroyed forcibly once the grace has passed.
    */
  private def endProcess(process: java.lang.Process): IO[Unit] =
    val exited = IO.blocking(process.waitFor(ServerDestroyGrace.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS))
    exited.flatMap:
      case true => IO.unit
      case false =>
        IO.blocking(process.destroy()) >>
          exited.flatMap(gone => IO.blocking(process.destroyForcibly()).void.unlessA(gone)) >>
          exited.void

  private def initHandshake(conn: LspConnection, rootUri: WorkspaceRootUri, logger: Logger[IO]): IO[Unit] =
    for
      pid <- IO(ProcessHandle.current().pid().toInt)
      _   <- logger.info(s"[LSP] initialize ${conn.languageId.id} rootUri=${rootUri.value}")
      initializeResult <- conn
        .sendRequest(LspMethod("initialize"), LspProtocol.initializeParams(pid, rootUri))
        .handleErrorWith(ex => logger.error(ex)("[LSP] initialize failed") >> IO.raiseError(ex))
      _ <- conn.recordSemanticTokensLegend(LspProtocol.parseSemanticTokensLegend(initializeResult))
      _ <- conn.setSyncKind(TextDocumentSyncKind.fromInitializeResult(initializeResult))
      _ <- conn.sendNotification(LspMethod("initialized"), LspProtocol.initializedParams)
      _ <- logger.info(s"[LSP] Handshake complete: ${conn.languageId.id}")
    yield ()

  private def closeQuietly(closeable: AutoCloseable): IO[Unit] =
    IO.blocking(closeable.close()).attempt.void
