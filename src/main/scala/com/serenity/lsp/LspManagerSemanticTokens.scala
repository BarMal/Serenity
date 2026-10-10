package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Supervisor
import cats.effect.{Fiber, IO, Ref}
import cats.syntax.all.*
import com.serenity.diagnostics.Trace
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.LspManager.{RequestAnchor, RequestContext, RequestKey, RequestKind}
import com.serenity.lsp.OpenDocument.textOf
import com.serenity.lsp.client.{DocumentUri, LspConnection, LspMethod, LspProtocol}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{
  LineRange,
  PrimaryRequest,
  SemanticTokenData,
  SemanticTokensFeatures,
  SemanticTokensLegend,
  SemanticTokensPlan,
  SemanticTokensResult,
  TextChangeDiff
}
import com.serenity.rope.{Balance, ChangeSet, Rope}
import org.typelevel.log4cats.Logger

/** The result a server last gave for a document, which a later `full/delta` request is relative to. */
final private[lsp] case class TokenBaseline(resultId: String, data: IArray[Int])

/** Which refresh a document is waiting out the debounce delay for: one that asks for everything, or only for the lines
  * a scroll brought into view.
  */
private[lsp] enum PendingRefresh:
  case Everything, VisibleLines

/** What [[LspManagerSemanticTokens]] remembers per document between requests: the server's last result (the base of the
  * next delta), the lines the editor shows, and whether tokens are on screen -- with the document version a
  * whole-document result was shown for, if one was.
  */
final private[lsp] class SemanticTokensSession private (
    baselines: Ref[IO, Map[DocumentUri, TokenBaseline]],
    visible: Ref[IO, Map[DocumentUri, LineRange]],
    shown: Ref[IO, Map[DocumentUri, Option[Int]]],
    claimed: Ref[IO, Map[DocumentUri, LineRange]],
    pending: Ref[IO, Map[DocumentUri, PendingRefresh]]
):

  def baselineOf(uri: DocumentUri): IO[Option[TokenBaseline]] = baselines.get.map(_.get(uri))

  def visibleOf(uri: DocumentUri): IO[Option[LineRange]] = visible.get.map(_.get(uri))

  def isShown(uri: DocumentUri): IO[Boolean] = shown.get.map(_.contains(uri))

  def completeVersionOf(uri: DocumentUri): IO[Option[Int]] = shown.get.map(_.get(uri).flatten)

  /** A result without a `resultId` cannot be the base of a delta, so it clears the baseline instead. */
  def recordBaseline(uri: DocumentUri, resultId: Option[String], data: IArray[Int]): IO[Unit] =
    baselines.update(held => resultId.fold(held - uri)(id => held.updated(uri, TokenBaseline(id, data))))

  def setVisible(uri: DocumentUri, lines: LineRange): IO[Unit] = visible.update(_.updated(uri, lines))

  def markComplete(uri: DocumentUri, version: Int): IO[Unit] = shown.update(_.updated(uri, Some(version)))

  def markPartial(uri: DocumentUri): IO[Unit] = shown.update(held => held.updated(uri, held.get(uri).flatten))

  /** Whether the caller is the one to request `lines`: false while the same lines are already being requested. */
  def claimRange(uri: DocumentUri, lines: LineRange): IO[Boolean] =
    claimed.modify(held => if held.get(uri).contains(lines) then (held, false) else (held.updated(uri, lines), true))

  def releaseRange(uri: DocumentUri, lines: LineRange): IO[Unit] =
    claimed.update(held => if held.get(uri).contains(lines) then held - uri else held)

  def pendingOf(uri: DocumentUri): IO[Option[PendingRefresh]] = pending.get.map(_.get(uri))

  def markPending(uri: DocumentUri, refresh: PendingRefresh): IO[Unit] = pending.update(_.updated(uri, refresh))

  def clearPending(uri: DocumentUri): IO[Unit] = pending.update(_ - uri)

  def forgetResults(uri: DocumentUri): IO[Unit] =
    baselines.update(_ - uri) >> shown.update(_ - uri) >> claimed.update(_ - uri) >> pending.update(_ - uri)

  def forget(uri: DocumentUri): IO[Unit] = forgetResults(uri) >> visible.update(_ - uri)

private[lsp] object SemanticTokensSession:

  def create: IO[SemanticTokensSession] =
    for
      baselines <- Ref.of[IO, Map[DocumentUri, TokenBaseline]](Map.empty)
      visible   <- Ref.of[IO, Map[DocumentUri, LineRange]](Map.empty)
      shown     <- Ref.of[IO, Map[DocumentUri, Option[Int]]](Map.empty)
      claimed   <- Ref.of[IO, Map[DocumentUri, LineRange]](Map.empty)
      pending   <- Ref.of[IO, Map[DocumentUri, PendingRefresh]](Map.empty)
    yield new SemanticTokensSession(baselines, visible, shown, claimed, pending)

/** Semantic tokens for the documents [[LspManager]] serves: debounced requests, `full/delta` and `range` where the
  * server offers them, and tokens that move with the text between a local edit and the server's answer.
  *
  * Requests follow [[LspManager.startRequest]]'s lifecycle: the primary request, the range preview and the debounce
  * timer each have a request kind of their own, so a newer edit cancels whichever of them is still pending and a
  * response for an older document version is dropped by [[LspManager.isCurrent]]. Everything that starts a request runs
  * under the pool's lock, as an effect handler does.
  */
final private[lsp] class LspManagerSemanticTokens(
    pool: LspConnectionPool,
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    openDocuments: Ref[IO, OpenDocument.Registry],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    session: SemanticTokensSession,
    debounce: FiniteDuration
):

  private given Balance    = Balance.default
  private given Logger[IO] = logger

  /** What a server answered, still in the protocol's relative encoding. */
  final private case class Fetched(resultId: Option[String], data: IArray[Int])

  def documentOpened(uri: DocumentUri): IO[Unit] = session.forgetResults(uri)

  def documentClosed(uri: DocumentUri): IO[Unit] = session.forget(uri)

  /** Requests tokens now, without waiting for the document to go quiet. */
  def refreshNow(rawUri: String, languageId: LanguageId): IO[Unit] =
    val uri = DocumentUri(rawUri)
    for
      connection <- pool.connectionFor(uri)
      features   <- connection.fold(IO.pure(SemanticTokensFeatures.FullOnly))(_.semanticTokensFeatures)
      baseline   <- session.baselineOf(uri)
      visible    <- session.visibleOf(uri)
      lineCount  <- openDocuments.get.map(_.textOf(uri).fold(0)(_.lineCount))
      plan = SemanticTokensPlan.choose(features, baseline.map(_.resultId), lineCount, visible)
      _ <- plan.primary.traverse_(startPrimary(rawUri, languageId, _))
      _ <- plan.preview.traverse_(startPreview(rawUri, languageId, _, connected = connection.isDefined))
    yield ()

  /** Requests tokens once no further call has come for `debounce`: each call replaces the timer the last one started,
    * so a burst of edits makes one request.
    */
  def refreshAfterQuiet(rawUri: String, languageId: LanguageId): IO[Unit] =
    afterQuiet(rawUri, PendingRefresh.Everything)(refreshNow(rawUri, languageId))

  /** Asks only for the lines on screen once the viewport has stopped moving for `debounce`, reading them when the timer
    * fires so the newest are used.
    */
  private def refreshVisibleAfterQuiet(rawUri: String, languageId: LanguageId): IO[Unit] =
    afterQuiet(rawUri, PendingRefresh.VisibleLines)(refreshVisible(rawUri, languageId))

  private def afterQuiet(rawUri: String, refresh: PendingRefresh)(request: IO[Unit]): IO[Unit] =
    val uri = DocumentUri(rawUri)
    val key = RequestKey(uri, RequestKind.SemanticTokensDebounce)
    val delayed = (IO.sleep(debounce) >> session.clearPending(uri) >> pool.serialized(request))
      .handleErrorWith(ex => logger.error(ex)(s"[LSP] semanticTokens refresh failed: $rawUri"))
    for
      earlier <- requestFibers.modify(fibers => (fibers - key, fibers.get(key)))
      _       <- earlier.traverse_(_.cancel)
      _       <- session.markPending(uri, refresh)
      timer   <- supervisor.supervise(delayed)
      _       <- requestFibers.update(_.updated(key, timer))
    yield ()

  private def refreshVisible(rawUri: String, languageId: LanguageId): IO[Unit] =
    val uri = DocumentUri(rawUri)
    for
      connection <- pool.connectionFor(uri)
      visible    <- session.visibleOf(uri)
      _          <- visible.traverse_(startPreview(rawUri, languageId, _, connected = connection.isDefined))
    yield ()

  /** Moves the tokens on screen with the text that changed from `previous` to `next`, until the server answers for
    * `next`.
    */
  def documentEdited(rawUri: String, previous: Rope, next: Rope, known: Option[ChangeSet] = None): IO[Unit] =
    session
      .isShown(DocumentUri(rawUri))
      .ifM(
        IO.delay(known.fold(List(TextChangeDiff.diff(previous, next)))(TextChangeDiff.changes(previous, _))).flatMap {
          changes =>
            changes.traverse_(change =>
              applyEvent(LspEvent.LspSemanticTokensEdited(rawUri, change)).unlessA(
                change.text.isEmpty && change.rangeLength == 0
              )
            )
        },
        IO.unit
      )

  /** A server that offers `range` is asked for the lines scrolled into view when it has no answer for them: one that
    * offers nothing else has none until it is asked, and one that sends the whole document has none while that answer
    * is pending or the text has changed since. A refresh already waiting out its delay will read the newest lines
    * itself, so it is left to do so.
    */
  def visibleRangeChanged(rawUri: String, languageId: LanguageId, firstLine: Int, lastLine: Int): IO[Unit] =
    val uri = DocumentUri(rawUri)
    session.setVisible(uri, LineRange(firstLine, lastLine)) >>
      pool.connectionFor(uri).flatMap { connection =>
        connection.traverse_(
          _.semanticTokensFeatures.flatMap(features =>
            if !features.range then IO.unit
            else if !features.full then refreshAfterQuiet(rawUri, languageId)
            else scrolledWithFullOffered(rawUri, languageId)
          )
        )
      }

  private def scrolledWithFullOffered(rawUri: String, languageId: LanguageId): IO[Unit] =
    val uri = DocumentUri(rawUri)
    for
      version  <- documentVersions.get.map(_.get(uri))
      complete <- session.completeVersionOf(uri)
      pending  <- session.pendingOf(uri)
      _ <- refreshVisibleAfterQuiet(rawUri, languageId)
        .whenA(version != complete && !pending.contains(PendingRefresh.Everything))
    yield ()

  private def startRequest(kind: RequestKind, uri: DocumentUri, languageId: LanguageId)(
    request: (LspConnection, RequestContext) => IO[Unit]
  ): IO[Unit] =
    LspManager.startRequest(
      kind,
      uri,
      languageId,
      RequestAnchor.WholeDocument,
      pool,
      documentVersions,
      requestContexts,
      requestFibers,
      supervisor,
      applyEvent
    )(request)

  /** Runs `request` against the server's legend when it declared one and offers the kind of request `offers` names. */
  private def whenServed(connection: LspConnection)(offers: SemanticTokensFeatures => Boolean)(
    request: SemanticTokensLegend => IO[Unit]
  ): IO[Unit] =
    (connection.semanticTokensFeatures, connection.semanticTokensLegend).tupled.flatMap {
      case (features, Some(legend)) if offers(features) => request(legend)
      case _                                            => IO.unit
    }

  private def startPrimary(rawUri: String, languageId: LanguageId, primary: PrimaryRequest): IO[Unit] =
    val uri = DocumentUri(rawUri)
    startRequest(RequestKind.SemanticTokens, uri, languageId) { (connection, context) =>
      Trace
        .timed(s"lsp.semanticTokens.$rawUri")(
          whenServed(connection)(_.full) { legend =>
            fetch(connection, uri, primary).flatMap(_.traverse_(showPrimary(rawUri, uri, legend, context, _)))
          }
        )
        .handleErrorWith(ex => logger.error(ex)(s"[LSP] semanticTokens failed: $rawUri"))
    }

  /** The server's result is recorded as the base of the next delta even when it is stale -- it is still what the server
    * will compare that delta against -- but only a result for the current document version is shown.
    */
  private def showPrimary(
    rawUri: String,
    uri: DocumentUri,
    legend: SemanticTokensLegend,
    context: RequestContext,
    fetched: Fetched
  ): IO[Unit] =
    for
      isOpen <- documentVersions.get.map(_.contains(uri))
      _      <- session.recordBaseline(uri, fetched.resultId, fetched.data).whenA(isOpen)
      current <- LspManager.isCurrent(
        RequestKey(uri, RequestKind.SemanticTokens),
        context,
        documentVersions,
        requestContexts
      )
      _ <- (session.markComplete(uri, context.version) >>
        IO.delay(SemanticTokenData.decode(fetched.data, legend))
          .flatMap(tokens => applyEvent(LspEvent.LspSemanticTokensReceived(rawUri, tokens)))).whenA(current)
    yield ()

  private def fetch(connection: LspConnection, uri: DocumentUri, primary: PrimaryRequest): IO[Option[Fetched]] =
    primary match
      case PrimaryRequest.Full => fetchFull(connection, uri)
      case PrimaryRequest.Delta(previousResultId) =>
        fetchDelta(connection, uri, previousResultId)
          .handleErrorWith(ex =>
            logger.debug(ex)(s"[LSP] semanticTokens delta failed, requesting the full tokens: ${uri.value}").as(None)
          )
          .flatMap(_.fold(fetchFull(connection, uri))(delta => IO.pure(Some(delta))))

  private def fetchFull(connection: LspConnection, uri: DocumentUri): IO[Option[Fetched]] =
    connection
      .sendRequest(LspMethod("textDocument/semanticTokens/full"), LspProtocol.semanticTokensParams(uri))
      .map(response => fetchedFrom(LspProtocol.parseSemanticTokensResult(response)))

  /** `None` when the server could not answer relative to what this client holds, so the caller asks for everything. */
  private def fetchDelta(
    connection: LspConnection,
    uri: DocumentUri,
    previousResultId: String
  ): IO[Option[Fetched]] =
    connection
      .sendRequest(
        LspMethod("textDocument/semanticTokens/full/delta"),
        LspProtocol.semanticTokensDeltaParams(uri, previousResultId)
      )
      .flatMap(response =>
        LspProtocol.parseSemanticTokensResult(response) match
          case Some(SemanticTokensResult.Delta(resultId, edits)) =>
            session
              .baselineOf(uri)
              .map(
                _.filter(_.resultId == previousResultId)
                  .flatMap(baseline => SemanticTokensResult.applyEdits(baseline.data, edits))
                  .map(Fetched(resultId, _))
              )
          case other => IO.pure(fetchedFrom(other))
      )

  private def fetchedFrom(result: Option[SemanticTokensResult]): Option[Fetched] =
    result.collect { case SemanticTokensResult.Full(resultId, data) => Fetched(resultId, data) }

  /** Requests `lines` unless the same lines are already being requested, and cancels a request for others, which the
    * viewport has since left. A request that cannot reach a server is not claimed, as nothing would release it.
    */
  private def startPreview(rawUri: String, languageId: LanguageId, lines: LineRange, connected: Boolean): IO[Unit] =
    val uri = DocumentUri(rawUri)
    val key = RequestKey(uri, RequestKind.SemanticTokensRange)
    val send = startRequest(RequestKind.SemanticTokensRange, uri, languageId) { (connection, context) =>
      Trace
        .timed(s"lsp.semanticTokens.range.$rawUri")(
          whenServed(connection)(_.range) { legend =>
            connection
              .sendRequest(
                LspMethod("textDocument/semanticTokens/range"),
                LspProtocol.semanticTokensRangeParams(uri, lines.first, lines.last)
              )
              .flatMap(response =>
                LspProtocol
                  .parseSemanticTokens(response, legend)
                  .traverse_(showPreview(rawUri, uri, lines, context, _))
              )
          }
        )
        .handleErrorWith(ex => logger.error(ex)(s"[LSP] semanticTokens range failed: $rawUri"))
        .guarantee(session.releaseRange(uri, lines))
    }
    if !connected then send
    else
      session.claimRange(uri, lines).flatMap { claimed =>
        val supersede = requestFibers.modify(fibers => (fibers - key, fibers.get(key))).flatMap(_.traverse_(_.cancel))
        (supersede >> send).whenA(claimed)
      }

  /** A preview is shown only while no whole-document result has been shown for its version: it would otherwise replace
    * lines the fuller answer already settled.
    */
  private def showPreview(
    rawUri: String,
    uri: DocumentUri,
    lines: LineRange,
    context: RequestContext,
    tokens: SemanticTokenData
  ): IO[Unit] =
    (
      LspManager.isCurrent(
        RequestKey(uri, RequestKind.SemanticTokensRange),
        context,
        documentVersions,
        requestContexts
      ),
      session.completeVersionOf(uri)
    ).mapN((current, complete) => current && !complete.contains(context.version))
      .ifM(
        session.markPartial(uri) >>
          applyEvent(LspEvent.LspSemanticTokensRangeReceived(rawUri, lines.first, lines.last, tokens)),
        IO.unit
      )

private[lsp] object LspManagerSemanticTokens:

  val DefaultDebounce: FiniteDuration = 200.millis
