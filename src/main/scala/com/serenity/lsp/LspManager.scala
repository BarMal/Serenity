package com.serenity.lsp

import scala.annotation.unused

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.diagnostics.Trace
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{DocumentUri, LspConnection, LspMethod, LspProtocol, WorkspaceRootUri}
import com.serenity.lsp.config.*
import com.serenity.lsp.model.TextDocumentSyncKind
import com.serenity.state.models.CursorPosition
import fs2.Stream
import org.typelevel.log4cats.Logger

object LspManager:

  final private[lsp] case class ConnectionIdentity(rootUri: WorkspaceRootUri, serverConfig: LspServerConfig)
  final private[lsp] case class ResolvedConnection(identity: ConnectionIdentity, resource: Resource[IO, LspConnection])

  final private case class ManagedConnection(connection: LspConnection, release: IO[Unit])

  private enum RequestKind:
    case Hover, Definition, Completion, SemanticTokens

  final private case class RequestKey(uri: DocumentUri, kind: RequestKind)

  /** `anchor` is meaningful only for the cursor-anchored request kinds handled today (Hover, Completion, Definition).
    * #1507: a whole-document request kind (e.g. semantic tokens, tracked in the still-unmerged #1506) has no cursor
    * position to give here, so `startRequest`'s shared parameter shape will need reworking -- into an ADT
    * distinguishing cursor-anchored from whole-document requests, or a separate dispatch path for whole-document ones
    * -- once that request kind actually exists in this file. Not done speculatively here: there is nothing today to
    * construct or test a whole-document case against.
    */
  final private case class RequestContext(version: Int, anchor: CursorPosition)

  private[lsp] trait ConnectionProvider:

    def resolve(
      languageId: LanguageId,
      fileUri: DocumentUri,
      onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
    ): IO[Option[ResolvedConnection]]

    /** Drop any cached resolution for a document that's closing, so a later reopen re-resolves the workspace root
      * instead of reusing a stale one. No-op by default -- only the real, cache-backed provider needs to do anything
      * here.
      *
      * Deliberately eager rather than tied to a config-change event (#1469): there is no config-reload effect in this
      * pipeline today -- `LspUserConfig` is fixed for the lifetime of a `run`/`runWithProvider` call -- so a resolution
      * can only actually go stale here because the *workspace root* for this uri changes between closes (a multi-root
      * workspace, or the project being reopened from a different root), not because server availability on PATH changes
      * mid-session. Eviction on close is cheap insurance against that: the cost of a false negative (skip
      * re-resolution, misroute requests to the wrong workspace's server) is worse than the cost of redoing a
      * PATH/filesystem walk the next time this uri is reopened, which is the uncommon case, not the hot path. If this
      * ever shows up as a real cost (e.g. rapid tab close/reopen against a slow filesystem), the fix is to key eviction
      * off an explicit config/workspace-change signal instead of every `FileClosed`, not to drop eviction altogether.
      */
    def evictResolution(@unused languageId: LanguageId, @unused fileUri: DocumentUri): IO[Unit] = IO.unit

  def run(
    effects: Stream[IO, LspEffect],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    userConfig: LspUserConfig = LspUserConfig.empty
  ): IO[Unit] =
    LspResolutionCache.empty.flatMap { resolutionCache =>
      runWithProvider(effects, applyEvent, logger, connectionProvider(userConfig, logger, resolutionCache))
    }

  private[lsp] def runWithProvider(
    effects: Stream[IO, LspEffect],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Unit] =
    Supervisor[IO].allocated.flatMap {
      case (supervisor, releaseRequests) =>
        for
          connectionsRef      <- Ref.of[IO, Map[ConnectionIdentity, ManagedConnection]](Map.empty)
          documentConnections <- Ref.of[IO, Map[DocumentUri, ConnectionIdentity]](Map.empty)
          documentVersions    <- Ref.of[IO, Map[DocumentUri, Int]](Map.empty)
          documentTexts       <- Ref.of[IO, Map[DocumentUri, String]](Map.empty)
          requestContexts     <- Ref.of[IO, Map[RequestKey, RequestContext]](Map.empty)
          requestFibers       <- Ref.of[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]](Map.empty)
          runEffects = effects
            .evalMap(
              handleEffect(
                _,
                connectionsRef,
                documentConnections,
                documentVersions,
                documentTexts,
                requestContexts,
                requestFibers,
                supervisor,
                applyEvent,
                logger,
                connectionProvider
              )
            )
            .compile
            .drain
          _ <- runEffects.guarantee(releaseRequests >> releaseConnections(connectionsRef, logger))
        yield ()
    }

  private def handleEffect(
    effect: LspEffect,
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    documentConnections: Ref[IO, Map[DocumentUri, ConnectionIdentity]],
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    documentTexts: Ref[IO, Map[DocumentUri, String]],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Unit] =
    given Logger[IO] = logger
    effect match
      case LspEffect.FileOpened(rawUri, languageId, text) =>
        val uri = DocumentUri(rawUri)
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ + (uri -> 1)) >>
          documentTexts.update(_ + (uri -> text)) >>
          ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, connectionProvider).flatMap {
            case Some((identity, conn)) =>
              associateDocument(uri, identity, documentConnections, connectionsRef, logger) >>
                conn
                  .sendNotification(
                    LspMethod("textDocument/didOpen"),
                    LspProtocol.didOpenParams(uri, languageId.id, 1, text)
                  )
                  .handleErrorWith(ex => logger.error(ex)(s"[LSP] didOpen failed: $rawUri")) >>
                requestSemanticTokens(
                  rawUri,
                  languageId,
                  connectionsRef,
                  documentVersions,
                  requestContexts,
                  requestFibers,
                  supervisor,
                  applyEvent,
                  logger,
                  connectionProvider
                )
            case None =>
              // No server for this language at all -- distinct from a request still in flight, so the renderer
              // shows the confirmed "unavailable" style rather than staying stuck in the neutral loading state
              // forever (issue #859/#1177 rendering-slice review finding).
              logger.debug(s"[LSP] No server for ${languageId.id}, skipping didOpen") >>
                applyEvent(LspEvent.LspSemanticTokensUnavailable(rawUri))
          }

      case LspEffect.FileChanged(rawUri, languageId, text, version) =>
        val uri = DocumentUri(rawUri)
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ + (uri -> version)) >>
          connectionForDocument(uri, documentConnections, connectionsRef)
            .flatMap {
              case Some(managed) =>
                sendDidChange(managed.connection, uri, version, text, documentTexts, logger).flatMap {
                  case true  => documentTexts.update(_ + (uri -> text))
                  case false => IO.unit
                } >>
                  requestSemanticTokens(
                    rawUri,
                    languageId,
                    connectionsRef,
                    documentVersions,
                    requestContexts,
                    requestFibers,
                    supervisor,
                    applyEvent,
                    logger,
                    connectionProvider
                  )
              case None =>
                documentTexts.update(_ + (uri -> text)) >>
                  applyEvent(LspEvent.LspSemanticTokensUnavailable(rawUri))
            }

      case LspEffect.FileClosed(rawUri, languageId) =>
        val uri = DocumentUri(rawUri)
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ - uri) >>
          documentTexts.update(_ - uri) >>
          connectionProvider.evictResolution(languageId, uri) >>
          connectionForDocument(uri, documentConnections, connectionsRef).flatMap {
            case Some(managed) =>
              managed.connection
                .sendNotification(LspMethod("textDocument/didClose"), LspProtocol.didCloseParams(uri))
                .handleErrorWith(ex => logger.error(ex)(s"[LSP] didClose failed: $rawUri"))
            case None => IO.unit
          } >> releaseDocument(uri, documentConnections, connectionsRef, logger)

      case LspEffect.HoverRequested(rawUri, languageId, line, character, anchor) =>
        val uri = DocumentUri(rawUri)
        startRequest(
          RequestKind.Hover,
          uri,
          languageId,
          anchor,
          connectionsRef,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent,
          logger,
          connectionProvider
        ) { (conn, context) =>
          Trace
            .timed(s"lsp.hover.$rawUri")(
              conn.sendRequest(LspMethod("textDocument/hover"), LspProtocol.hoverParams(uri, line, character))
            )
            .flatMap(response =>
              LspProtocol.parseHoverText(response).fold(IO.unit) { text =>
                isCurrent(RequestKey(uri, RequestKind.Hover), context, documentVersions, requestContexts)
                  .ifM(applyEvent(LspEvent.LspHoverReceived(text, anchor)), IO.unit)
              }
            )
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] hover failed: $rawUri"))
        }

      case LspEffect.CompletionRequested(rawUri, languageId, line, character, anchor) =>
        val uri = DocumentUri(rawUri)
        startRequest(
          RequestKind.Completion,
          uri,
          languageId,
          anchor,
          connectionsRef,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent,
          logger,
          connectionProvider
        ) { (conn, _) =>
          Trace
            .timed(s"lsp.completion.$rawUri")(
              conn.sendRequest(LspMethod("textDocument/completion"), LspProtocol.completionParams(uri, line, character))
            )
            .flatMap(response =>
              LspProtocol
                .parseCompletionItems(response)
                .fold(IO.unit)(items => applyEvent(LspEvent.LspCompletionReceived(items, anchor)))
            )
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] completion failed: $rawUri"))
        }

      case LspEffect.DefinitionRequested(rawUri, languageId, line, character, anchor, symbol) =>
        val uri = DocumentUri(rawUri)
        startRequest(
          RequestKind.Definition,
          uri,
          languageId,
          anchor,
          connectionsRef,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent,
          logger,
          connectionProvider
        ) { (conn, context) =>
          Trace
            .timed(s"lsp.definition.$rawUri")(
              conn.sendRequest(LspMethod("textDocument/definition"), LspProtocol.definitionParams(uri, line, character))
            )
            .flatMap(response =>
              LspProtocol.parseDefinitionLocation(response).fold(IO.unit) { location =>
                isCurrent(RequestKey(uri, RequestKind.Definition), context, documentVersions, requestContexts)
                  .ifM(
                    applyEvent(
                      LspEvent.LspDefinitionReceived(symbol, location.uri.value, location.range.start, anchor)
                    ),
                    IO.unit
                  )
              }
            )
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] definition failed: $rawUri"))
        }

      case LspEffect.SemanticTokensRequested(rawUri, languageId) =>
        requestSemanticTokens(
          rawUri,
          languageId,
          connectionsRef,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent,
          logger,
          connectionProvider
        )

  /** Requests `textDocument/semanticTokens/full` for `rawUri` if its connection's server declared the capability during
    * its handshake, decodes the response against the legend it captured then, and emits
    * [[LspEvent.LspSemanticTokensReceived]] -- discarding a stale response exactly like [[RequestKind.Definition]]'s
    * `isCurrent` check. Shared by the [[LspEffect.SemanticTokensRequested]] effect and the automatic re-request this
    * manager makes on every `FileOpened`/`FileChanged` (issue #859/#1177's rendering slice needs semantic tokens
    * refreshed on every edit, not only when something explicitly asks for them). The "no connection" case is handled by
    * [[startRequest]]'s shared fallback via [[noServerEvent]], which emits [[LspEvent.LspSemanticTokensUnavailable]]
    * for this request kind -- see that function's doc comment.
    */
  private def requestSemanticTokens(
    rawUri: String,
    languageId: LanguageId,
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Unit] =
    given Logger[IO] = logger
    val uri          = DocumentUri(rawUri)
    startRequest(
      RequestKind.SemanticTokens,
      uri,
      languageId,
      // Semantic tokens apply to the whole document, not a cursor position -- `anchor` here is never read back
      // out of `context` by the request lambda below, unlike hover/definition.
      anchor = CursorPosition(0, 0),
      connectionsRef,
      documentVersions,
      requestContexts,
      requestFibers,
      supervisor,
      applyEvent,
      logger,
      connectionProvider
    ) { (conn, context) =>
      Trace
        .timed(s"lsp.semanticTokens.$rawUri")(
          conn.semanticTokensLegend.flatMap {
            case None => IO.unit // server never declared semanticTokensProvider during its handshake
            case Some(legend) =>
              conn
                .sendRequest(LspMethod("textDocument/semanticTokens/full"), LspProtocol.semanticTokensParams(uri))
                .flatMap(response =>
                  LspProtocol.parseSemanticTokens(response, legend).fold(IO.unit) { tokens =>
                    isCurrent(
                      RequestKey(uri, RequestKind.SemanticTokens),
                      context,
                      documentVersions,
                      requestContexts
                    )
                      .ifM(applyEvent(LspEvent.LspSemanticTokensReceived(rawUri, tokens)), IO.unit)
                  }
                )
          }
        )
        .handleErrorWith(ex => logger.error(ex)(s"[LSP] semanticTokens failed: $rawUri"))
    }

  private def startRequest(
    kind: RequestKind,
    uri: DocumentUri,
    languageId: LanguageId,
    anchor: CursorPosition,
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  )(
    request: (LspConnection, RequestContext) => IO[Unit]
  ): IO[Unit] =
    documentVersions.get.map(_.getOrElse(uri, 1)).flatMap { version =>
      val key     = RequestKey(uri, kind)
      val context = RequestContext(version, anchor)
      requestContexts.update(_ + (key -> context)) >>
        // Cancelling the previous in-flight request for this key (hover only -- see below) must complete before the
        // new one is sent, not merely before this method returns: `Fiber#cancel` doesn't resolve until the
        // cancelled fiber has finished unwinding, so sequencing it ahead of the new `sendRequest` guarantees any
        // `$/cancelRequest` it triggers reaches the wire first. Racing the two (start the new fiber, cancel the old
        // one alongside it) leaves the order of those two queue offers to fiber scheduling, and the new caller has
        // no way to tell the resulting message apart from the request it is waiting for.
        //
        // Hover-only is intentional, not an oversight (#1450): hover requests are re-issued continuously as the
        // cursor moves within the same document version, so without eager cancellation a fast mouse can pile up
        // many concurrent hover round-trips against the server. Definition requests are one-shot, user-invoked
        // actions (an explicit "go to definition"); a second one for the same `key` before the first resolves is
        // rare, and when it does happen the stale result is still discarded correctly -- `requestContexts.update`
        // above already overwrote this key's context with the new anchor, so `isCurrent` for the first fiber's
        // response will find a context mismatch and no-op it (see `isCurrent`). Cancelling it too would only save
        // one redundant network round-trip in an uncommon case, at the cost of the same complexity hover already
        // carries here, so the asymmetry is left as-is.
        (if kind == RequestKind.Hover then
           requestFibers.modify(fibers => (fibers - key, fibers.get(key))).flatMap(_.traverse_(_.cancel))
         else IO.unit) >>
        ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, connectionProvider).flatMap {
          case Some((_, conn)) =>
            supervisor.supervise(request(conn, context)).flatMap(fiber => requestFibers.update(_.updated(key, fiber)))
          case None =>
            noServerEvent(kind, uri, languageId, anchor).traverse_(applyEvent)
        }
    }

  /** The "no result" event for each request kind when no server is available, matching what each kind already emits for
    * a real, connected server's empty/absent result: Completion already emits `LspCompletionReceived(Nil, _)` for an
    * empty item list (rendered as "No completions available." by `SystemEventReducer`), and Definition's "not found"
    * case is already silently absorbed (no event) above in its own response handling -- there is no `LspEvent` shape
    * for "no definition found". Hover is the one kind whose result is free-form text, so it alone gets an explanatory
    * message here rather than staying silent. SemanticTokens gets [[LspEvent.LspSemanticTokensUnavailable]] -- `None`
    * here would leave `AppState.semanticTokensAvailability` stuck at `Pending` forever for a document whose language
    * has no server at all, never reaching the confirmed `Unavailable` state (issue #859/#1177 rendering-slice review
    * finding).
    */
  private def noServerEvent(
    kind: RequestKind,
    uri: DocumentUri,
    languageId: LanguageId,
    anchor: CursorPosition
  ): Option[LspEvent] =
    kind match
      case RequestKind.Hover =>
        Some(LspEvent.LspHoverReceived(s"No LSP server available for ${languageId.displayName}", anchor))
      case RequestKind.Completion =>
        Some(LspEvent.LspCompletionReceived(Nil, anchor))
      case RequestKind.Definition =>
        None
      case RequestKind.SemanticTokens =>
        Some(LspEvent.LspSemanticTokensUnavailable(uri.value))

  /** Sends `didChange` using whatever sync kind the connection negotiated during `initialize` (#1468): a range-based
    * diff against the document's previous text for `Incremental`, the existing full-text notification for `Full`, and
    * nothing at all for `None` -- a server that opted out of document sync should not be sent notifications for it
    * regardless of how expensive skipping them is.
    *
    * Returns whether the caller's `documentTexts` mirror may now advance to `text`. Under `Incremental` sync that
    * mirror is the base the next diff is computed against, so if the notification failed to send, the server never saw
    * this version -- advancing the mirror anyway would compute the next diff against text the server doesn't have,
    * permanently desyncing client and server state.
    */
  private def sendDidChange(
    connection: LspConnection,
    uri: DocumentUri,
    version: Int,
    text: String,
    documentTexts: Ref[IO, Map[DocumentUri, String]],
    logger: Logger[IO]
  ): IO[Boolean] =
    connection.syncKind.flatMap {
      case TextDocumentSyncKind.None => IO.pure(true)
      case syncKind =>
        documentTexts.get.map(_.getOrElse(uri, text)).flatMap { previousText =>
          connection
            .sendNotification(
              LspMethod("textDocument/didChange"),
              LspProtocol.didChangeParams(uri, version, previousText, text, syncKind)
            )
            .as(true)
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] didChange failed: ${uri.value}").as(false))
        }
    }

  private def isCurrent(
    key: RequestKey,
    context: RequestContext,
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]]
  ): IO[Boolean] =
    (documentVersions.get, requestContexts.get).mapN { (versions, contexts) =>
      versions.get(key.uri).contains(context.version) && contexts.get(key).contains(context)
    }

  private def invalidateDocument(
    uri: DocumentUri,
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]]
  ): IO[Unit] =
    requestContexts.update(_.filterNot { case (key, _) => key.uri == uri }) >>
      requestFibers
        .modify { fibers =>
          val (stale, current) = fibers.partition { case (key, _) => key.uri == uri }
          (current, stale.values.toList)
        }
        .flatMap(_.traverse_(_.cancel))

  private def connectionForDocument(
    uri: DocumentUri,
    documentConnections: Ref[IO, Map[DocumentUri, ConnectionIdentity]],
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]]
  ): IO[Option[ManagedConnection]] =
    (documentConnections.get, connectionsRef.get).mapN { (documents, connections) =>
      documents.get(uri).flatMap(connections.get)
    }

  private def associateDocument(
    uri: DocumentUri,
    identity: ConnectionIdentity,
    documentConnections: Ref[IO, Map[DocumentUri, ConnectionIdentity]],
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    logger: Logger[IO]
  ): IO[Unit] =
    documentConnections
      .modify { documents =>
        val previous = documents.get(uri).filter(_ != identity)
        (documents.updated(uri, identity), previous)
      }
      .flatMap(_.traverse_(releaseIfUnreferenced(_, documentConnections, connectionsRef, logger)))

  private def releaseDocument(
    uri: DocumentUri,
    documentConnections: Ref[IO, Map[DocumentUri, ConnectionIdentity]],
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    logger: Logger[IO]
  ): IO[Unit] =
    documentConnections
      .modify(documents => (documents - uri, documents.get(uri)))
      .flatMap(_.traverse_(releaseIfUnreferenced(_, documentConnections, connectionsRef, logger)))

  private def releaseIfUnreferenced(
    identity: ConnectionIdentity,
    documentConnections: Ref[IO, Map[DocumentUri, ConnectionIdentity]],
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    logger: Logger[IO]
  ): IO[Unit] =
    documentConnections.get.flatMap { documents =>
      if documents.values.exists(_ == identity) then IO.unit
      else
        connectionsRef
          .modify(connections => (connections - identity, connections.get(identity)))
          .flatMap(_.traverse_(_.release.handleErrorWith(ex => logger.error(ex)("[LSP] release failed"))))
    }

  private def connectionProvider(
    userConfig: LspUserConfig,
    logger: Logger[IO],
    resolutionCache: LspResolutionCache
  ): ConnectionProvider =
    new ConnectionProvider:
      def resolve(
        languageId: LanguageId,
        fileUri: DocumentUri,
        onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
      ): IO[Option[ResolvedConnection]] =
        resolutionCache
          .resolve(languageId, fileUri) {
            LspServerRegistry.resolve(languageId, userConfig).flatMap {
              case None =>
                logger.info(s"[LSP] No server available for ${languageId.id}").as(None)
              case Some(config) =>
                val filePath = uriToPath(fileUri)
                WorkspaceRootDetector.detect(filePath, languageId).map { rootOpt =>
                  val rootUri = rootOpt.map(r => WorkspaceRootUri(r.toUri.toString)).getOrElse(parentUri(fileUri))
                  Some(config -> rootUri)
                }
            }
          }
          .map {
            case None => None
            case Some((config, rootUri)) =>
              Some(ResolvedConnection(ConnectionIdentity(rootUri, config), LspConnection(config, rootUri, logger)))
          }

      override def evictResolution(languageId: LanguageId, fileUri: DocumentUri): IO[Unit] =
        resolutionCache.evict(languageId, fileUri)

  private def ensureConnection(
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    languageId: LanguageId,
    fileUri: DocumentUri,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Option[(ConnectionIdentity, LspConnection)]] =
    val onDiagnostics = (uri: DocumentUri, diags: List[com.serenity.lsp.model.Diagnostic]) =>
      applyEvent(LspEvent.LspDiagnosticsReceived(uri.value, diags))
    connectionProvider.resolve(languageId, fileUri, onDiagnostics).flatMap {
      case None => IO.pure(None)
      case Some(resolved) =>
        connectionsRef.get.flatMap { connections =>
          connections.get(resolved.identity) match
            case Some(managed) => IO.pure(Some(resolved.identity -> managed.connection))
            case None          => spawnConnection(connectionsRef, resolved, onDiagnostics, logger)
        }
    }

  private def spawnConnection(
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    resolved: ResolvedConnection,
    onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit],
    logger: Logger[IO]
  ): IO[Option[(ConnectionIdentity, LspConnection)]] =
    resolved.resource.allocated
      .flatMap {
        case (conn, release) =>
          conn.processIncoming(onDiagnostics).start.flatMap { diagnosticsFiber =>
            val managed = ManagedConnection(
              connection = conn,
              release = release >> diagnosticsFiber.cancel
            )
            connectionsRef.update(_ + (resolved.identity -> managed)) >> IO.pure(Some(resolved.identity -> conn))
          }
      }
      .handleErrorWith(ex =>
        logger.error(ex)(s"[LSP] Failed to connect for ${resolved.identity.serverConfig.languageId.id}").as(None)
      )

  private def uriToPath(uri: DocumentUri): String =
    val s = uri.value
    if s.startsWith("file://") then java.net.URI.create(s).getPath
    else s

  private def parentUri(uri: DocumentUri): WorkspaceRootUri =
    val s         = uri.value
    val lastSlash = s.lastIndexOf('/')
    WorkspaceRootUri(if lastSlash > 0 then s.substring(0, lastSlash) else s)

  private def releaseConnections(
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    logger: Logger[IO]
  ): IO[Unit] =
    connectionsRef
      .modify(connections => (Map.empty, connections.values.toList))
      .flatMap(_.traverse_(managed => managed.release.handleErrorWith(ex => logger.error(ex)("[LSP] release failed"))))
