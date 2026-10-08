package com.serenity.lsp

import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.diagnostics.Trace
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.OpenDocument.{changedWithoutConnection, served, textOf, unserved}
import com.serenity.lsp.client.{DocumentUri, LspConnection, LspMethod, LspProtocol, WorkspaceRootUri}
import com.serenity.lsp.config.*
import com.serenity.state.models.CursorPosition
import fs2.Stream
import org.typelevel.log4cats.Logger

object LspManager:

  final private[lsp] case class ConnectionIdentity(rootUri: WorkspaceRootUri, serverConfig: LspServerConfig)
  final private[lsp] case class ResolvedConnection(identity: ConnectionIdentity, resource: Resource[IO, LspConnection])

  final private[lsp] case class ManagedConnection(connection: LspConnection, release: IO[Unit])

  private[lsp] enum RequestKind:
    case Hover, Definition, References, Rename, Completion, SemanticTokens

    /** The visible-lines request that runs beside [[SemanticTokens]], or alone for a server offering only `range`. */
    case SemanticTokensRange

    /** The timer a refresh waits on, kept as a request so a newer edit cancels it with the rest. */
    case SemanticTokensDebounce

  final private[lsp] case class RequestKey(uri: DocumentUri, kind: RequestKind)

  /** Distinguishes the cursor-anchored request kinds (Hover, Completion, Definition) from the whole-document ones
    * (SemanticTokens), so `startRequest`'s shared parameter shape doesn't force a placeholder cursor position onto a
    * request that has no cursor to anchor to (#1507).
    */
  private[lsp] enum RequestAnchor:
    case CursorAnchored(position: CursorPosition)
    case WholeDocument

    def cursorPosition: Option[CursorPosition] = this match
      case CursorAnchored(position) => Some(position)
      case WholeDocument            => None

  final private[lsp] case class RequestContext(version: Int, anchor: RequestAnchor)

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

    /** Drop every cached resolution, for the editor letting every server go. No-op by default, like the above. */
    def evictAllResolutions: IO[Unit] = IO.unit

  def run(
    effects: Stream[IO, LspEffect],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    userConfig: LspUserConfig = LspUserConfig.empty,
    notices: LspNotices = LspNotices.ignoring
  ): IO[Unit] =
    LspResolutionCache.empty.flatMap { resolutionCache =>
      runWithProvider(
        effects,
        applyEvent,
        logger,
        connectionProvider(userConfig, logger, resolutionCache),
        notices = notices
      )
    }

  private[lsp] def runWithProvider(
    effects: Stream[IO, LspEffect],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider,
    policy: LspSupervisionPolicy = LspSupervisionPolicy.Default,
    notices: LspNotices = LspNotices.ignoring,
    semanticTokensDebounce: FiniteDuration = LspManagerSemanticTokens.DefaultDebounce
  ): IO[Unit] =
    Supervisor[IO].allocated.flatMap {
      case (supervisor, releaseRequests) =>
        for
          serverNotices    <- LspServerNotices.create(notices)
          documentVersions <- Ref.of[IO, Map[DocumentUri, Int]](Map.empty)
          openDocuments    <- Ref.of[IO, OpenDocument.Registry](Map.empty)
          requestContexts  <- Ref.of[IO, Map[RequestKey, RequestContext]](Map.empty)
          requestFibers    <- Ref.of[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]](Map.empty)
          pool <- LspConnectionPool
            .create(
              documentVersions,
              openDocuments,
              supervisor,
              connectionProvider,
              policy,
              serverNotices,
              applyEvent,
              logger
            )
          semanticTokensSession <- SemanticTokensSession.create
          semanticTokens = LspManagerSemanticTokens(
            pool,
            documentVersions,
            openDocuments,
            requestContexts,
            requestFibers,
            supervisor,
            applyEvent,
            logger,
            semanticTokensSession,
            semanticTokensDebounce
          )
          runEffects = effects
            .evalMap(effect =>
              pool.serialized(
                handleEffect(
                  effect,
                  pool,
                  documentVersions,
                  openDocuments,
                  requestContexts,
                  requestFibers,
                  supervisor,
                  semanticTokens,
                  applyEvent,
                  logger
                )
              )
            )
            .compile
            .drain
          _ <- runEffects.guarantee(releaseRequests >> pool.releaseAll)
        yield ()
    }

  private def handleEffect(
    effect: LspEffect,
    pool: LspConnectionPool,
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    openDocuments: Ref[IO, OpenDocument.Registry],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    semanticTokens: LspManagerSemanticTokens,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO]
  ): IO[Unit] =
    given Logger[IO] = logger
    effect match
      case LspEffect.FileOpened(rawUri, languageId, text) =>
        val uri = DocumentUri(rawUri)
        invalidateDocument(uri, requestContexts, requestFibers) >>
          semanticTokens.documentOpened(uri) >>
          documentVersions.update(_ + (uri -> 1)) >>
          openDocuments.update(_.served(uri, text)) >>
          pool.attach(languageId, uri).flatMap {
            case LspAttachment.Live(identity, conn) =>
              pool.associate(uri, identity) >>
                conn
                  .sendNotification(
                    LspMethod("textDocument/didOpen"),
                    LspProtocol.didOpenParams(uri, languageId.id, 1, text.collect())
                  )
                  .handleErrorWith(ex => logger.error(ex)(s"[LSP] didOpen failed: $rawUri")) >>
                semanticTokens.refreshNow(rawUri, languageId)
            case LspAttachment.Suspended(identity) =>
              // Its server is down; the restart reopens every associated document with its latest text.
              pool.associate(uri, identity) >> applyEvent(LspEvent.LspSemanticTokensUnavailable(rawUri))
            case LspAttachment.NoServer =>
              // No server for this language at all -- distinct from a request still in flight, so the renderer
              // shows the confirmed "unavailable" style rather than staying stuck in the neutral loading state
              // forever (issue #859/#1177 rendering-slice review finding).
              openDocuments.update(_.unserved(uri)) >>
                logger.debug(s"[LSP] No server for ${languageId.id}, skipping didOpen") >>
                applyEvent(LspEvent.LspSemanticTokensUnavailable(rawUri))
          }

      case LspEffect.FileChanged(rawUri, languageId, text, version) =>
        val uri = DocumentUri(rawUri)
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ + (uri -> version)) >>
          pool
            .connectionFor(uri)
            .flatMap {
              case Some(connection) =>
                openDocuments.get.map(_.textOf(uri)).flatMap { previous =>
                  LspDidChange.send(connection, uri, version, text, openDocuments, logger).flatMap {
                    case true =>
                      openDocuments.update(_.served(uri, text)) >>
                        previous.traverse_(semanticTokens.documentEdited(rawUri, _, text))
                    case false => IO.unit
                  }
                } >> semanticTokens.refreshAfterQuiet(rawUri, languageId)
              case None =>
                // Already reported by FileOpened or this document's first edit; a repeat is a commit per keystroke.
                pool.hasServer(uri).flatMap { awaitsServer =>
                  openDocuments
                    .modify(_.changedWithoutConnection(uri, text, awaitsServer))
                    .flatMap(reported => applyEvent(LspEvent.LspSemanticTokensUnavailable(rawUri)).unlessA(reported))
                }
            }

      case LspEffect.FileClosed(rawUri, languageId) =>
        val uri = DocumentUri(rawUri)
        invalidateDocument(uri, requestContexts, requestFibers) >>
          semanticTokens.documentClosed(uri) >>
          documentVersions.update(_ - uri) >>
          openDocuments.update(_ - uri) >>
          pool.evictResolution(languageId, uri) >>
          pool.connectionFor(uri).flatMap {
            case Some(connection) =>
              connection
                .sendNotification(LspMethod("textDocument/didClose"), LspProtocol.didCloseParams(uri))
                .handleErrorWith(ex => logger.error(ex)(s"[LSP] didClose failed: $rawUri"))
            case None => IO.unit
          } >> pool.release(uri)

      case LspEffect.HoverRequested(rawUri, languageId, line, character, anchor) =>
        val uri = DocumentUri(rawUri)
        startRequest(
          RequestKind.Hover,
          uri,
          languageId,
          RequestAnchor.CursorAnchored(anchor),
          pool,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent
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
          RequestAnchor.CursorAnchored(anchor),
          pool,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent
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
          RequestAnchor.CursorAnchored(anchor),
          pool,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent
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

      case LspEffect.ReferencesRequested(rawUri, languageId, line, character, anchor, symbol) =>
        LspManagerReferenceRenameSupport.requestReferences(
          rawUri,
          languageId,
          line,
          character,
          anchor,
          symbol,
          pool,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent,
          logger
        )
      case LspEffect.RenameRequested(rawUri, languageId, line, character, anchor, newName) =>
        LspManagerReferenceRenameSupport.requestRename(
          rawUri,
          languageId,
          line,
          character,
          anchor,
          newName,
          pool,
          documentVersions,
          requestContexts,
          requestFibers,
          supervisor,
          applyEvent,
          logger
        )

      case LspEffect.MessageRequestAnswered(prompt, choice) =>
        pool.notices.answer(prompt, choice)

      case LspEffect.SemanticTokensRequested(rawUri, languageId) =>
        semanticTokens.refreshNow(rawUri, languageId)

      case LspEffect.VisibleRangeChanged(rawUri, languageId, firstLine, lastLine) =>
        semanticTokens.visibleRangeChanged(rawUri, languageId, firstLine, lastLine)

      case LspEffect.ReleaseAll =>
        releaseAll(pool, documentVersions, openDocuments, requestContexts, requestFibers, semanticTokens)

  private[lsp] def startRequest(
    kind: RequestKind,
    uri: DocumentUri,
    languageId: LanguageId,
    anchor: RequestAnchor,
    pool: LspConnectionPool,
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    applyEvent: Event => IO[Unit]
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
        pool.attach(languageId, uri).flatMap {
          case LspAttachment.Live(_, conn) =>
            supervisor.supervise(request(conn, context)).flatMap(fiber => requestFibers.update(_.updated(key, fiber)))
          case LspAttachment.Suspended(_) | LspAttachment.NoServer =>
            noServerEvent(kind, uri, languageId, anchor).traverse_(applyEvent)
        }
    }

  /** The "no result" event for each request kind when no server is available, matching what each kind already emits for
    * a real, connected server's empty/absent result: Completion already emits `LspCompletionReceived(Nil, _)` for an
    * empty item list (rendered as "No completions available." by `SystemEventReducer`), and Definition's "not found"
    * case is already silently absorbed (no event) above in its own response handling -- there is no `LspEvent` shape
    * for "no definition found". References and Rename follow the same "no event" policy as Definition, for the same
    * reason. Hover is the one kind whose result is free-form text, so it alone gets an explanatory message here rather
    * than staying silent. SemanticTokens gets [[LspEvent.LspSemanticTokensUnavailable]] -- `None` here would leave
    * `AppState.semanticTokensAvailability` stuck at `Pending` forever for a document whose language has no server at
    * all, never reaching the confirmed `Unavailable` state (issue #859/#1177 rendering-slice review finding).
    */
  private def noServerEvent(
    kind: RequestKind,
    uri: DocumentUri,
    languageId: LanguageId,
    anchor: RequestAnchor
  ): Option[LspEvent] =
    kind match
      case RequestKind.Hover =>
        anchor.cursorPosition.map(position =>
          LspEvent.LspHoverReceived(s"No LSP server available for ${languageId.displayName}", position)
        )
      case RequestKind.Completion =>
        anchor.cursorPosition.map(position => LspEvent.LspCompletionReceived(Nil, position))
      case RequestKind.Definition => None
      case RequestKind.References => None
      case RequestKind.Rename     => None
      case RequestKind.SemanticTokens | RequestKind.SemanticTokensRange =>
        Some(LspEvent.LspSemanticTokensUnavailable(uri.value))
      case RequestKind.SemanticTokensDebounce => None

  private[lsp] def isCurrent(
    key: RequestKey,
    context: RequestContext,
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]]
  ): IO[Boolean] =
    (documentVersions.get, requestContexts.get).mapN { (versions, contexts) =>
      versions.get(key.uri).contains(context.version) && contexts.get(key).contains(context)
    }

  private def releaseAll(
    pool: LspConnectionPool,
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    openDocuments: Ref[IO, OpenDocument.Registry],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    semanticTokens: LspManagerSemanticTokens
  ): IO[Unit] =
    for
      known <- (openDocuments.get.map(_.keySet), documentVersions.get.map(_.keySet)).mapN(_ ++ _)
      _     <- requestFibers.getAndSet(Map.empty).flatMap(_.values.toList.traverse_(_.cancel))
      _     <- requestContexts.set(Map.empty)
      _     <- known.toList.traverse_(semanticTokens.documentClosed)
      _     <- documentVersions.set(Map.empty) >> openDocuments.set(Map.empty)
      _     <- pool.releaseEverything
    yield ()

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

      override def evictAllResolutions: IO[Unit] = resolutionCache.evictAll

  private def uriToPath(uri: DocumentUri): String =
    val s = uri.value
    if s.startsWith("file://") then java.net.URI.create(s).getPath
    else s

  private def parentUri(uri: DocumentUri): WorkspaceRootUri =
    val s         = uri.value
    val lastSlash = s.lastIndexOf('/')
    WorkspaceRootUri(if lastSlash > 0 then s.substring(0, lastSlash) else s)
