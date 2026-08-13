package com.serenity.lsp

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{LspConnection, LspProtocol}
import com.serenity.lsp.config.*
import com.serenity.state.models.CursorPosition
import fs2.Stream
import org.typelevel.log4cats.Logger

object LspManager:

  private[lsp] case class ConnectionIdentity(rootUri: String, serverConfig: LspServerConfig)
  private[lsp] case class ResolvedConnection(identity: ConnectionIdentity, resource: Resource[IO, LspConnection])

  private case class ManagedConnection(connection: LspConnection, release: IO[Unit])

  private enum RequestKind:
    case Hover, Definition

  private case class RequestKey(uri: String, kind: RequestKind)
  private case class RequestContext(version: Int, anchor: CursorPosition)

  private[lsp] trait ConnectionProvider:

    def resolve(
      languageId: LanguageId,
      fileUri: String,
      onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
    ): IO[Option[ResolvedConnection]]

    /** Drop any cached resolution for a document that's closing, so a later reopen re-resolves the workspace root
      * instead of reusing a stale one. No-op by default -- only the real, cache-backed provider needs to do anything
      * here.
      */
    def evictResolution(languageId: LanguageId, fileUri: String): IO[Unit] = IO.unit

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
          documentConnections <- Ref.of[IO, Map[String, ConnectionIdentity]](Map.empty)
          documentVersions    <- Ref.of[IO, Map[String, Int]](Map.empty)
          requestContexts     <- Ref.of[IO, Map[RequestKey, RequestContext]](Map.empty)
          requestFibers       <- Ref.of[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]](Map.empty)
          runEffects = effects
            .evalMap(
              handleEffect(
                _,
                connectionsRef,
                documentConnections,
                documentVersions,
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
    documentConnections: Ref[IO, Map[String, ConnectionIdentity]],
    documentVersions: Ref[IO, Map[String, Int]],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]],
    requestFibers: Ref[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Unit] =
    effect match
      case LspEffect.FileOpened(uri, languageId, text) =>
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ + (uri -> 1)) >>
          ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, connectionProvider).flatMap {
            case Some((identity, conn)) =>
              associateDocument(uri, identity, documentConnections, connectionsRef, logger) >>
                conn
                  .sendNotification("textDocument/didOpen", LspProtocol.didOpenParams(uri, languageId.id, 1, text))
                  .handleErrorWith(ex => logger.error(ex)(s"[LSP] didOpen failed: $uri"))
            case None =>
              logger.debug(s"[LSP] No server for ${languageId.id}, skipping didOpen")
          }

      case LspEffect.FileChanged(uri, languageId, text, version) =>
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ + (uri -> version)) >>
          connectionForDocument(uri, documentConnections, connectionsRef).flatMap {
            case Some(managed) =>
              managed.connection
                .sendNotification("textDocument/didChange", LspProtocol.didChangeParams(uri, version, text))
                .handleErrorWith(ex => logger.error(ex)(s"[LSP] didChange failed: $uri"))
            case None => IO.unit
          }

      case LspEffect.FileClosed(uri, languageId) =>
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ - uri) >>
          connectionProvider.evictResolution(languageId, uri) >>
          connectionForDocument(uri, documentConnections, connectionsRef).flatMap {
            case Some(managed) =>
              managed.connection
                .sendNotification("textDocument/didClose", LspProtocol.didCloseParams(uri))
                .handleErrorWith(ex => logger.error(ex)(s"[LSP] didClose failed: $uri"))
            case None => IO.unit
          } >> releaseDocument(uri, documentConnections, connectionsRef, logger)

      case LspEffect.HoverRequested(uri, languageId, line, character, anchor) =>
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
          conn
            .sendRequest("textDocument/hover", LspProtocol.textDocumentPositionParams(uri, line, character))
            .flatMap(response =>
              LspProtocol.parseHoverText(response).fold(IO.unit) { text =>
                isCurrent(RequestKey(uri, RequestKind.Hover), context, documentVersions, requestContexts)
                  .ifM(applyEvent(LspEvent.LspHoverReceived(text, anchor)), IO.unit)
              }
            )
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] hover failed: $uri"))
        }

      case LspEffect.CompletionRequested(uri, languageId, line, character, anchor) =>
        ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, connectionProvider).flatMap {
          case Some((_, conn)) =>
            conn
              .sendRequest("textDocument/completion", LspProtocol.completionParams(uri, line, character))
              .flatMap(response =>
                LspProtocol
                  .parseCompletionItems(response)
                  .fold(IO.unit)(items => applyEvent(LspEvent.LspCompletionReceived(items, anchor)))
              )
              .handleErrorWith(ex => logger.error(ex)(s"[LSP] completion failed: $uri"))
          case None =>
            applyEvent(LspEvent.LspHoverReceived(s"No LSP server available for ${languageId.displayName}", anchor))
        }

      case LspEffect.DefinitionRequested(uri, languageId, line, character, anchor, symbol) =>
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
          conn
            .sendRequest("textDocument/definition", LspProtocol.textDocumentPositionParams(uri, line, character))
            .flatMap(response =>
              LspProtocol.parseDefinitionLocation(response).fold(IO.unit) { location =>
                isCurrent(RequestKey(uri, RequestKind.Definition), context, documentVersions, requestContexts)
                  .ifM(
                    applyEvent(LspEvent.LspDefinitionReceived(symbol, location.uri, location.range.start, anchor)),
                    IO.unit
                  )
              }
            )
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] definition failed: $uri"))
        }

  private def startRequest(
    kind: RequestKind,
    uri: String,
    languageId: LanguageId,
    anchor: CursorPosition,
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    documentVersions: Ref[IO, Map[String, Int]],
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
        ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, connectionProvider).flatMap {
          case Some((_, conn)) =>
            supervisor.supervise(request(conn, context)).flatMap { fiber =>
              requestFibers
                .modify(fibers => (fibers.updated(key, fiber), fibers.get(key)))
                .flatMap(previous =>
                  if kind == RequestKind.Hover then previous.traverse_(_.cancel)
                  else IO.unit
                )
            }
          case None =>
            applyEvent(LspEvent.LspHoverReceived(s"No LSP server available for ${languageId.displayName}", anchor))
        }
    }

  private def isCurrent(
    key: RequestKey,
    context: RequestContext,
    documentVersions: Ref[IO, Map[String, Int]],
    requestContexts: Ref[IO, Map[RequestKey, RequestContext]]
  ): IO[Boolean] =
    (documentVersions.get, requestContexts.get).mapN { (versions, contexts) =>
      versions.get(key.uri).contains(context.version) && contexts.get(key).contains(context)
    }

  private def invalidateDocument(
    uri: String,
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
    uri: String,
    documentConnections: Ref[IO, Map[String, ConnectionIdentity]],
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]]
  ): IO[Option[ManagedConnection]] =
    (documentConnections.get, connectionsRef.get).mapN { (documents, connections) =>
      documents.get(uri).flatMap(connections.get)
    }

  private def associateDocument(
    uri: String,
    identity: ConnectionIdentity,
    documentConnections: Ref[IO, Map[String, ConnectionIdentity]],
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
    uri: String,
    documentConnections: Ref[IO, Map[String, ConnectionIdentity]],
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    logger: Logger[IO]
  ): IO[Unit] =
    documentConnections
      .modify(documents => (documents - uri, documents.get(uri)))
      .flatMap(_.traverse_(releaseIfUnreferenced(_, documentConnections, connectionsRef, logger)))

  private def releaseIfUnreferenced(
    identity: ConnectionIdentity,
    documentConnections: Ref[IO, Map[String, ConnectionIdentity]],
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
        fileUri: String,
        onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
      ): IO[Option[ResolvedConnection]] =
        resolutionCache
          .resolve(languageId, fileUri) {
            LspServerRegistry.resolve(languageId, userConfig).flatMap {
              case None =>
                logger.info(s"[LSP] No server available for ${languageId.id}").as(None)
              case Some(config) =>
                val filePath = uriToPath(fileUri)
                WorkspaceRootDetector.detect(filePath, languageId).map { rootOpt =>
                  val rootUri = rootOpt.map(_.toUri.toString).getOrElse(parentUri(fileUri))
                  Some(config -> rootUri)
                }
            }
          }
          .map {
            case None => None
            case Some((config, rootUri)) =>
              Some(ResolvedConnection(ConnectionIdentity(rootUri, config), LspConnection(config, rootUri, logger)))
          }

      override def evictResolution(languageId: LanguageId, fileUri: String): IO[Unit] =
        resolutionCache.evict(languageId, fileUri)

  private def ensureConnection(
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    languageId: LanguageId,
    fileUri: String,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Option[(ConnectionIdentity, LspConnection)]] =
    val onDiagnostics = (uri: String, diags: List[com.serenity.lsp.model.Diagnostic]) =>
      applyEvent(LspEvent.LspDiagnosticsReceived(uri, diags))
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
    onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit],
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

  private def uriToPath(uri: String): String =
    if uri.startsWith("file://") then java.net.URI.create(uri).getPath
    else uri

  private def parentUri(uri: String): String =
    val lastSlash = uri.lastIndexOf('/')
    if lastSlash > 0 then uri.substring(0, lastSlash) else uri

  private def releaseConnections(
    connectionsRef: Ref[IO, Map[ConnectionIdentity, ManagedConnection]],
    logger: Logger[IO]
  ): IO[Unit] =
    connectionsRef
      .modify(connections => (Map.empty, connections.values.toList))
      .flatMap(_.traverse_(managed => managed.release.handleErrorWith(ex => logger.error(ex)("[LSP] release failed"))))
