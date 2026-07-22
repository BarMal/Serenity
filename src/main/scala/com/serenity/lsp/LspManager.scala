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

  private case class ManagedConnection(connection: LspConnection, release: IO[Unit])

  private enum RequestKind:
    case Hover, Definition

  private case class RequestKey(uri: String, kind: RequestKind)
  private case class RequestContext(version: Int, anchor: CursorPosition)

  private[lsp] trait ConnectionProvider:

    def connect(
      languageId: LanguageId,
      fileUri: String,
      onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
    ): IO[Option[Resource[IO, LspConnection]]]

  def run(
    effects: Stream[IO, LspEffect],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    userConfig: LspUserConfig = LspUserConfig.empty
  ): IO[Unit] =
    runWithProvider(effects, applyEvent, logger, connectionProvider(userConfig, logger))

  private[lsp] def runWithProvider(
    effects: Stream[IO, LspEffect],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Unit] =
    Supervisor[IO].allocated.flatMap {
      case (supervisor, releaseRequests) =>
        for
          connectionsRef   <- Ref.of[IO, Map[LanguageId, ManagedConnection]](Map.empty)
          documentVersions <- Ref.of[IO, Map[String, Int]](Map.empty)
          requestContexts  <- Ref.of[IO, Map[RequestKey, RequestContext]](Map.empty)
          requestFibers    <- Ref.of[IO, Map[RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]](Map.empty)
          runEffects = effects
            .evalMap(
              handleEffect(
                _,
                connectionsRef,
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
    connectionsRef: Ref[IO, Map[LanguageId, ManagedConnection]],
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
            case Some(conn) =>
              conn
                .sendNotification("textDocument/didOpen", LspProtocol.didOpenParams(uri, languageId.id, 1, text))
                .handleErrorWith(ex => logger.error(ex)(s"[LSP] didOpen failed: $uri"))
            case None =>
              logger.debug(s"[LSP] No server for ${languageId.id}, skipping didOpen")
          }

      case LspEffect.FileChanged(uri, languageId, text, version) =>
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ + (uri -> version)) >>
          connectionsRef.get.flatMap { conns =>
            conns.get(languageId) match
              case Some(managed) =>
                managed.connection
                  .sendNotification("textDocument/didChange", LspProtocol.didChangeParams(uri, version, text))
                  .handleErrorWith(ex => logger.error(ex)(s"[LSP] didChange failed: $uri"))
              case None =>
                IO.unit
          }

      case LspEffect.FileClosed(uri, languageId) =>
        invalidateDocument(uri, requestContexts, requestFibers) >>
          documentVersions.update(_ - uri) >>
          connectionsRef.get.flatMap { conns =>
            conns.get(languageId) match
              case Some(managed) =>
                managed.connection
                  .sendNotification("textDocument/didClose", LspProtocol.didCloseParams(uri))
                  .handleErrorWith(ex => logger.error(ex)(s"[LSP] didClose failed: $uri"))
              case None =>
                IO.unit
          }

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
          case Some(conn) =>
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
    connectionsRef: Ref[IO, Map[LanguageId, ManagedConnection]],
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
          case Some(conn) =>
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

  private def connectionProvider(userConfig: LspUserConfig, logger: Logger[IO]): ConnectionProvider =
    new ConnectionProvider:
      def connect(
        languageId: LanguageId,
        fileUri: String,
        onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
      ): IO[Option[Resource[IO, LspConnection]]] =
        LspServerRegistry.resolve(languageId, userConfig).flatMap {
          case None =>
            logger.info(s"[LSP] No server available for ${languageId.id}").as(None)
          case Some(config) =>
            val filePath = uriToPath(fileUri)
            WorkspaceRootDetector.detect(filePath, languageId).map { rootOpt =>
              val rootUri = rootOpt.map(_.toUri.toString).getOrElse(parentUri(fileUri))
              Some(LspConnection(config, rootUri, logger))
            }
        }

  private def ensureConnection(
    connectionsRef: Ref[IO, Map[LanguageId, ManagedConnection]],
    languageId: LanguageId,
    fileUri: String,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Option[LspConnection]] =
    connectionsRef.get.flatMap { conns =>
      conns.get(languageId) match
        case Some(managed) => IO.pure(Some(managed.connection))
        case None => spawnConnection(connectionsRef, languageId, fileUri, applyEvent, logger, connectionProvider)
    }

  private def spawnConnection(
    connectionsRef: Ref[IO, Map[LanguageId, ManagedConnection]],
    languageId: LanguageId,
    fileUri: String,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: ConnectionProvider
  ): IO[Option[LspConnection]] =
    val onDiagnostics = (uri: String, diags: List[com.serenity.lsp.model.Diagnostic]) =>
      applyEvent(LspEvent.LspDiagnosticsReceived(uri, diags))
    connectionProvider.connect(languageId, fileUri, onDiagnostics).flatMap {
      case None => IO.pure(None)
      case Some(resource) =>
        resource.allocated
          .flatMap {
            case (conn, release) =>
              conn.processIncoming(onDiagnostics).start.flatMap { diagnosticsFiber =>
                val managed = ManagedConnection(
                  connection = conn,
                  release = release >> diagnosticsFiber.cancel
                )
                connectionsRef.update(_ + (languageId -> managed)) >> IO.pure(Some(conn))
              }
          }
          .handleErrorWith(ex => logger.error(ex)(s"[LSP] Failed to connect for ${languageId.id}").as(None))
    }

  private def uriToPath(uri: String): String =
    if uri.startsWith("file://") then java.net.URI.create(uri).getPath
    else uri

  private def parentUri(uri: String): String =
    val lastSlash = uri.lastIndexOf('/')
    if lastSlash > 0 then uri.substring(0, lastSlash) else uri

  private def releaseConnections(
    connectionsRef: Ref[IO, Map[LanguageId, ManagedConnection]],
    logger: Logger[IO]
  ): IO[Unit] =
    connectionsRef
      .modify(connections => (Map.empty, connections.values.toList))
      .flatMap(_.traverse_(managed => managed.release.handleErrorWith(ex => logger.error(ex)("[LSP] release failed"))))
