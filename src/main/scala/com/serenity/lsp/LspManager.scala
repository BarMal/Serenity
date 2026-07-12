package com.serenity.lsp

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{LspConnection, LspProtocol}
import com.serenity.lsp.config.*
import fs2.Stream
import org.typelevel.log4cats.Logger

object LspManager:

  private case class ManagedConnection(connection: LspConnection, release: IO[Unit])

  def run(
    effects: Stream[IO, LspEffect],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    userConfig: LspUserConfig = LspUserConfig.empty
  ): IO[Unit] =
    Ref.of[IO, Map[LanguageId, ManagedConnection]](Map.empty).flatMap { connectionsRef =>
      effects
        .evalMap {
          case LspEffect.FileOpened(uri, languageId, text) =>
            ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, userConfig)
              .flatMap {
                case Some(conn) =>
                  conn
                    .sendNotification("textDocument/didOpen", LspProtocol.didOpenParams(uri, languageId.id, 1, text))
                    .handleErrorWith(ex => logger.error(ex)(s"[LSP] didOpen failed: $uri"))
                case None =>
                  logger.debug(s"[LSP] No server for ${languageId.id}, skipping didOpen")
              }

          case LspEffect.FileChanged(uri, languageId, text, version) =>
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
            ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, userConfig).flatMap {
              case Some(conn) =>
                conn
                  .sendRequest("textDocument/hover", LspProtocol.textDocumentPositionParams(uri, line, character))
                  .flatMap(response =>
                    LspProtocol
                      .parseHoverText(response)
                      .fold(IO.unit)(text => applyEvent(LspEvent.LspHoverReceived(text, anchor)))
                  )
                  .handleErrorWith(ex => logger.error(ex)(s"[LSP] hover failed: $uri"))
              case None =>
                applyEvent(LspEvent.LspHoverReceived(s"No LSP server available for ${languageId.displayName}", anchor))
            }

          case LspEffect.CompletionRequested(uri, languageId, line, character, anchor) =>
            ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, userConfig).flatMap {
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
            ensureConnection(connectionsRef, languageId, uri, applyEvent, logger, userConfig).flatMap {
              case Some(conn) =>
                conn
                  .sendRequest("textDocument/definition", LspProtocol.textDocumentPositionParams(uri, line, character))
                  .flatMap(response =>
                    LspProtocol
                      .parseDefinitionLocation(response)
                      .fold(IO.unit)(location =>
                        applyEvent(
                          LspEvent.LspDefinitionReceived(symbol, location.uri, location.range.start, anchor)
                        )
                      )
                  )
                  .handleErrorWith(ex => logger.error(ex)(s"[LSP] definition failed: $uri"))
              case None =>
                applyEvent(LspEvent.LspHoverReceived(s"No LSP server available for ${languageId.displayName}", anchor))
            }
        }
        .compile
        .drain
        .guarantee(releaseConnections(connectionsRef, logger))
    }

  private def ensureConnection(
    connectionsRef: Ref[IO, Map[LanguageId, ManagedConnection]],
    languageId: LanguageId,
    fileUri: String,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    userConfig: LspUserConfig
  ): IO[Option[LspConnection]] =
    connectionsRef.get.flatMap { conns =>
      conns.get(languageId) match
        case Some(managed) => IO.pure(Some(managed.connection))
        case None          => spawnConnection(connectionsRef, languageId, fileUri, applyEvent, logger, userConfig)
    }

  private def spawnConnection(
    connectionsRef: Ref[IO, Map[LanguageId, ManagedConnection]],
    languageId: LanguageId,
    fileUri: String,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    userConfig: LspUserConfig
  ): IO[Option[LspConnection]] =
    LspServerRegistry.resolve(languageId, userConfig).flatMap {
      case None =>
        logger.info(s"[LSP] No server available for ${languageId.id}").as(None)
      case Some(config) =>
        val filePath = uriToPath(fileUri)
        WorkspaceRootDetector.detect(filePath, languageId).flatMap { rootOpt =>
          val rootUri = rootOpt.map(_.toUri.toString).getOrElse(parentUri(fileUri))
          LspConnection(config, rootUri, logger).allocated
            .flatMap {
              case (conn, release) =>
                val onDiagnostics = (uri: String, diags: List[com.serenity.lsp.model.Diagnostic]) =>
                  applyEvent(LspEvent.LspDiagnosticsReceived(uri, diags))
                conn.processIncoming(onDiagnostics).start.flatMap { diagnosticsFiber =>
                  val managed = ManagedConnection(
                    connection = conn,
                    release = release >> diagnosticsFiber.cancel
                  )
                  connectionsRef.update(_ + (languageId -> managed))
                } >>
                  IO.pure(Some(conn))
            }
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] Failed to connect to ${config.command}").as(None))
        }
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
