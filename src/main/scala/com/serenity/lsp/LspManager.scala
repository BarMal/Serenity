package com.serenity.lsp

import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{LspConnection, LspProtocol}
import com.serenity.lsp.config.*
import fs2.Stream
import org.typelevel.log4cats.Logger

object LspManager:

  def run(
    effects: Stream[IO, LspEffect],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO]
  ): IO[Unit] =
    Ref.of[IO, Map[LanguageId, LspConnection]](Map.empty).flatMap { connectionsRef =>
      effects
        .evalMap {
          case LspEffect.FileOpened(uri, languageId, text) =>
            ensureConnection(connectionsRef, languageId, uri, applyEvent, logger)
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
                case Some(conn) =>
                  conn
                    .sendNotification("textDocument/didChange", LspProtocol.didChangeParams(uri, version, text))
                    .handleErrorWith(ex => logger.error(ex)(s"[LSP] didChange failed: $uri"))
                case None =>
                  IO.unit
            }

          case LspEffect.FileClosed(uri, languageId) =>
            connectionsRef.get.flatMap { conns =>
              conns.get(languageId) match
                case Some(conn) =>
                  conn
                    .sendNotification("textDocument/didClose", LspProtocol.didCloseParams(uri))
                    .handleErrorWith(ex => logger.error(ex)(s"[LSP] didClose failed: $uri"))
                case None =>
                  IO.unit
            }
        }
        .compile
        .drain
    }

  private def ensureConnection(
    connectionsRef: Ref[IO, Map[LanguageId, LspConnection]],
    languageId: LanguageId,
    fileUri: String,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO]
  ): IO[Option[LspConnection]] =
    connectionsRef.get.flatMap { conns =>
      conns.get(languageId) match
        case Some(conn) => IO.pure(Some(conn))
        case None       => spawnConnection(connectionsRef, languageId, fileUri, applyEvent, logger)
    }

  private def spawnConnection(
    connectionsRef: Ref[IO, Map[LanguageId, LspConnection]],
    languageId: LanguageId,
    fileUri: String,
    applyEvent: Event => IO[Unit],
    logger: Logger[IO]
  ): IO[Option[LspConnection]] =
    LspServerRegistry.resolve(languageId, LspUserConfig.empty).flatMap {
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
                conn.processIncoming(onDiagnostics).start >>
                  connectionsRef.update(_ + (languageId -> conn)) >>
                  IO.pure(Some(conn))
            }
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] Failed to connect to ${config.binary.command}").as(None))
        }
    }

  private def uriToPath(uri: String): String =
    if uri.startsWith("file://") then java.net.URI.create(uri).getPath
    else uri

  private def parentUri(uri: String): String =
    val lastSlash = uri.lastIndexOf('/')
    if lastSlash > 0 then uri.substring(0, lastSlash) else uri
