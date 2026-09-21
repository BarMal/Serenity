package com.serenity.lsp

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref}
import com.serenity.diagnostics.Trace
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{DocumentUri, LspMethod, LspProtocol}
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.CursorPosition
import org.typelevel.log4cats.Logger

/** `textDocument/references` and `textDocument/rename` request handling, split out of [[LspManager]] to keep that file
  * under the architecture ratchet's file-length target. Both follow [[LspManager.startRequest]]'s shared request
  * lifecycle exactly like [[LspManager.requestSemanticTokens]] does, just kept out of `LspManager` itself.
  */
private[lsp] object LspManagerReferenceRenameSupport:

  def requestReferences(
    rawUri: String,
    languageId: LanguageId,
    line: Int,
    character: Int,
    anchor: CursorPosition,
    symbol: String,
    connectionsRef: Ref[IO, Map[LspManager.ConnectionIdentity, LspManager.ManagedConnection]],
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    requestContexts: Ref[IO, Map[LspManager.RequestKey, LspManager.RequestContext]],
    requestFibers: Ref[IO, Map[LspManager.RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: LspManager.ConnectionProvider
  ): IO[Unit] =
    given Logger[IO] = logger
    val uri          = DocumentUri(rawUri)
    LspManager.startRequest(
      LspManager.RequestKind.References,
      uri,
      languageId,
      LspManager.RequestAnchor.CursorAnchored(anchor),
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
        .timed(s"lsp.references.$rawUri")(
          conn.sendRequest(LspMethod("textDocument/references"), LspProtocol.referencesParams(uri, line, character))
        )
        .flatMap(response =>
          LspProtocol.parseReferencesLocations(response).fold(IO.unit) { locations =>
            LspManager
              .isCurrent(
                LspManager.RequestKey(uri, LspManager.RequestKind.References),
                context,
                documentVersions,
                requestContexts
              )
              .ifM(
                applyEvent(
                  LspEvent.LspReferencesReceived(
                    symbol,
                    locations.map(location => location.uri.value -> location.range.start),
                    anchor
                  )
                ),
                IO.unit
              )
          }
        )
        .handleErrorWith(ex => logger.error(ex)(s"[LSP] references failed: $rawUri"))
    }

  def requestRename(
    rawUri: String,
    languageId: LanguageId,
    line: Int,
    character: Int,
    anchor: CursorPosition,
    newName: String,
    connectionsRef: Ref[IO, Map[LspManager.ConnectionIdentity, LspManager.ManagedConnection]],
    documentVersions: Ref[IO, Map[DocumentUri, Int]],
    requestContexts: Ref[IO, Map[LspManager.RequestKey, LspManager.RequestContext]],
    requestFibers: Ref[IO, Map[LspManager.RequestKey, cats.effect.Fiber[IO, Throwable, Unit]]],
    supervisor: Supervisor[IO],
    applyEvent: Event => IO[Unit],
    logger: Logger[IO],
    connectionProvider: LspManager.ConnectionProvider
  ): IO[Unit] =
    given Logger[IO] = logger
    val uri          = DocumentUri(rawUri)
    LspManager.startRequest(
      LspManager.RequestKind.Rename,
      uri,
      languageId,
      LspManager.RequestAnchor.CursorAnchored(anchor),
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
        .timed(s"lsp.rename.$rawUri")(
          conn.sendRequest(LspMethod("textDocument/rename"), LspProtocol.renameParams(uri, line, character, newName))
        )
        .flatMap { response =>
          val edits = LspProtocol.parseWorkspaceEdit(response).map { case (docUri, e) => docUri.value -> e }
          LspManager
            .isCurrent(
              LspManager.RequestKey(uri, LspManager.RequestKind.Rename),
              context,
              documentVersions,
              requestContexts
            )
            .ifM(applyEvent(LspEvent.LspRenameReceived(edits, anchor)), IO.unit)
        }
        .handleErrorWith(ex => logger.error(ex)(s"[LSP] rename failed: $rawUri"))
    }
