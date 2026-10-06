package com.serenity.lsp

import cats.effect.{IO, Ref}
import com.serenity.lsp.OpenDocument.textOf
import com.serenity.lsp.client.{DocumentUri, LspConnection, LspMethod, LspProtocol}
import com.serenity.lsp.model.TextDocumentSyncKind
import com.serenity.rope.{Balance, Rope}
import org.typelevel.log4cats.Logger

private[lsp] object LspDidChange:

  private given Balance = Balance.default

  /** Sends `didChange` using whatever sync kind the connection negotiated during `initialize` (#1468): a range-based
    * diff against the document's previous text for `Incremental`, the existing full-text notification for `Full`, and
    * nothing at all for `None` -- a server that opted out of document sync should not be sent notifications for it
    * regardless of how expensive skipping them is.
    *
    * The text is collected here, once, and only for `Full`: the lane hands over ropes, and an incremental change is
    * found by walking what the edit left shared between the two.
    *
    * Returns whether the caller's mirror of the document may now advance to `text`. Under `Incremental` sync that
    * mirror is the base the next diff is computed against, so if the notification failed to send, the server never saw
    * this version -- advancing the mirror anyway would compute the next diff against text the server doesn't have,
    * permanently desyncing client and server state.
    */
  def send(
    connection: LspConnection,
    uri: DocumentUri,
    version: Int,
    text: Rope,
    openDocuments: Ref[IO, OpenDocument.Registry],
    logger: Logger[IO]
  ): IO[Boolean] =
    connection.syncKind.flatMap {
      case TextDocumentSyncKind.None => IO.pure(true)
      case syncKind =>
        openDocuments.get.map(_.textOf(uri).getOrElse(text)).flatMap { previous =>
          IO.delay(LspProtocol.didChangeParams(uri, version, previous, text, syncKind))
            .flatMap(connection.sendNotification(LspMethod("textDocument/didChange"), _))
            .as(true)
            .handleErrorWith(ex => logger.error(ex)(s"[LSP] didChange failed: ${uri.value}").as(false))
        }
    }
