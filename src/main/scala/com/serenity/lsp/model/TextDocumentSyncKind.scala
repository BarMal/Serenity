package com.serenity.lsp.model

import io.circe.Json

/** The server-negotiated `textDocumentSync` capability from an LSP `initialize` response (the spec's
  * `TextDocumentSyncKind`: 0 = None, 1 = Full, 2 = Incremental).
  */
enum TextDocumentSyncKind:
  case None, Full, Incremental

object TextDocumentSyncKind:

  /** `capabilities.textDocumentSync` is either the kind code directly, or an object whose `change` field carries it
    * (the richer `TextDocumentSyncOptions` shape). Falls back to `Full` when the field is absent or neither shape
    * parses -- this client always sent full-document `didChange` notifications before this capability was read at
    * all, so an unset or unrecognized capability must keep doing that rather than silently going quiet.
    */
  def fromInitializeResult(result: Json): TextDocumentSyncKind =
    val field = result.hcursor.downField("capabilities").downField("textDocumentSync")
    val code  = field.as[Int].toOption.orElse(field.downField("change").as[Int].toOption)
    code match
      case scala.Some(0) => TextDocumentSyncKind.None
      case scala.Some(2) => TextDocumentSyncKind.Incremental
      case _             => TextDocumentSyncKind.Full
