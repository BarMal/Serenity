package com.serenity.lsp.client

import com.serenity.lsp.model.*
import io.circe.syntax.*
import io.circe.{HCursor, Json}

/** Every shape an incoming JSON-RPC message can actually take (see
  * https://www.jsonrpc.org/specification#response_object): a successful response, an error response, or a notification.
  * `Malformed` is a catch-all for anything else (e.g. a server-to-client request, which this client does not serve) so
  * it can be logged instead of silently dropped.
  */
enum JsonRpcMessage:
  case Response(id: RequestId, result: Json)
  case ResponseError(id: RequestId, code: Int, message: String)
  case Notification(method: LspMethod, params: Json)
  case Malformed(raw: Json)

object LspProtocol:

  final case class JsonRpcRequest(id: RequestId, method: LspMethod, params: Json)
  final case class JsonRpcNotification(method: LspMethod, params: Json)
  final case class LspLocation(uri: DocumentUri, range: LspRange)

  def request(id: RequestId, method: LspMethod, params: Json): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> id.value.asJson,
      "method"  -> method.value.asJson,
      "params"  -> params
    )

  def notification(method: LspMethod, params: Json): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "method"  -> method.value.asJson,
      "params"  -> params
    )

  /** `$/cancelRequest`: tells the server to stop working on a request whose answer nobody will read.
    *
    * A notification, not a request -- the server is not expected to reply to it. It may still answer the original
    * request normally if it had already finished, or with `RequestCancelled` (-32800); either way the id is no longer
    * tracked by then, so [[classify]]'s caller drops it.
    */
  def cancelRequest(id: RequestId): Json =
    notification(LspMethod("$/cancelRequest"), Json.obj("id" -> id.value.asJson))

  /** The single exhaustive parse that replaces the old `isResponse`/`isNotification` boolean-predicate pair: every
    * incoming message is classified once, here, instead of leaving each caller to re-derive "what is this" from the
    * presence or absence of `id`/`method`/`error` fields.
    */
  def classify(json: Json): JsonRpcMessage =
    val c      = json.hcursor
    val idOpt  = c.downField("id").as[Long].toOption
    val method = c.downField("method").as[String].toOption
    (idOpt, method) match
      case (Some(id), None) =>
        c.downField("error").focus match
          case Some(error) =>
            val code    = error.hcursor.downField("code").as[Int].getOrElse(0)
            val message = error.hcursor.downField("message").as[String].getOrElse("Unknown LSP error")
            JsonRpcMessage.ResponseError(RequestId(id), code, message)
          case None =>
            val result = c.downField("result").focus.getOrElse(Json.Null)
            JsonRpcMessage.Response(RequestId(id), result)
      case (None, Some(m)) =>
        val params = c.downField("params").focus.getOrElse(Json.obj())
        JsonRpcMessage.Notification(LspMethod(m), params)
      case _ => JsonRpcMessage.Malformed(json)

  def notificationMethod(json: Json): Option[LspMethod] =
    json.hcursor.downField("method").as[String].toOption.map(LspMethod(_))

  // ── Initialize ──────────────────────────────────────────────────────────────

  def initializeParams(pid: Int, rootUri: WorkspaceRootUri): Json =
    Json.obj(
      "processId"  -> pid.asJson,
      "clientInfo" -> Json.obj("name" -> "Serenity".asJson, "version" -> "0.1.0".asJson),
      "rootUri"    -> rootUri.value.asJson,
      "capabilities" -> Json.obj(
        "textDocument" -> Json.obj(
          "synchronization"    -> Json.obj("dynamicRegistration" -> false.asJson),
          "publishDiagnostics" -> Json.obj("relatedInformation" -> true.asJson),
          "hover"              -> Json.obj("contentFormat" -> Json.arr("markdown".asJson, "plaintext".asJson)),
          "definition"         -> Json.obj("linkSupport" -> false.asJson),
          "completion" -> Json.obj(
            "completionItem" -> Json.obj(
              "snippetSupport"      -> false.asJson,
              "documentationFormat" -> Json.arr("markdown".asJson, "plaintext".asJson)
            )
          ),
          "semanticTokens" -> Json.obj(
            "requests"       -> Json.obj("full" -> true.asJson),
            "tokenTypes"     -> ClientSemanticTokenTypes.asJson,
            "tokenModifiers" -> ClientSemanticTokenModifiers.asJson,
            "formats"        -> Json.arr("relative".asJson)
          )
        )
      )
    )

  def initializedParams: Json = Json.obj()

  /** The client's own requested legend (LSP 3.17 §3.17.7.4) -- the *server's* legend, returned in its `initialize`
    * result, is what [[parseSemanticTokens]] must decode against, since a server is free to use its own ordering rather
    * than echo this one back.
    */
  val ClientSemanticTokenTypes: List[String] = List(
    "namespace",
    "type",
    "class",
    "enum",
    "interface",
    "struct",
    "typeParameter",
    "parameter",
    "variable",
    "property",
    "enumMember",
    "event",
    "function",
    "method",
    "macro",
    "keyword",
    "modifier",
    "comment",
    "string",
    "number",
    "regexp",
    "operator",
    "decorator"
  )

  val ClientSemanticTokenModifiers: List[String] = List(
    "declaration",
    "definition",
    "readonly",
    "static",
    "deprecated",
    "abstract",
    "async",
    "modification",
    "documentation",
    "defaultLibrary"
  )

  // ── TextDocument ────────────────────────────────────────────────────────────

  def didOpenParams(uri: DocumentUri, languageId: String, version: Int, text: String): Json =
    Json.obj(
      "textDocument" -> Json.obj(
        "uri"        -> uri.value.asJson,
        "languageId" -> languageId.asJson,
        "version"    -> version.asJson,
        "text"       -> text.asJson
      )
    )

  def didCloseParams(uri: DocumentUri): Json =
    Json.obj("textDocument" -> Json.obj("uri" -> uri.value.asJson))

  def didChangeParams(uri: DocumentUri, version: Int, text: String): Json =
    Json.obj(
      "textDocument"   -> Json.obj("uri" -> uri.value.asJson, "version" -> version.asJson),
      "contentChanges" -> Json.arr(Json.obj("text" -> text.asJson))
    )

  /** Sends a single range-based `TextDocumentContentChangeEvent` (see [[TextChangeDiff]]) when the server negotiated
    * `Incremental` sync, falling back to the existing full-text form (`text` with no `range`) for every other kind --
    * `Full` because that is what the server asked for, and `None` defensively, since callers are expected not to send
    * `didChange` at all in that case (see `LspManager`).
    */
  def didChangeParams(
    uri: DocumentUri,
    version: Int,
    previousText: String,
    newText: String,
    syncKind: TextDocumentSyncKind
  ): Json =
    syncKind match
      case TextDocumentSyncKind.Incremental => incrementalDidChangeParams(uri, version, previousText, newText)
      case _                                => didChangeParams(uri, version, newText)

  private def incrementalDidChangeParams(uri: DocumentUri, version: Int, previousText: String, newText: String): Json =
    val change = TextChangeDiff.diff(previousText, newText)
    Json.obj(
      "textDocument" -> Json.obj("uri" -> uri.value.asJson, "version" -> version.asJson),
      "contentChanges" -> Json.arr(
        Json.obj(
          "range"       -> rangeJson(change.range),
          "rangeLength" -> change.rangeLength.asJson,
          "text"        -> change.text.asJson
        )
      )
    )

  private def rangeJson(range: LspRange): Json =
    Json.obj("start" -> positionJson(range.start), "end" -> positionJson(range.end))

  private def positionJson(position: LspPosition): Json =
    Json.obj("line" -> position.line.asJson, "character" -> position.character.asJson)

  def hoverParams(uri: DocumentUri, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  def definitionParams(uri: DocumentUri, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  def completionParams(uri: DocumentUri, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  def semanticTokensParams(uri: DocumentUri): Json =
    Json.obj("textDocument" -> Json.obj("uri" -> uri.value.asJson))

  def textDocumentPositionParams(uri: DocumentUri, line: Int, character: Int): Json =
    Json.obj(
      "textDocument" -> Json.obj("uri" -> uri.value.asJson),
      "position" -> Json.obj(
        "line"      -> line.asJson,
        "character" -> character.asJson
      )
    )

  /** `result` here is already the unwrapped `result` field of a classified [[JsonRpcMessage.Response]] -- callers no
    * longer hand this the raw top-level response envelope.
    */
  def parseHoverText(result: Json): Option[String] =
    result.hcursor
      .downField("contents")
      .focus
      .flatMap(parseHoverContents)
      .map(_.trim)
      .filter(_.nonEmpty)

  private def parseHoverContents(json: Json): Option[String] =
    json.asString
      .orElse(markupContentValue(json))
      .orElse(markedStringValue(json))
      .orElse(
        json.asArray
          .map(_.toList.flatMap(parseHoverContents))
          .map(_.mkString("\n\n"))
      )

  private def markupContentValue(json: Json): Option[String] =
    json.hcursor.downField("kind").as[String].toOption.flatMap(_ => json.hcursor.downField("value").as[String].toOption)

  private def markedStringValue(json: Json): Option[String] =
    json.hcursor.downField("language").as[String].toOption.flatMap { _ =>
      json.hcursor.downField("value").as[String].toOption
    }

  def parseCompletionItems(result: Json): Option[List[String]] =
    result.asArray
      .orElse(result.hcursor.downField("items").focus.flatMap(_.asArray))
      .map(_.toList.flatMap(completionLabel))

  private def completionLabel(json: Json): Option[String] =
    json.asString
      .orElse(json.hcursor.downField("label").as[String].toOption)
      .map(_.trim)
      .filter(_.nonEmpty)

  def parseDefinitionLocation(result: Json): Option[LspLocation] =
    parseLocation(result).orElse(result.asArray.flatMap(_.toList.view.flatMap(parseLocation).headOption))

  /** Parses the `range.start`/`range.end` line/character pair off `c`, the cursor for the object that holds the `range`
    * field. LSP coordinates are 0-based.
    */
  def parseRange(c: HCursor): Option[LspRange] =
    val range = c.downField("range")
    for
      startLine <- range.downField("start").downField("line").as[Int].toOption
      startChar <- range.downField("start").downField("character").as[Int].toOption
      endLine   <- range.downField("end").downField("line").as[Int].toOption
      endChar   <- range.downField("end").downField("character").as[Int].toOption
    yield LspRange(LspPosition(startLine, startChar), LspPosition(endLine, endChar))

  private def parseLocation(json: Json): Option[LspLocation] =
    val c = json.hcursor
    for
      uri   <- c.downField("uri").as[String].toOption
      range <- parseRange(c)
    yield LspLocation(DocumentUri(uri), range)

  // ── PublishDiagnostics ──────────────────────────────────────────────────────

  def parseDiagnostics(json: Json): Option[(DocumentUri, List[Diagnostic])] =
    val c = json.hcursor.downField("params")
    for
      uri <- c.downField("uri").as[String].toOption
      diags = c
        .downField("diagnostics")
        .as[List[Json]]
        .getOrElse(Nil)
        .flatMap(parseDiagnostic)
    yield (DocumentUri(uri), diags)

  private def parseDiagnostic(json: Json): Option[Diagnostic] =
    val c = json.hcursor
    for
      range   <- parseRange(c)
      message <- c.downField("message").as[String].toOption
    yield
      val severity = c.downField("severity").as[Int].toOption.flatMap(DiagnosticSeverity.fromCode)
      val source   = c.downField("source").as[String].toOption
      val code     = c.downField("code").as[String].orElse(c.downField("code").as[Int].map(_.toString)).toOption
      Diagnostic(
        range = range,
        severity = severity,
        message = message,
        source = source,
        code = code
      )

  // ── SemanticTokens ────────────────────────────────────────────────────────────

  /** `initializeResult` is the unwrapped `result` of the `initialize` response. A server that supports semantic tokens
    * returns `capabilities.semanticTokensProvider` as either an object (with at least a `legend`) or `true` -- only the
    * object form carries the legend this client needs to decode `data`, so a bare `true` (permitted by the LSP spec but
    * not observed from any real server) is treated as "declared, but no legend to decode against."
    */
  def parseSemanticTokensLegend(initializeResult: Json): Option[SemanticTokensLegend] =
    val legend = initializeResult.hcursor
      .downField("capabilities")
      .downField("semanticTokensProvider")
      .downField("legend")
    for
      tokenTypes     <- legend.downField("tokenTypes").as[List[String]].toOption
      tokenModifiers <- legend.downField("tokenModifiers").as[List[String]].toOption
    yield SemanticTokensLegend(tokenTypes, tokenModifiers)

  def supportsSemanticTokens(initializeResult: Json): Boolean =
    initializeResult.hcursor
      .downField("capabilities")
      .downField("semanticTokensProvider")
      .focus
      .exists(!_.isNull)

  /** Decodes a `textDocument/semanticTokens/full` result's `data` (LSP 3.17 §3.17.7.4): a flat `uint32` array, five
    * integers per token -- `deltaLine`, `deltaStartChar`, `length`, `tokenType`, `tokenModifiers` -- each token's
    * position given relative to the previous token's, not absolute. `deltaLine == 0` means "same line as the previous
    * token," so `deltaStartChar` is then relative to that token's start column; a nonzero `deltaLine` starts a new
    * line, so `deltaStartChar` is that line's absolute column.
    *
    * Deltas are threaded through every token in the array, including one dropped below for an out-of-range `tokenType`
    * index -- a server's later deltas are still relative to that dropped token's *position*, not to whatever the last
    * *kept* token was, so threading must not skip it.
    */
  def parseSemanticTokens(result: Json, legend: SemanticTokensLegend): Option[List[SemanticToken]] =
    result.hcursor.downField("data").as[List[Int]].toOption.map { data =>
      case class Acc(line: Int, startCharacter: Int, tokens: List[SemanticToken])

      data
        .grouped(5)
        .foldLeft(Acc(0, 0, Nil)) {
          case (acc, deltaLine :: deltaStartChar :: length :: tokenTypeIndex :: modifiersBitset :: Nil) =>
            val line           = if deltaLine == 0 then acc.line else acc.line + deltaLine
            val startCharacter = if deltaLine == 0 then acc.startCharacter + deltaStartChar else deltaStartChar
            val nextTokens = legend.tokenTypes.lift(tokenTypeIndex) match
              case None => acc.tokens
              case Some(tokenType) =>
                val modifiers = legend.tokenModifiers.zipWithIndex.collect {
                  case (modifier, index) if (modifiersBitset & (1 << index)) != 0 => modifier
                }.toSet
                SemanticToken(line, startCharacter, length, tokenType, modifiers) :: acc.tokens
            Acc(line, startCharacter, nextTokens)
          case (acc, _) => acc
        }
        .tokens
        .reverse
    }
