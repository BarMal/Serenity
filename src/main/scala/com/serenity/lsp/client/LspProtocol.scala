package com.serenity.lsp.client

import cats.syntax.traverse.*
import com.serenity.lsp.model.*
import com.serenity.rope.{Balance, ChangeSet, Rope}
import io.circe.syntax.*
import io.circe.{HCursor, Json}

/** Every shape an incoming JSON-RPC message can actually take (see
  * https://www.jsonrpc.org/specification#response_object): a successful response, an error response, a notification, or
  * a request the server sends the client. `Malformed` is a catch-all for anything else so it can be logged instead of
  * silently dropped.
  */
enum JsonRpcMessage:
  case Response(id: RequestId, result: Json)
  case ResponseError(id: RequestId, code: Int, message: String)
  case Notification(method: LspMethod, params: Json)
  case ServerRequest(id: ServerRequestId, method: LspMethod, params: Json)
  case Malformed(raw: Json)

object LspProtocol:

  final case class JsonRpcRequest(id: RequestId, method: LspMethod, params: Json)
  final case class JsonRpcNotification(method: LspMethod, params: Json)
  final case class LspLocation(uri: DocumentUri, range: LspRange)

  /** `Json.Null` params are left out rather than sent as `"params": null`: JSON-RPC only allows a structured value
    * there, and parameterless methods such as `shutdown` and `exit` take none.
    */
  def request(id: RequestId, method: LspMethod, params: Json): Json =
    Json.fromFields(
      List("jsonrpc" -> "2.0".asJson, "id" -> id.value.asJson, "method" -> method.value.asJson) ++ paramsField(params)
    )

  def notification(method: LspMethod, params: Json): Json =
    Json.fromFields(List("jsonrpc" -> "2.0".asJson, "method" -> method.value.asJson) ++ paramsField(params))

  private def paramsField(params: Json): List[(String, Json)] =
    if params.isNull then Nil else List("params" -> params)

  def response(id: ServerRequestId, result: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> serverRequestIdJson(id), "result" -> result)

  def errorResponse(id: ServerRequestId, code: Int, message: String): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> serverRequestIdJson(id),
      "error"   -> Json.obj("code" -> code.asJson, "message" -> message.asJson)
    )

  private def serverRequestIdJson(id: ServerRequestId): Json =
    id match
      case ServerRequestId.Numeric(value) => value.asJson
      case ServerRequestId.Text(value)    => value.asJson

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
    val id     = c.downField("id").focus.filterNot(_.isNull)
    val method = c.downField("method").as[String].toOption
    (id, method) match
      case (Some(rawId), Some(m)) =>
        serverRequestId(rawId).fold(JsonRpcMessage.Malformed(json)) { requestId =>
          JsonRpcMessage.ServerRequest(requestId, LspMethod(m), c.downField("params").focus.getOrElse(Json.Null))
        }
      case (Some(rawId), None) =>
        rawId
          .as[Long]
          .toOption
          .fold(JsonRpcMessage.Malformed(json))(responseId => classifyResponse(c, RequestId(responseId)))
      case (None, Some(m)) =>
        val params = c.downField("params").focus.getOrElse(Json.obj())
        JsonRpcMessage.Notification(LspMethod(m), params)
      case (None, None) => JsonRpcMessage.Malformed(json)

  private def classifyResponse(c: HCursor, id: RequestId): JsonRpcMessage =
    c.downField("error").focus match
      case Some(error) =>
        val code    = error.hcursor.downField("code").as[Int].getOrElse(0)
        val message = error.hcursor.downField("message").as[String].getOrElse("Unknown LSP error")
        JsonRpcMessage.ResponseError(id, code, message)
      case None =>
        val result = c.downField("result").focus.getOrElse(Json.Null)
        JsonRpcMessage.Response(id, result)

  private def serverRequestId(raw: Json): Option[ServerRequestId] =
    raw.as[Long].toOption.map(ServerRequestId.Numeric(_)).orElse(raw.asString.map(ServerRequestId.Text(_)))

  def notificationMethod(json: Json): Option[LspMethod] =
    json.hcursor.downField("method").as[String].toOption.map(LspMethod(_))

  // ── Initialize ──────────────────────────────────────────────────────────────

  def initializeParams(pid: Int, rootUri: WorkspaceRootUri): Json =
    Json.obj(
      "processId"  -> pid.asJson,
      "clientInfo" -> Json.obj("name" -> "Serenity".asJson, "version" -> "0.1.0".asJson),
      "rootUri"    -> rootUri.value.asJson,
      "capabilities" -> Json.obj(
        "workspace" -> Json.obj(
          "applyEdit"     -> true.asJson,
          "configuration" -> true.asJson
        ),
        "window" -> Json.obj(
          "workDoneProgress" -> true.asJson,
          "showMessage"      -> Json.obj("messageActionItem" -> Json.obj("additionalPropertiesSupport" -> false.asJson))
        ),
        "textDocument" -> Json.obj(
          "synchronization"    -> Json.obj("dynamicRegistration" -> false.asJson),
          "publishDiagnostics" -> Json.obj("relatedInformation" -> true.asJson),
          "hover"              -> Json.obj("contentFormat" -> Json.arr("markdown".asJson, "plaintext".asJson)),
          "definition"         -> Json.obj("linkSupport" -> false.asJson),
          "references"         -> Json.obj("dynamicRegistration" -> false.asJson),
          "rename"             -> Json.obj("prepareSupport" -> false.asJson),
          "completion" -> Json.obj(
            "completionItem" -> Json.obj(
              "snippetSupport"      -> false.asJson,
              "documentationFormat" -> Json.arr("markdown".asJson, "plaintext".asJson)
            )
          ),
          "semanticTokens" -> Json.obj(
            "requests"       -> Json.obj("full" -> Json.obj("delta" -> true.asJson), "range" -> true.asJson),
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

  /** The notification for an edit between two ropes. Incremental sync reads only what changed, and no sync kind builds
    * the old document's text; `Full` collects the new text once, here, because this is where it is sent.
    */
  def didChangeParams(uri: DocumentUri, version: Int, previous: Rope, next: Rope, syncKind: TextDocumentSyncKind)(using
    Balance
  ): Json =
    syncKind match
      case TextDocumentSyncKind.Incremental =>
        incrementalDidChangeParams(uri, version, TextChangeDiff.diff(previous, next))
      case _ => didChangeParams(uri, version, next.collect())

  /** [[didChangeParams]] for an edit whose change is known, which `Incremental` sync sends as it is: one content change
    * per part, the last first, so no two texts are compared. Any other kind sends the new text.
    */
  def didChangeParams(
    uri: DocumentUri,
    version: Int,
    previous: Rope,
    next: Rope,
    change: ChangeSet,
    syncKind: TextDocumentSyncKind
  ): Json =
    syncKind match
      case TextDocumentSyncKind.Incremental =>
        Json.obj(
          "textDocument"   -> Json.obj("uri" -> uri.value.asJson, "version" -> version.asJson),
          "contentChanges" -> Json.arr(TextChangeDiff.changes(previous, change).map(contentChangeJson)*)
        )
      case _ => didChangeParams(uri, version, next.collect())

  private def incrementalDidChangeParams(uri: DocumentUri, version: Int, previousText: String, newText: String): Json =
    incrementalDidChangeParams(uri, version, TextChangeDiff.diff(previousText, newText))

  private def incrementalDidChangeParams(uri: DocumentUri, version: Int, change: TextChangeDiff.Change): Json =
    Json.obj(
      "textDocument"   -> Json.obj("uri" -> uri.value.asJson, "version" -> version.asJson),
      "contentChanges" -> Json.arr(contentChangeJson(change))
    )

  private def contentChangeJson(change: TextChangeDiff.Change): Json =
    Json.obj(
      "range"       -> rangeJson(change.range),
      "rangeLength" -> change.rangeLength.asJson,
      "text"        -> change.text.asJson
    )

  private def rangeJson(range: LspRange): Json =
    Json.obj("start" -> positionJson(range.start), "end" -> positionJson(range.end))

  private def positionJson(position: LspPosition): Json =
    Json.obj("line" -> position.line.asJson, "character" -> position.character.asJson)

  def hoverParams(uri: DocumentUri, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  def definitionParams(uri: DocumentUri, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  /** `includeDeclaration` is always `true`: this client has no UI distinction for "references excluding the declaration
    * itself" today, and including it matches what most editors show by default.
    */
  def referencesParams(uri: DocumentUri, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character).deepMerge(
      Json.obj("context" -> Json.obj("includeDeclaration" -> true.asJson))
    )

  def renameParams(uri: DocumentUri, line: Int, character: Int, newName: String): Json =
    textDocumentPositionParams(uri, line, character).deepMerge(Json.obj("newName" -> newName.asJson))

  def completionParams(uri: DocumentUri, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  def semanticTokensParams(uri: DocumentUri): Json =
    Json.obj("textDocument" -> Json.obj("uri" -> uri.value.asJson))

  def semanticTokensDeltaParams(uri: DocumentUri, previousResultId: String): Json =
    semanticTokensParams(uri).deepMerge(Json.obj("previousResultId" -> previousResultId.asJson))

  /** The range covers whole lines: from the start of `firstLine` to the start of the line after `lastLine`. */
  def semanticTokensRangeParams(uri: DocumentUri, firstLine: Int, lastLine: Int): Json =
    semanticTokensParams(uri).deepMerge(
      Json.obj(
        "range" -> rangeJson(LspRange(LspPosition(firstLine, 0), LspPosition(lastLine + 1, 0)))
      )
    )

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

  /** `None` only when `result` isn't an array at all (e.g. `null` for "no references") -- an empty array is a real,
    * confirmed "found zero references" answer and parses to `Some(Nil)`, the same distinction [[parseCompletionItems]]
    * already draws between "no result" and "result is an empty list".
    */
  def parseReferencesLocations(result: Json): Option[List[LspLocation]] =
    result.asArray.map(_.toList.flatMap(parseLocation))

  /** Parses a `textDocument/rename` response's `WorkspaceEdit` (LSP 3.17 §3.17.9) into edits keyed by the document uri
    * they apply to. A server may use either the `changes` form (a plain uri -> `TextEdit[]` map) or the richer
    * `documentChanges` form (an array of `TextDocumentEdit`, needed when an edit also creates/renames/deletes a file --
    * this client has no use for that extra information, only the edits themselves); `changes` is preferred when both
    * are present, per the spec. Neither field present (or a `null` result) parses to an empty map, not `None` -- unlike
    * [[parseReferencesLocations]], a rename that touches nothing is not distinguishable from, nor treated differently
    * than, one whose result shape this client doesn't recognise.
    */
  def parseWorkspaceEdit(result: Json): Map[DocumentUri, List[LspTextEdit]] =
    val c = result.hcursor
    c.downField("changes").as[Map[String, List[Json]]].toOption match
      case Some(changes) =>
        changes.map { case (uri, edits) => DocumentUri(uri) -> edits.flatMap(parseTextEdit) }
      case None =>
        c.downField("documentChanges")
          .as[List[Json]]
          .getOrElse(Nil)
          .flatMap { documentChange =>
            val dc = documentChange.hcursor
            for
              uri   <- dc.downField("textDocument").downField("uri").as[String].toOption
              edits <- dc.downField("edits").as[List[Json]].toOption
            yield DocumentUri(uri) -> edits.flatMap(parseTextEdit)
          }
          .toMap

  private def parseTextEdit(json: Json): Option[LspTextEdit] =
    for
      range   <- parseRange(json.hcursor)
      newText <- json.hcursor.downField("newText").as[String].toOption
    yield LspTextEdit(range, newText)

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

  /** Which requests the server's `semanticTokensProvider` offers: `full` and `range` are each a boolean or an options
    * object, and `full.delta` says whether `full/delta` is served. A provider naming neither is read as offering
    * `full`, the one request this client sent before it knew the others.
    */
  def parseSemanticTokensFeatures(initializeResult: Json): SemanticTokensFeatures =
    val provider = initializeResult.hcursor.downField("capabilities").downField("semanticTokensProvider")
    val offered = (name: String) =>
      provider.downField(name).focus.exists(json => json.asBoolean.getOrElse(json.isObject))
    val full  = offered("full")
    val range = offered("range")
    val delta = full && provider.downField("full").downField("delta").as[Boolean].getOrElse(false)
    if full || range then SemanticTokensFeatures(full, delta, range) else SemanticTokensFeatures.FullOnly

  /** Decodes a semantic tokens response: `data` of a full or range result, or the `edits` of a delta result. Integers
    * are read straight into a primitive array; a value that is not a non-negative integer rejects the whole response,
    * since every later token would be mis-placed.
    */
  def parseSemanticTokensResult(result: Json): Option[SemanticTokensResult] =
    val cursor   = result.hcursor
    val resultId = cursor.downField("resultId").as[String].toOption
    cursor
      .downField("data")
      .focus
      .flatMap(parseIntegers)
      .map(SemanticTokensResult.Full(resultId, _))
      .orElse(
        cursor
          .downField("edits")
          .focus
          .flatMap(_.asArray)
          .flatMap(_.toList.traverse(parseSemanticTokensEdit))
          .map(SemanticTokensResult.Delta(resultId, _))
      )

  private def parseSemanticTokensEdit(json: Json): Option[SemanticTokensEdit] =
    val cursor = json.hcursor
    for
      start       <- cursor.downField("start").as[Int].toOption
      deleteCount <- cursor.downField("deleteCount").as[Int].toOption
      data        <- cursor.downField("data").focus.fold(Some(IArray.empty[Int]))(parseIntegers)
    yield SemanticTokensEdit(start, deleteCount, data)

  private def parseIntegers(json: Json): Option[IArray[Int]] =
    json.asArray.flatMap { values =>
      val integers = IArray.tabulate(values.length)(index => values(index).asNumber.flatMap(_.toInt).getOrElse(-1))
      Option.when(integers.forall(_ >= 0))(integers)
    }

  /** Decodes a full or range result's `data` against the server's `legend` (LSP 3.17 §3.17.7.4). */
  def parseSemanticTokens(result: Json, legend: SemanticTokensLegend): Option[SemanticTokenData] =
    parseSemanticTokensResult(result).collect {
      case SemanticTokensResult.Full(_, data) =>
        SemanticTokenData.decode(data, legend)
    }
