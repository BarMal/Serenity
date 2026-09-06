package com.serenity.lsp.client

import com.serenity.lsp.model.*
import io.circe.syntax.*
import io.circe.{HCursor, Json}

object LspProtocol:

  final case class JsonRpcRequest(id: Long, method: String, params: Json)
  final case class JsonRpcNotification(method: String, params: Json)
  final case class LspLocation(uri: String, range: LspRange)

  def request(id: Long, method: String, params: Json): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> id.asJson,
      "method"  -> method.asJson,
      "params"  -> params
    )

  def notification(method: String, params: Json): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "method"  -> method.asJson,
      "params"  -> params
    )

  /** `$/cancelRequest`: tells the server to stop working on a request whose answer nobody will read.
    *
    * A notification, not a request -- the server is not expected to reply to it. It may still answer the original
    * request normally if it had already finished, or with `RequestCancelled` (-32800); either way the id is no longer
    * tracked by then, so [[isResponse]]'s caller drops it.
    */
  def cancelRequest(id: Long): Json =
    notification("$/cancelRequest", Json.obj("id" -> id.asJson))

  def isResponse(json: Json): Boolean =
    json.hcursor.downField("id").succeeded && json.hcursor.downField("method").failed
  def isNotification(json: Json): Boolean =
    json.hcursor.downField("method").succeeded && json.hcursor.downField("id").failed

  def responseId(json: Json): Option[Long]           = json.hcursor.downField("id").as[Long].toOption
  def notificationMethod(json: Json): Option[String] = json.hcursor.downField("method").as[String].toOption

  // ── Initialize ──────────────────────────────────────────────────────────────

  def initializeParams(pid: Int, rootUri: String): Json =
    Json.obj(
      "processId"  -> pid.asJson,
      "clientInfo" -> Json.obj("name" -> "Serenity".asJson, "version" -> "0.1.0".asJson),
      "rootUri"    -> rootUri.asJson,
      "capabilities" -> Json.obj(
        "textDocument" -> Json.obj(
          "publishDiagnostics" -> Json.obj("relatedInformation" -> true.asJson),
          "hover"              -> Json.obj("contentFormat" -> Json.arr("markdown".asJson, "plaintext".asJson)),
          "definition"         -> Json.obj("linkSupport" -> false.asJson),
          "completion" -> Json.obj(
            "completionItem" -> Json.obj(
              "snippetSupport"      -> false.asJson,
              "documentationFormat" -> Json.arr("markdown".asJson, "plaintext".asJson)
            )
          )
        )
      )
    )

  def initializedParams: Json = Json.obj()

  // ── TextDocument ────────────────────────────────────────────────────────────

  def didOpenParams(uri: String, languageId: String, version: Int, text: String): Json =
    Json.obj(
      "textDocument" -> Json.obj(
        "uri"        -> uri.asJson,
        "languageId" -> languageId.asJson,
        "version"    -> version.asJson,
        "text"       -> text.asJson
      )
    )

  def didCloseParams(uri: String): Json =
    Json.obj("textDocument" -> Json.obj("uri" -> uri.asJson))

  def didChangeParams(uri: String, version: Int, text: String): Json =
    Json.obj(
      "textDocument"   -> Json.obj("uri" -> uri.asJson, "version" -> version.asJson),
      "contentChanges" -> Json.arr(Json.obj("text" -> text.asJson))
    )

  def hoverParams(uri: String, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  def definitionParams(uri: String, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  def completionParams(uri: String, line: Int, character: Int): Json =
    textDocumentPositionParams(uri, line, character)

  def textDocumentPositionParams(uri: String, line: Int, character: Int): Json =
    Json.obj(
      "textDocument" -> Json.obj("uri" -> uri.asJson),
      "position" -> Json.obj(
        "line"      -> line.asJson,
        "character" -> character.asJson
      )
    )

  def parseHoverText(json: Json): Option[String] =
    json.hcursor
      .downField("result")
      .focus
      .flatMap(_.hcursor.downField("contents").focus)
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

  def parseCompletionItems(json: Json): Option[List[String]] =
    json.hcursor.downField("result").focus.flatMap { result =>
      result.asArray
        .orElse(result.hcursor.downField("items").focus.flatMap(_.asArray))
        .map(_.toList.flatMap(completionLabel))
    }

  private def completionLabel(json: Json): Option[String] =
    json.asString
      .orElse(json.hcursor.downField("label").as[String].toOption)
      .map(_.trim)
      .filter(_.nonEmpty)

  def parseDefinitionLocation(json: Json): Option[LspLocation] =
    json.hcursor.downField("result").focus.flatMap { result =>
      parseLocation(result).orElse(result.asArray.flatMap(_.toList.view.flatMap(parseLocation).headOption))
    }

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
    yield LspLocation(uri, range)

  // ── PublishDiagnostics ──────────────────────────────────────────────────────

  def parseDiagnostics(json: Json): Option[(String, List[Diagnostic])] =
    val c = json.hcursor.downField("params")
    for
      uri <- c.downField("uri").as[String].toOption
      diags = c
        .downField("diagnostics")
        .as[List[Json]]
        .getOrElse(Nil)
        .flatMap(parseDiagnostic)
    yield (uri, diags)

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
