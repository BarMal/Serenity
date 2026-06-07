package com.serenity.lsp.client

import io.circe.Json
import io.circe.syntax.*

import com.serenity.lsp.model.*

object LspProtocol:

  case class JsonRpcRequest(id: Long, method: String, params: Json)
  case class JsonRpcNotification(method: String, params: Json)

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
          "publishDiagnostics" -> Json.obj("relatedInformation" -> true.asJson)
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
      startLine <- c.downField("range").downField("start").downField("line").as[Int].toOption
      startChar <- c.downField("range").downField("start").downField("character").as[Int].toOption
      endLine   <- c.downField("range").downField("end").downField("line").as[Int].toOption
      endChar   <- c.downField("range").downField("end").downField("character").as[Int].toOption
      message   <- c.downField("message").as[String].toOption
    yield
      val severity = c.downField("severity").as[Int].toOption.flatMap(DiagnosticSeverity.fromCode)
      val source   = c.downField("source").as[String].toOption
      val code     = c.downField("code").as[String].orElse(c.downField("code").as[Int].map(_.toString)).toOption
      Diagnostic(
        range = LspRange(LspPosition(startLine, startChar), LspPosition(endLine, endChar)),
        severity = severity,
        message = message,
        source = source,
        code = code
      )
