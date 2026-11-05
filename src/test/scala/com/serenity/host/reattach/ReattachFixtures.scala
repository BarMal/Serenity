package com.serenity.host.reattach

import io.circe.Json
import io.circe.syntax.*

/** JSON-RPC messages as a language client and server exchange them, for driving the reattach state machine. */
object ReattachFixtures:

  def request(id: Json, method: String, params: Json = Json.obj()): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "method" -> method.asJson, "params" -> params)

  def notification(method: String, params: Json = Json.obj()): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "method" -> method.asJson, "params" -> params)

  def response(id: Json, result: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "result" -> result)

  def errorResponse(id: Json, code: Int): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "error" -> Json.obj("code" -> code.asJson))

  def initializeParams(processId: Int = 100, capabilities: Json = Json.obj()): Json =
    Json.obj(
      "processId"    -> processId.asJson,
      "rootUri"      -> "file:///work/serenity".asJson,
      "capabilities" -> capabilities
    )

  def didOpen(uri: String): Json =
    notification("textDocument/didOpen", Json.obj("textDocument" -> Json.obj("uri" -> uri.asJson)))

  def didClose(uri: String): Json =
    notification("textDocument/didClose", Json.obj("textDocument" -> Json.obj("uri" -> uri.asJson)))

  def registration(id: String, method: String): Json =
    Json.obj("id" -> id.asJson, "method" -> method.asJson)

  def registerCapability(id: Json, registrations: Json*): Json =
    request(id, "client/registerCapability", Json.obj("registrations" -> Json.arr(registrations*)))

  def progressCreate(id: Json, token: String): Json =
    request(id, "window/workDoneProgress/create", Json.obj("token" -> token.asJson))

  def progress(token: String, value: Json): Json =
    notification("$/progress", Json.obj("token" -> token.asJson, "value" -> value))

  def progressValue(kind: String, fields: (String, Json)*): Json =
    Json.obj((("kind" -> kind.asJson) +: fields)*)

  def num(value: Long): Json   = value.asJson
  def str(value: String): Json = value.asJson
