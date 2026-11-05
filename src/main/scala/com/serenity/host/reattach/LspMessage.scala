package com.serenity.host.reattach

import io.circe.Json
import io.circe.syntax.*

/** A JSON-RPC message told apart by which of `id` and `method` it has. The original JSON is kept so a forwarded message
  * carries every field it arrived with.
  */
enum LspMessage:
  case Request(id: Json, method: String, params: Json, json: Json)
  case Notification(method: String, params: Json, json: Json)
  case Response(id: Json, json: Json)

object LspMessage:

  def parse(json: Json): Option[LspMessage] =
    val cursor = json.hcursor
    val id     = cursor.downField("id").focus
    val method = cursor.get[String]("method").toOption
    val params = cursor.downField("params").focus.getOrElse(Json.Null)
    (id, method) match
      case (Some(id), Some(method)) => Some(Request(id, method, params, json))
      case (None, Some(method))     => Some(Notification(method, params, json))
      case (Some(id), None)         => Some(Response(id, json))
      case (None, None)             => None

  def withId(json: Json, id: Json): Json = json.mapObject(_.add("id", id))

  def result(id: Json, value: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "result" -> value)

  def failure(id: Json, code: Int): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "error" -> Json.obj("code" -> code.asJson))

  def request(id: Json, method: String, params: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "method" -> method.asJson, "params" -> params)

  def notification(method: String, params: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "method" -> method.asJson, "params" -> params)

  def documentUri(params: Json): Option[String] =
    params.hcursor.downField("textDocument").get[String]("uri").toOption
