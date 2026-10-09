package com.serenity.lsp.client

import io.circe.Json

/** Replies to the requests a server sends this client that need nothing from the application. Every request gets
  * exactly one reply -- some servers hold back features until theirs arrives -- so a method this client does not serve
  * is refused with `MethodNotFound` rather than left unanswered. The requests that wait on the application,
  * `workspace/applyEdit` and `window/showMessageRequest`, are answered by `LspConnection` once it has answered them.
  */
object LspServerRequests:

  val MethodNotFound: Int = -32601

  def reply(id: ServerRequestId, method: LspMethod, params: Json): Json =
    handlers.get(method.value) match
      case Some(handler) => LspProtocol.response(id, handler(params))
      case None          => LspProtocol.errorResponse(id, MethodNotFound, s"Method not found: ${method.value}")

  private val acknowledge: Json => Json = _ => Json.Null

  private val handlers: Map[String, Json => Json] = Map(
    "workspace/configuration"        -> noConfiguration,
    "client/registerCapability"      -> acknowledge,
    "client/unregisterCapability"    -> acknowledge,
    "window/workDoneProgress/create" -> acknowledge
  )

  /** Serenity has no per-server settings to hand out, so every requested section is answered with `null`, which the
    * spec defines as "no configuration" -- one entry per item, in order, as servers index the reply by position.
    */
  private def noConfiguration(params: Json): Json =
    Json.fromValues(params.hcursor.downField("items").values.toList.flatten.map(_ => Json.Null))
