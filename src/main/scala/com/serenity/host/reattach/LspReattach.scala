package com.serenity.host.reattach

import io.circe.Json
import io.circe.syntax.*

import LspMessage.*
import ReattachAction.*

/** How a host lets one language server outlive its clients (#2522 R3). LSP is stateful per connection, so the host
  * cannot just pass bytes: it keeps what the server was told, and turns "a different client" into something the server
  * need not notice.
  *
  *   - `initialize` is answered from the first result, and the `initialized` that follows is swallowed;
  *   - at detach every open document is closed, so a new client's `didOpen` is the server's next word about it;
  *   - the server's dynamic registrations and running progress are replayed to the new client;
  *   - the server never sees a client's request ids, so an answer meant for an earlier client matches nothing.
  *
  * Every function is pure: it takes the state and one message, and returns the next state and what to do.
  */
object LspReattach:

  private val RequestCancelled = -32800

  def fromClient(state: ReattachState, json: Json): (ReattachState, List[ReattachAction]) =
    if !state.attached then (state, Nil)
    else
      LspMessage.parse(json) match
        case Some(Request(id, "initialize", params, raw)) => initialize(state, id, params, raw)
        case Some(Request(id, _, _, raw))                 => forwardRequest(state, id, raw, None)
        case Some(Notification(method, params, raw))      => clientNotification(state, method, params, raw)
        case Some(Response(id, raw))                      => clientResponse(state, id, raw)
        case None                                         => (state, Nil)

  def fromServer(state: ReattachState, json: Json): (ReattachState, List[ReattachAction]) =
    LspMessage.parse(json) match
      case Some(Response(id, raw))                 => serverResponse(state, id, raw)
      case Some(Request(id, method, params, raw))  => serverRequest(state, id, method, params, raw)
      case Some(Notification(method, params, raw)) => serverNotification(state, method, params, raw)
      case None                                    => (state, Nil)

  /** The client is gone. The server is left initialized with no document open and nothing waiting on a client. */
  def detach(state: ReattachState): (ReattachState, List[ReattachAction]) =
    if !state.attached then (state, Nil)
    else
      val closes = state.openDocuments.toList.sorted.map(uri =>
        ToServer(notification("textDocument/didClose", Json.obj("textDocument" -> Json.obj("uri" -> uri.asJson))))
      )
      val cancels = state.serverRequests.map(id => ToServer(failure(id, RequestCancelled)))
      val next = state.copy(
        attached = false,
        swallowInitialized = false,
        openDocuments = Set.empty,
        clientRequests = Map.empty,
        serverRequests = Nil,
        replayIds = Set.empty
      )
      (next, closes ++ cancels)

  /** A client takes over: it is told what the server registered and what it is still busy with. */
  def attach(state: ReattachState): (ReattachState, List[ReattachAction]) =
    if state.attached then (state, Nil)
    else
      val registrations = replayRegistrations(state)
      val progress      = replayProgress(registrations._1)
      (progress._1.copy(attached = true), registrations._2 ++ progress._2)

  private def initialize(
    state: ReattachState,
    id: Json,
    params: Json,
    raw: Json
  ): (ReattachState, List[ReattachAction]) =
    val fingerprint = InitializeFingerprint.of(params)
    state.initialize match
      case Some(cached) if cached.fingerprint == fingerprint =>
        (state.copy(swallowInitialized = true), List(ToClient(result(id, cached.result))))
      case Some(_) =>
        val restarted = ReattachState.fresh.copy(nextRequestId = state.nextRequestId, nextReplayId = state.nextReplayId)
        val (next, forward) = forwardRequest(restarted, id, raw, Some(fingerprint))
        (next, RestartServer :: forward)
      case None => forwardRequest(state, id, raw, Some(fingerprint))

  private def forwardRequest(
    state: ReattachState,
    id: Json,
    raw: Json,
    fingerprint: Option[String]
  ): (ReattachState, List[ReattachAction]) =
    val serverId = state.nextRequestId
    val next = state.copy(
      clientRequests = state.clientRequests.updated(serverId, PendingClientRequest(id, fingerprint)),
      nextRequestId = serverId + 1
    )
    (next, List(ToServer(withId(raw, serverId.asJson))))

  private def clientNotification(
    state: ReattachState,
    method: String,
    params: Json,
    raw: Json
  ): (ReattachState, List[ReattachAction]) =
    method match
      case "initialized" if state.swallowInitialized => (state.copy(swallowInitialized = false), Nil)
      case "textDocument/didOpen" =>
        (state.copy(openDocuments = state.openDocuments ++ documentUri(params)), List(ToServer(raw)))
      case "textDocument/didClose" =>
        (state.copy(openDocuments = state.openDocuments -- documentUri(params)), List(ToServer(raw)))
      case "$/cancelRequest" => (state, cancelOnServer(state, params).toList)
      case _                 => (state, List(ToServer(raw)))

  private def cancelOnServer(state: ReattachState, params: Json): Option[ReattachAction] =
    params.hcursor
      .downField("id")
      .focus
      .flatMap { clientId =>
        state.clientRequests.collectFirst { case (serverId, pending) if pending.clientId == clientId => serverId }
      }
      .map(serverId => ToServer(notification("$/cancelRequest", Json.obj("id" -> serverId.asJson))))

  private def clientResponse(state: ReattachState, id: Json, raw: Json): (ReattachState, List[ReattachAction]) =
    if state.replayIds.contains(id) then (state.copy(replayIds = state.replayIds - id), Nil)
    else if state.serverRequests.contains(id) then
      (state.copy(serverRequests = state.serverRequests.filterNot(_ == id)), List(ToServer(raw)))
    else (state, Nil)

  private def serverResponse(state: ReattachState, id: Json, raw: Json): (ReattachState, List[ReattachAction]) =
    val pending =
      id.asNumber.flatMap(_.toLong).flatMap(serverId => state.clientRequests.get(serverId).map(serverId -> _))
    pending match
      case Some((serverId, request)) if state.attached =>
        val cached = for
          fingerprint <- request.initializeFingerprint
          value       <- raw.hcursor.downField("result").focus
        yield CachedInitialize(fingerprint, value)
        val next =
          state.copy(clientRequests = state.clientRequests - serverId, initialize = cached.orElse(state.initialize))
        (next, List(ToClient(withId(raw, request.clientId))))
      case _ => (state, Nil)

  private def serverRequest(
    state: ReattachState,
    id: Json,
    method: String,
    params: Json,
    raw: Json
  ): (ReattachState, List[ReattachAction]) =
    val tracked = trackServerRequest(state, method, params)
    if state.attached then (tracked.copy(serverRequests = tracked.serverRequests :+ id), List(ToClient(raw)))
    else (tracked, List(ToServer(answerWithoutClient(id, method))))

  private def answerWithoutClient(id: Json, method: String): Json =
    if registrationMethods.contains(method) then result(id, Json.Null) else failure(id, RequestCancelled)

  private val registrationMethods =
    Set("client/registerCapability", "client/unregisterCapability", "window/workDoneProgress/create")

  private def trackServerRequest(state: ReattachState, method: String, params: Json): ReattachState =
    method match
      case "client/registerCapability" =>
        val added =
          ids(params, "registrations").flatMap(registration => registrationId(registration).map(_ -> registration))
        state.copy(registrations = state.registrations ++ added)
      case "client/unregisterCapability" =>
        val removed = ids(params, "unregisterations").flatMap(registrationId)
        state.copy(registrations = state.registrations -- removed)
      case "window/workDoneProgress/create" =>
        val token = params.hcursor.downField("token").focus
        state.copy(progress =
          token.fold(state.progress)(t => state.progress.updatedWith(t)(_.orElse(Some(RunningProgress(None)))))
        )
      case _ => state

  private def ids(params: Json, field: String): List[Json] =
    params.hcursor.downField(field).values.map(_.toList).getOrElse(Nil)

  private def registrationId(registration: Json): Option[String] = registration.hcursor.get[String]("id").toOption

  private def serverNotification(
    state: ReattachState,
    method: String,
    params: Json,
    raw: Json
  ): (ReattachState, List[ReattachAction]) =
    val next = if method == "$/progress" then trackProgress(state, params) else state
    (next, if state.attached then List(ToClient(raw)) else Nil)

  private val mergedOnReport = List("message", "percentage")

  private def trackProgress(state: ReattachState, params: Json): ReattachState =
    val cursor = params.hcursor
    val value  = cursor.downField("value").focus.getOrElse(Json.Null)
    (cursor.downField("token").focus, value.hcursor.get[String]("kind").toOption) match
      case (Some(token), Some("begin")) =>
        state.copy(progress = state.progress.updated(token, RunningProgress(Some(value))))
      case (Some(token), Some("report")) =>
        val merged = state.progress.get(token).flatMap(_.begin).map(begin => mergeReport(begin, value))
        state.copy(progress = state.progress.updatedWith(token)(_.map(_ => RunningProgress(merged))))
      case (Some(token), Some("end")) => state.copy(progress = state.progress - token)
      case _                          => state

  private def mergeReport(begin: Json, report: Json): Json =
    val updates = mergedOnReport.flatMap(field => report.hcursor.downField(field).focus.map(field -> _))
    begin.mapObject(fields => updates.foldLeft(fields)((merged, update) => merged.add(update._1, update._2)))

  private def replayRegistrations(state: ReattachState): (ReattachState, List[ReattachAction]) =
    if state.registrations.isEmpty then (state, Nil)
    else
      val sorted = state.registrations.toList.sortBy(_._1).map(_._2)
      replayRequest(state, "client/registerCapability", Json.obj("registrations" -> Json.fromValues(sorted)))

  private def replayProgress(state: ReattachState): (ReattachState, List[ReattachAction]) =
    state.progress.toList.sortBy(_._1.noSpaces).foldLeft((state, List.empty[ReattachAction])) {
      case ((current, actions), (token, running)) =>
        val (next, create) = replayRequest(current, "window/workDoneProgress/create", Json.obj("token" -> token))
        val begin =
          running.begin.map(value => ToClient(notification("$/progress", Json.obj("token" -> token, "value" -> value))))
        (next, actions ++ create ++ begin)
    }

  private def replayRequest(state: ReattachState, method: String, params: Json): (ReattachState, List[ReattachAction]) =
    val id   = s"host-replay-${state.nextReplayId}".asJson
    val next = state.copy(replayIds = state.replayIds + id, nextReplayId = state.nextReplayId + 1)
    (next, List(ToClient(request(id, method, params))))
