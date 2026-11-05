package com.serenity.host.reattach

import io.circe.Json

/** The result a server gave the first `initialize`, and the digest of the request that earned it. */
final case class CachedInitialize(fingerprint: String, result: Json)

/** A request a client sent, waiting for the server's answer under the id the host gave it. */
final case class PendingClientRequest(clientId: Json, initializeFingerprint: Option[String])

/** A work-done progress the server created; `begin` is the latest state of it, once it has begun. */
final case class RunningProgress(begin: Option[Json])

/** Everything the host must remember about one server so that a different client can take over from the last one.
  *
  * @param attached
  *   whether a client is connected; messages from a client are ignored while none is
  * @param clientRequests
  *   requests the current client has outstanding, by the id the server was given
  * @param serverRequests
  *   ids of requests the server made that the current client has not yet answered
  * @param replayIds
  *   ids of requests the host made to the client itself, whose answers are not the server's business
  * @param nextRequestId
  *   never goes back, so an answer meant for an earlier client can never match a later request
  */
final case class ReattachState(
    attached: Boolean,
    initialize: Option[CachedInitialize],
    swallowInitialized: Boolean,
    openDocuments: Set[String],
    registrations: Map[String, Json],
    progress: Map[Json, RunningProgress],
    clientRequests: Map[Long, PendingClientRequest],
    serverRequests: List[Json],
    replayIds: Set[Json],
    nextRequestId: Long,
    nextReplayId: Long
):

  def hasCachedInitialize: Boolean = initialize.isDefined

object ReattachState:

  /** A server nobody has spoken to yet, with its first client attached. */
  val fresh: ReattachState = ReattachState(
    attached = true,
    initialize = None,
    swallowInitialized = false,
    openDocuments = Set.empty,
    registrations = Map.empty,
    progress = Map.empty,
    clientRequests = Map.empty,
    serverRequests = Nil,
    replayIds = Set.empty,
    nextRequestId = 1,
    nextReplayId = 1
  )
