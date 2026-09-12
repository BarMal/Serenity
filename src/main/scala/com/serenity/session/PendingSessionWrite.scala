package com.serenity.session

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** A durably-recorded description of a session-file/index update still in flight.
  *
  * `SessionManager` writes one of these (as a single atomic file) before touching the session files
  * or the index it is about to update. If the process crashes partway through applying the change,
  * the next `SessionManager` operation finds this file still present and replays it -- reapplying the
  * session-file writes/deletes and the index write it describes -- before doing anything else. Both
  * the underlying file writes (`AtomicFileWriter`) and the replay itself are idempotent, so replaying
  * an already-applied or partially-applied transaction is always safe.
  */
final case class PendingSessionWrite(
    writes: Map[String, String],
    deletes: List[String],
    indexJson: String
)

object PendingSessionWrite:
  given Encoder[PendingSessionWrite] = deriveEncoder
  given Decoder[PendingSessionWrite] = deriveDecoder
