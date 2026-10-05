package com.serenity.session

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder}

/** A durably-recorded description of a session-file/index update still in flight; see [[SessionWriteJournal]].
  *
  * `staged` names session files whose new content waits beside them as `<name>.staged`. `writes` carries content
  * inline, and only markers written before #1912 use it: a crash under an older Serenity must still replay.
  */
final case class PendingSessionWrite(
    writes: Map[String, String] = Map.empty,
    deletes: List[String],
    indexJson: String,
    staged: List[String] = Nil
)

object PendingSessionWrite:
  given Encoder[PendingSessionWrite] = deriveEncoder

  given Decoder[PendingSessionWrite] = Decoder.instance { cursor =>
    for
      writes    <- cursor.getOrElse[Map[String, String]]("writes")(Map.empty)
      deletes   <- cursor.get[List[String]]("deletes")
      indexJson <- cursor.get[String]("indexJson")
      staged    <- cursor.getOrElse[List[String]]("staged")(Nil)
    yield PendingSessionWrite(writes, deletes, indexJson, staged)
  }
