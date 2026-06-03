package com.serenity.session

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class SessionId(value: String)

case class SessionMetadata(
    id: SessionId,
    displayName: String,
    sessionFileName: String,
    createdAtEpochMillis: Long,
    updatedAtEpochMillis: Long,
    lastOpenedAtEpochMillis: Option[Long] = None
):
  def renamed(newDisplayName: String, updatedAt: Long): SessionMetadata =
    copy(displayName = newDisplayName, updatedAtEpochMillis = updatedAt)

case class SessionIndex(
    sessions: List[SessionMetadata],
    currentSessionId: Option[SessionId] = None
):

  def rename(sessionId: SessionId, newDisplayName: String, updatedAt: Long): SessionIndex =
    copy(
      sessions = sessions.map { metadata =>
        if metadata.id == sessionId then metadata.renamed(newDisplayName, updatedAt)
        else metadata
      }
    )

  def remove(sessionId: SessionId): SessionIndex =
    val remainingSessions = sessions.filterNot(_.id == sessionId)
    val remainingCurrent =
      currentSessionId.filterNot(_ == sessionId)

    copy(
      sessions = remainingSessions,
      currentSessionId = remainingCurrent
    )

object SessionId:
  given Encoder[SessionId] = Encoder.encodeString.contramap(_.value)
  given Decoder[SessionId] = Decoder.decodeString.map(SessionId.apply)

object SessionMetadata:
  given Encoder[SessionMetadata] = deriveEncoder
  given Decoder[SessionMetadata] = deriveDecoder

object SessionIndex:
  val empty: SessionIndex = SessionIndex(Nil, None)

  given Encoder[SessionIndex] = deriveEncoder
  given Decoder[SessionIndex] = deriveDecoder
