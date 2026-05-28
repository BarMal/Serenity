package com.serenity

import com.serenity.session.{SessionId, SessionIndex, SessionMetadata}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionIndexSpec extends AnyFlatSpec with Matchers:

  "SessionIndex" should "round-trip through json" in {
    val index = SessionIndex(
      sessions = List(
        SessionMetadata(
          id = SessionId("session-1"),
          displayName = "Daily notes",
          sessionFileName = "session-1.json",
          createdAtEpochMillis = 1000L,
          updatedAtEpochMillis = 2000L,
          lastOpenedAtEpochMillis = Some(2500L)
        ),
        SessionMetadata(
          id = SessionId("session-2"),
          displayName = "Refactor branch",
          sessionFileName = "session-2.json",
          createdAtEpochMillis = 3000L,
          updatedAtEpochMillis = 4000L,
          lastOpenedAtEpochMillis = None
        )
      ),
      currentSessionId = Some(SessionId("session-2"))
    )

    val decoded = _root_.io.circe.parser.decode[SessionIndex](
      _root_.io.circe.syntax.EncoderOps(index).asJson.noSpaces
    )

    decoded.shouldBe(Right(index))
  }

  it should "rename a session without changing its identity" in {
    val metadata = SessionMetadata(
      id = SessionId("session-1"),
      displayName = "Old name",
      sessionFileName = "session-1.json",
      createdAtEpochMillis = 1000L,
      updatedAtEpochMillis = 2000L,
      lastOpenedAtEpochMillis = Some(2500L)
    )
    val index = SessionIndex(sessions = List(metadata), currentSessionId = Some(metadata.id))

    val renamed = index.rename(metadata.id, "New name", 9000L)

    renamed.sessions should have size 1
    renamed.sessions.head.id shouldBe metadata.id
    renamed.sessions.head.displayName shouldBe "New name"
    renamed.sessions.head.createdAtEpochMillis shouldBe 1000L
    renamed.sessions.head.updatedAtEpochMillis shouldBe 9000L
    renamed.currentSessionId shouldBe Some(metadata.id)
  }

  it should "remove a session and clear current selection when needed" in {
    val first = SessionMetadata(
      id = SessionId("session-1"),
      displayName = "One",
      sessionFileName = "session-1.json",
      createdAtEpochMillis = 1000L,
      updatedAtEpochMillis = 1000L
    )
    val second = SessionMetadata(
      id = SessionId("session-2"),
      displayName = "Two",
      sessionFileName = "session-2.json",
      createdAtEpochMillis = 2000L,
      updatedAtEpochMillis = 2000L
    )

    val index = SessionIndex(
      sessions = List(first, second),
      currentSessionId = Some(second.id)
    )

    val updated = index.remove(second.id)

    updated.sessions shouldBe List(first)
    updated.currentSessionId shouldBe None
  }
