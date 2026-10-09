package com.serenity.session

import java.time.Instant

import _root_.io.circe.Json
import _root_.io.circe.syntax.*
import com.serenity.TestWorkspaceTrees
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Comment ids, authors, times, replies and resolved state in the session file (#1903), and sessions written before
  * they existed.
  */
class SessionCommentThreadSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val wrote   = Instant.parse("2026-10-06T10:00:00Z")
  private val replied = Instant.parse("2026-10-06T11:30:00.250Z")

  private val thread = DocumentComment(
    CursorPosition(0, 0),
    CursorPosition(0, 5),
    "Tighten this",
    id = CommentId(4),
    author = Some("Ada"),
    createdAt = Some(wrote),
    editedAt = Some(replied),
    replies = List(CommentReply("Grace", replied, "Done"), CommentReply("Ada", replied, "Thanks\nreally")),
    resolved = true
  )

  private def stateWith(annotations: Annotations): AppState =
    val buffer = Buffer.fromString(BufferId(0), "hello world").copy(annotations = annotations)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

  private def restored(json: Json): Annotations =
    val decoded = json
      .as[SessionState]
      .fold(failure => fail(s"The session did not decode: $failure"), identity)
    SessionState.toAppState(decoded, Theme.default).persisted.buffers(BufferId(0)).annotations

  /** A session as it was written before comments had anything but a range and text. */
  private def legacyJson(annotations: Annotations): Json =
    val encoded = SessionState.fromAppState(stateWith(annotations)).asJson
    val stripped = encoded.hcursor
      .downField("buffers")
      .downArray
      .withFocus(_.mapObject(_.remove("nextCommentId")))
      .downField("documentComments")
      .withFocus(
        _.mapArray(
          _.map(
            _.mapObject(comment =>
              comment.keys.filterNot(Set("anchor", "focus", "text")).foldLeft(comment)((left, key) => left.remove(key))
            )
          )
        )
      )
      .top
      .getOrElse(fail("Expected the encoded session to have a buffer with comments"))
    stripped.mapObject(_.add("schemaVersion", Json.fromInt(4)))

  "A session" should "carry a comment's id, author, times, replies and resolved state through JSON" in {
    val json = SessionState.fromAppState(stateWith(Annotations(documentComments = List(thread)))).asJson

    restored(json).documentComments shouldBe List(thread)
  }

  it should "keep the next comment id, so a deleted comment's id is not reused after a restore" in {
    val annotations = Annotations(documentComments = List(thread), nextCommentId = CommentId(9))

    restored(SessionState.fromAppState(stateWith(annotations)).asJson).nextCommentId shouldBe CommentId(9)
  }

  it should "be written at schema version 5" in {
    SessionState.CurrentSchemaVersion.value shouldBe 5
    SessionState.fromAppState(stateWith(Annotations())).asJson.hcursor.get[Int]("schemaVersion") shouldBe Right(5)
  }

  "A session written before comments had ids" should "load, giving each comment a fresh id" in {
    val old = List(
      DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "First"),
      DocumentComment(CursorPosition(0, 6), CursorPosition(0, 11), "Second")
    )

    val annotations = restored(legacyJson(Annotations(documentComments = old)))

    annotations.documentComments.map(c => c.text -> c.id) shouldBe
      List("First" -> CommentId(1), "Second" -> CommentId(2))
    annotations.nextCommentId shouldBe CommentId(3)
  }

  it should "load comments as unresolved, with no author, times or replies" in {
    val old = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "Plain")

    val loaded = restored(legacyJson(Annotations(documentComments = List(old)))).documentComments

    loaded.map(c => (c.author, c.createdAt, c.editedAt, c.replies, c.resolved)) shouldBe
      List((None, None, None, Nil, false))
  }

  it should "load a comment with its range and text unchanged" in {
    val old = DocumentComment(CursorPosition(0, 2), CursorPosition(0, 9), "Keep my range")

    val loaded = restored(legacyJson(Annotations(documentComments = List(old)))).documentComments

    loaded.map(c => (c.anchor, c.focus, c.text)) shouldBe List((old.anchor, old.focus, old.text))
  }

  "A restored session with repeated comment ids" should "give the repeats ids of their own" in {
    val annotations = Annotations(documentComments = List(thread, thread.copy(text = "Twin")))

    val loaded = restored(SessionState.fromAppState(stateWith(annotations)).asJson).documentComments

    loaded.map(_.id).distinct should have size 2
    loaded.map(_.text) shouldBe List("Tighten this", "Twin")
  }
