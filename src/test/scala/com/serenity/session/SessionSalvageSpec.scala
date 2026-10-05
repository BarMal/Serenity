package com.serenity.session

import _root_.io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionSalvageSpec extends AnyFlatSpec with Matchers:

  private val newer   = SessionState.CurrentSchemaVersion.value + 1
  private val current = SessionState.CurrentSchemaVersion.value

  private def buffer(filePath: Option[String], isDirty: Boolean, unsavedContent: Option[String]): Json =
    Json.obj(
      "id"             -> Json.fromInt(1),
      "filePath"       -> filePath.fold(Json.Null)(Json.fromString),
      "isDirty"        -> Json.fromBoolean(isDirty),
      "unsavedContent" -> unsavedContent.fold(Json.Null)(Json.fromString)
    )

  private def session(schemaVersion: Int, buffers: Json*): String =
    Json.obj("schemaVersion" -> Json.fromInt(schemaVersion), "buffers" -> Json.arr(buffers*)).spaces2

  "SessionSalvage.reason" should "call a session from a newer schema a newer version, not corruption" in {
    SessionSalvage.reason(session(newer)) shouldBe UnreadableReason.NewerVersion(newer)
  }

  it should "call unparseable JSON corrupt" in {
    SessionSalvage.reason("""{ "schemaVersion": 3, "buffers": [ { "unsavedContent": "half""") shouldBe
      UnreadableReason.Corrupt
  }

  it should "call a current-schema file that failed to decode corrupt" in {
    SessionSalvage.reason(session(current)) shouldBe UnreadableReason.Corrupt
  }

  "SessionSalvage.salvage" should "keep unsaved and untitled text from a parseable session, labelled by file name" in {
    val json = session(
      4,
      buffer(Some("/work/notes.md"), isDirty = true, Some("edited notes")),
      buffer(Some("/work/clean.md"), isDirty = false, Some("same as disk")),
      buffer(None, isDirty = false, Some("an untitled draft")),
      buffer(None, isDirty = false, Some(""))
    )

    SessionSalvage.salvage(json) shouldBe List(
      SalvagedText("notes.md", "edited notes"),
      SalvagedText("untitled-3", "an untitled draft")
    )
  }

  it should "scan a truncated session for every complete unsaved text, unescaping it" in {
    val truncated =
      """{ "schemaVersion": 3, "buffers": [ { "id": 1, "unsavedContent": "line one\nsaid \"hi\"" },""" +
        """ { "id": 2, "unsavedContent": "second" }, { "id": 3, "unsavedContent": "cut off mid"""

    SessionSalvage.salvage(truncated) shouldBe List(
      SalvagedText("recovered-1", "line one\nsaid \"hi\""),
      SalvagedText("recovered-2", "second")
    )
  }

  it should "read a buffer's unsaved text from the content file its contentRef names" in {
    val json = session(
      current,
      buffer(Some("/work/notes.md"), isDirty = true, None).deepMerge(Json.obj("contentRef" -> Json.fromString("abc"))),
      buffer(Some("/work/gone.md"), isDirty = true, None).deepMerge(
        Json.obj("contentRef" -> Json.fromString("missing"))
      )
    )

    SessionSalvage.salvage(json, Map("abc" -> "edited in a content file").get) shouldBe List(
      SalvagedText("notes.md", "edited in a content file")
    )
  }

  it should "resolve contentRefs in a truncated session, in file order alongside inline text" in {
    val truncated =
      """{ "buffers": [ { "id": 1, "contentRef": "aa" }, { "id": 2, "unsavedContent": "inline" },""" +
        """ { "id": 3, "contentRef": "bb" }, { "id": 4, "contentRef": "cut"""

    SessionSalvage.salvage(truncated, Map("aa" -> "first", "bb" -> "third").get) shouldBe List(
      SalvagedText("recovered-1", "first"),
      SalvagedText("recovered-2", "inline"),
      SalvagedText("recovered-3", "third")
    )
  }

  "SessionSalvage.backupFileName" should "stamp the reason and the time onto the original name" in {
    SessionSalvage.backupFileName("session.json", UnreadableReason.Corrupt, 42L) shouldBe "session.json.corrupt-42"
    SessionSalvage.backupFileName("session.json", UnreadableReason.NewerVersion(newer), 42L) shouldBe
      "session.json.newer-42"
  }

  "SessionSalvage.recoveredFileName" should "keep a readable, filesystem-safe name" in {
    SessionSalvage.recoveredFileName(0, "notes.md") shouldBe "01-notes.md.txt"
    SessionSalvage.recoveredFileName(11, "a/b:c*d") shouldBe "12-a_b_c_d.txt"
  }
