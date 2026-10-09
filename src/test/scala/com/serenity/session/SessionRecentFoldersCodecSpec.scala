package com.serenity.session

import java.nio.file.Path

import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The recent folders are saved with the recent files, and a session saved before them still loads. */
class SessionRecentFoldersCodecSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val folders = List(Path.of("/work/book"), Path.of("/work/notes"))

  private val sessionWithFolders: SessionState =
    val initial = AppState.initial
    SessionState.fromAppState(initial.copy(persisted = initial.persisted.copy(recentFolders = folders)))

  "SessionState" should "take the recent folders from the app state it snapshots" in {
    sessionWithFolders.recentFolders shouldBe folders.map(_.toString)
  }

  it should "round-trip the recent folders through its JSON codec" in {
    decode[SessionState](sessionWithFolders.asJson.noSpaces).map(_.recentFolders) shouldBe
      Right(folders.map(_.toString))
  }

  it should "write them under recentFolders, beside recentFiles" in {
    sessionWithFolders.asJson.hcursor.get[List[String]]("recentFolders") shouldBe Right(folders.map(_.toString))
  }

  it should "decode a session file written before recent folders as having none" in {
    val legacyJson = sessionWithFolders.asJson.mapObject(_.remove("recentFolders"))

    decode[SessionState](legacyJson.noSpaces).map(_.recentFolders) shouldBe Right(Nil)
  }

  it should "restore the recent folders into the app state, a legacy file's as none" in {
    SessionState.toAppState(sessionWithFolders, Theme.default).persisted.recentFolders shouldBe folders
    SessionState
      .toAppState(sessionWithFolders.copy(recentFolders = Nil), Theme.default)
      .persisted
      .recentFolders shouldBe
      Nil
  }

  it should "be written at schema version 6, which an older build refuses rather than dropping the folders" in {
    SessionState.CurrentSchemaVersion.value shouldBe 6
    sessionWithFolders.asJson.hcursor.get[Int]("schemaVersion") shouldBe Right(6)
  }

  it should "still load a session saved at schema version 5" in {
    val version5 = sessionWithFolders.asJson.mapObject(_.remove("recentFolders").add("schemaVersion", 5.asJson))

    decode[SessionState](version5.noSpaces).map(state => (state.recentFolders, state.schemaVersion.value)) shouldBe
      Right((Nil, 5))
  }
end SessionRecentFoldersCodecSpec
