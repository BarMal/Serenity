package com.serenity.state.manager

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.keystroke.events.{LspEvent, Undo}
import com.serenity.lsp.model.{LspPosition, LspRange, LspTextEdit}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import org.scalatest.OptionValues
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A rename's edits to a file no buffer holds open it as a buffer, apply the edits and leave it unsaved and undoable
  * (#1965), end to end through `StateManager`.
  */
class RenameOpensUnopenedFilesSpec extends AnyFlatSpec with Matchers with Eventually with OptionValues:

  override given patienceConfig: PatienceConfig =
    PatienceConfig(timeout = org.scalatest.time.Span(20, org.scalatest.time.Seconds))

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def stateManager(): StateManager =
    StateManager
      .apply(
        LoggerFactory[IO].getLogger(using LoggerName("RenameOpensUnopenedFilesSpec")),
        dictionaryCache = SharedDictionary.default
      )
      .unsafeRunSync()

  private def opened(sm: StateManager, path: java.nio.file.Path): Option[Buffer] =
    sm.getCurrentState.unsafeRunSync().persisted.buffers.values.find(_.document.filePath.contains(path))

  "A rename naming a file that is not open" should "open it, edit it unsaved, leave the user's focus, and undo as one step" in
    TestTemp.within("rename-open") { dir =>
      val file = dir.resolve("Other.scala")
      Files.writeString(file, "def f = old")
      val sm          = stateManager()
      val before      = sm.getCurrentState.unsafeRunSync()
      val focusBefore = before.persisted.focus
      val edit        = LspTextEdit(LspRange(LspPosition(0, 8), LspPosition(0, 11)), "renamed")

      sm.applyEvent(LspEvent.LspRenameReceived(Map(file.toUri.toString -> List(edit)), CursorPosition(0, 0)))
        .unsafeRunSync()

      eventually {
        opened(sm, file).map(_.document.content.collect()) shouldBe Some("def f = renamed")
      }
      val renamed = opened(sm, file).value
      renamed.document.isDirty shouldBe true
      Files.readString(file) shouldBe "def f = old"
      sm.getCurrentState.unsafeRunSync().persisted.focus shouldBe focusBefore

      sm.applyEvent(Undo).unsafeRunSync()
      eventually {
        opened(sm, file).map(_.document.content.collect()) shouldBe Some("def f = old")
      }
    }
