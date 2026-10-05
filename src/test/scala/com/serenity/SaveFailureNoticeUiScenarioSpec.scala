package com.serenity

import java.io.IOException
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.FileManager
import com.serenity.keystroke.events.{InsertChar, SaveFile}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade
import com.serenity.state.models.*
import com.serenity.state.reducers.NoticeReducer
import com.serenity.testkit.AwaitCondition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end scenario for #1717: Ctrl+S onto a disk that refuses the write paints an error notice in the editor's
  * bottom-right corner, through the same state pipeline and renderer the app uses, while the editor keeps focus.
  */
class SaveFailureNoticeUiScenarioSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class FullDisk extends FileManager:
    override def saveBuffer(buffer: Buffer, path: Path): IO[Buffer] =
      IO.raiseError(new IOException("No space left on device"))

  "A save that cannot be written" should "show an error notice in the corner while the editor keeps focus" in {
    val directory = Files.createTempDirectory("save-failure-notice-scenario")
    val path      = Files.writeString(directory.resolve("notes.md"), "draft")
    val manager   = StateManagerTestFacade.stateManagerWithFileManager(new FullDisk).unsafeRunSync()
    val driver    = UiScenarioDriver.over(manager).unsafeRunSync()
    manager.fileOpener.openFile(path).unsafeRunSync()
    driver.dispatch(InsertChar('a')).unsafeRunSync()

    driver.dispatch(SaveFile).unsafeRunSync()
    AwaitCondition.awaitValue(driver.state)(NoticeReducer.visible(_).nonEmpty).unsafeRunSync()

    val frame = driver.renderFrame("save-failure-notice").unsafeRunSync()
    val drawn = frame.evidence.drawnText
    val message = drawn
      .find(_.text.contains("Couldn't save notes.md"))
      .getOrElse(fail(s"no notice text drawn: ${drawn.map(_.text)}"))
    message.text should include("the disk is full")
    drawn.exists(_.text.contains("Error")) shouldBe true
    frame.evidence.focus shouldBe a[Focus.EditorPane]
    val viewport = driver.environment.viewport
    message.bounds.y should be > viewport.height / 2
    message.bounds.right should be > viewport.width / 2
    frame.evidence.layoutViolations shouldBe empty
  }
