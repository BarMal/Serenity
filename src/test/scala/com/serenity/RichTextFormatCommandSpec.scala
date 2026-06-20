package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandIntent, CommandRegistry}
import com.serenity.richtext.InlineMark
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{BufferId, Selection}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class RichTextFormatCommandSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("RichTextFormatCommandSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def selectedStateManager(text: String, selection: Selection): (StateManager, BufferId) =
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer(text).unsafeRunSync()
    stateManager.setBufferForPane(com.serenity.state.models.PaneId(0), bufferId).unsafeRunSync()
    stateManager
      .updateState { state =>
        state.copy(
          buffers = state.buffers.updated(
            bufferId,
            state.buffers(bufferId).copy(selection = Some(selection), cursors = List(selection.focus))
          )
        )
      }
      .unsafeRunSync()
    (stateManager, bufferId)

  "Rich text format commands" should "be registered in the command runner" in {
    val registry = CommandRegistry.withToggleUI

    registry.findCommand("bold").map(_.intent) shouldBe Some(CommandIntent.ToggleRichTextMark(InlineMark.Bold))
    registry.findCommand("italic").map(_.intent) shouldBe Some(
      CommandIntent.ToggleRichTextMark(InlineMark.Italic)
    )
    registry.findCommand("underline").map(_.intent) shouldBe Some(
      CommandIntent.ToggleRichTextMark(InlineMark.Underline)
    )
  }

  it should "apply bold to the active selection" in {
    val (stateManager, bufferId) =
      selectedStateManager(
        "alpha beta",
        Selection(com.serenity.state.models.CursorPosition(0, 6), com.serenity.state.models.CursorPosition(0, 10))
      )
    val command = CommandRegistry.withToggleUI.findCommand("bold").getOrElse(fail("missing bold"))

    stateManager.executeCommand(command).unsafeRunSync()

    val buffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.isDirty shouldBe true
    buffer.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks) shouldBe Some(Set(InlineMark.Bold))
  }

  it should "toggle bold off when the active selection is already bold" in {
    val (stateManager, bufferId) =
      selectedStateManager(
        "alpha beta",
        Selection(com.serenity.state.models.CursorPosition(0, 6), com.serenity.state.models.CursorPosition(0, 10))
      )
    val command = CommandRegistry.withToggleUI.findCommand("bold").getOrElse(fail("missing bold"))

    stateManager.executeCommand(command).unsafeRunSync()
    stateManager.executeCommand(command).unsafeRunSync()

    val buffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.richTextDocument.map(_.plainText) shouldBe Some("alpha beta")
    buffer.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .map(_.runs) shouldBe Some(List(com.serenity.richtext.RichTextRun("alpha beta")))
  }
