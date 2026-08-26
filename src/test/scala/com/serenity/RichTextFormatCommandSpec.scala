package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandIntent, CommandRegistry}
import com.serenity.keystroke.events.InsertChar
import com.serenity.richtext.{InlineMark, ParagraphAlignment, ParagraphRole}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{BufferId, CursorPosition, Selection}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

extension (buffer: com.serenity.state.models.Buffer)

  private def withEditing(
    f: com.serenity.state.models.EditingState => com.serenity.state.models.EditingState
  ): com.serenity.state.models.Buffer =
    buffer.copy(editing = f(buffer.editing))

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
            state.buffers(bufferId).withEditing(_.copy(selection = Some(selection), cursors = List(selection.focus)))
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
    registry.findCommand("heading-1").map(_.intent) shouldBe Some(
      CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(1))
    )
    registry.findCommand("paragraph-body").map(_.intent) shouldBe Some(
      CommandIntent.SetRichTextParagraphRole(ParagraphRole.Body)
    )
    registry.findCommand("align-center").map(_.intent) shouldBe Some(
      CommandIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Center)
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
    buffer.document.isDirty shouldBe true
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks) shouldBe Some(Set(InlineMark.Bold))
  }

  it should "use plain formatting for text that replaces a newly formatted selection" in {
    val (stateManager, bufferId) =
      selectedStateManager(
        "alpha beta",
        Selection(CursorPosition(0, 6), CursorPosition(0, 10))
      )
    val command = CommandRegistry.withToggleUI.findCommand("bold").getOrElse(fail("missing bold"))

    stateManager.executeCommand(command).unsafeRunSync()
    stateManager.applyEvent(InsertChar('X')).unsafeRunSync()

    val buffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.document.content.collect() shouldBe "alpha X"
    buffer.richText.richTextDocument.flatMap(_.paragraphs.headOption).map(_.runs) shouldBe Some(
      List(com.serenity.richtext.RichTextRun("alpha X"))
    )
  }

  it should "apply a toggled mark at the cursor to subsequently entered text" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer("alpha").unsafeRunSync()
    stateManager.setBufferForPane(com.serenity.state.models.PaneId(0), bufferId).unsafeRunSync()
    stateManager
      .updateState { state =>
        state.copy(
          buffers = state.buffers.updated(
            bufferId,
            state.buffers(bufferId).withEditing(_.copy(cursors = List(CursorPosition(0, 5))))
          )
        )
      }
      .unsafeRunSync()
    val command = CommandRegistry.withToggleUI.findCommand("italic").getOrElse(fail("missing italic"))

    stateManager.executeCommand(command).unsafeRunSync()
    stateManager.applyEvent(InsertChar('X')).unsafeRunSync()

    val buffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.document.content.collect() shouldBe "alphaX"
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "X"))
      .map(_.style.marks) shouldBe Some(Set(InlineMark.Italic))
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
    buffer.richText.richTextDocument.map(_.plainText) shouldBe Some("alpha beta")
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .map(_.runs) shouldBe Some(List(com.serenity.richtext.RichTextRun("alpha beta")))
  }

  it should "apply heading roles to the active cursor paragraph" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer("Chapter One\nBody").unsafeRunSync()
    stateManager.setBufferForPane(com.serenity.state.models.PaneId(0), bufferId).unsafeRunSync()
    stateManager
      .updateState { state =>
        state.copy(
          buffers = state.buffers.updated(
            bufferId,
            state.buffers(bufferId).withEditing(_.copy(cursors = List(com.serenity.state.models.CursorPosition(0, 3))))
          )
        )
      }
      .unsafeRunSync()
    val command = CommandRegistry.withToggleUI.findCommand("heading-1").getOrElse(fail("missing heading-1"))

    stateManager.executeCommand(command).unsafeRunSync()

    val buffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.document.isDirty shouldBe true
    buffer.richText.richTextDocument.map(_.paragraphs.map(_.role)) shouldBe Some(
      List(ParagraphRole.Heading(1), ParagraphRole.Body)
    )
  }

  it should "apply paragraph alignment across the active selection" in {
    val (stateManager, bufferId) =
      selectedStateManager(
        "Lead\nCentered\nTail",
        Selection(com.serenity.state.models.CursorPosition(1, 0), com.serenity.state.models.CursorPosition(2, 2))
      )
    val command = CommandRegistry.withToggleUI.findCommand("align-center").getOrElse(fail("missing align-center"))

    stateManager.executeCommand(command).unsafeRunSync()

    val buffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.document.isDirty shouldBe true
    buffer.richText.richTextDocument.map(_.paragraphs.map(_.alignment)) shouldBe Some(
      List(ParagraphAlignment.Left, ParagraphAlignment.Center, ParagraphAlignment.Center)
    )
  }

  it should "apply rich text font family, size, and colour to the active selection" in {
    val (stateManager, bufferId) =
      selectedStateManager(
        "alpha beta",
        Selection(com.serenity.state.models.CursorPosition(0, 6), com.serenity.state.models.CursorPosition(0, 10))
      )

    stateManager
      .executeCommand(
        com.serenity.command.Command.typed(
          "rich-text-font-family",
          "Set selection font family.",
          CommandIntent.SetRichTextFontFamily("Serif")
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        com.serenity.command.Command.typed(
          "rich-text-font-size",
          "Set selection font size.",
          CommandIntent.SetRichTextFontSize(18.0f)
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        com.serenity.command.Command.typed(
          "rich-text-color",
          "Set selection colour.",
          CommandIntent.SetRichTextColor("#336699")
        )
      )
      .unsafeRunSync()

    val betaStyle = stateManager.getCurrentState
      .unsafeRunSync()
      .buffers(bufferId)
      .richText
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style)

    betaStyle.flatMap(_.fontFamily) shouldBe Some("Serif")
    betaStyle.flatMap(_.fontSize) shouldBe Some(18.0f)
    betaStyle.flatMap(_.color) shouldBe Some("#336699")
  }

  it should "use plain formatting when editing after formatting a selection" in {
    val (stateManager, bufferId) =
      selectedStateManager(
        "alpha beta",
        Selection(CursorPosition(0, 6), CursorPosition(0, 10))
      )
    val italicCommand = CommandRegistry.withToggleUI.findCommand("italic").getOrElse(fail("missing italic"))

    stateManager.executeCommand(italicCommand).unsafeRunSync()
    stateManager
      .updateState { state =>
        state.copy(
          buffers = state.buffers.updated(
            bufferId,
            state.buffers(bufferId).withEditing(_.copy(selection = None, cursors = List(CursorPosition(0, 8))))
          )
        )
      }
      .unsafeRunSync()
    stateManager.applyEvent(InsertChar('X')).unsafeRunSync()

    val buffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.document.content.collect() shouldBe "alpha beXta"
    buffer.richText.richTextDocument.map(_.plainText) shouldBe Some("alpha beXta")
    buffer.richText.richTextDocument.flatMap(_.paragraphs.headOption).map(_.runs) shouldBe Some(
      List(
        com.serenity.richtext.RichTextRun("alpha "),
        com.serenity.richtext.RichTextRun("be", com.serenity.richtext.RichTextStyle(Set(InlineMark.Italic))),
        com.serenity.richtext.RichTextRun("X"),
        com.serenity.richtext.RichTextRun("ta", com.serenity.richtext.RichTextStyle(Set(InlineMark.Italic)))
      )
    )
  }

  it should "make formatted rich text headings available to document navigation" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer("Chapter One\nBody").unsafeRunSync()
    stateManager.setBufferForPane(com.serenity.state.models.PaneId(0), bufferId).unsafeRunSync()
    stateManager
      .updateState { state =>
        state.copy(
          buffers = state.buffers.updated(
            bufferId,
            state.buffers(bufferId).withEditing(_.copy(cursors = List(com.serenity.state.models.CursorPosition(0, 0))))
          )
        )
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(CommandRegistry.withToggleUI.findCommand("heading-1").getOrElse(fail("missing heading-1")))
      .unsafeRunSync()
    stateManager
      .executeCommand(CommandRegistry.withToggleUI.findCommand("pin-outline").getOrElse(fail("missing pin-outline")))
      .unsafeRunSync()

    val outlineSymbols = stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.collectFirst {
      case com.serenity.state.models.UiSurface(
            _,
            com.serenity.state.models.SurfaceContent.Outline(symbols, _),
            _,
            _
          ) =>
        symbols
    }

    outlineSymbols.map(_.map(_.name)) shouldBe Some(List("Chapter One"))
  }
