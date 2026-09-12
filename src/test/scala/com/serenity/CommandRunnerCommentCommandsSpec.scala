package com.serenity

import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerCommentCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    configPersistencePath: Option[Path] = None,
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerCommentCommandsSpec"))
    StateManager
      .apply(
        logger,
        sessionRootOverride = sessionRootOverride,
        configPersistencePath = configPersistencePath,
        fileDialog = fileDialog
      )
      .unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    val beforeOpen = stateManager.getCurrentState.unsafeRunSync()
    if beforeOpen.commandRunnerSurface
          .flatMap {
            _.content match
              case SurfaceContent.CommandPalette(runner) => Some(runner.isActive)
              case _                                     => None
          }
          .getOrElse(false) == false
    then stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()


  "Command runner" should "toggle a cursor-attached comment lens for the active comment" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("val x = 1\n// **Review** this value"),
                language = Some(LanguageId.Scala)
              ),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 3)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "comment-lens", "comment-lens")

    val shownState = stateManager.getCurrentState.unsafeRunSync()
    val lens = shownState.commentLensSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommentLens(lens) => Some(lens)
          case _                                => None
      }
      .getOrElse(fail("Expected comment lens"))

    lens.comment.raw shouldBe "// **Review** this value"
    lens.comment.inlineMarkdown shouldBe "Review this value"
    lens.draft shouldBe "// **Review** this value"
    lens.target shouldBe None
    shownState.persisted.focus shouldBe Focus.Surface(shownState.commentLensSurface.get.id)
    shownState.commentLensSurface.get.dismissOnMove shouldBe false

    stateManager
      .executeCommand(
        Command.typed(
          "comment-lens",
          "Toggle comment lens.",
          CommandIntent.Comments(CommentsIntent.ToggleCommentLens),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    val hiddenState = stateManager.getCurrentState.unsafeRunSync()
    hiddenState.commentLensSurface shouldBe None
    hiddenState.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    hiddenState.runtime.focusHistory shouldBe Nil
  }

  it should "add, navigate, render, and delete authored document comments" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope("Opening paragraph\nSecond paragraph")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                cursors = List(CursorPosition(0, 2)),
                selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 7)))
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "add-document-comment", "add-document-comment")

    val commentedBuffer = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    commentedBuffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 0), CursorPosition(0, 7), "Comment")
    )
    commentedBuffer.document.isDirty shouldBe true

    executeCommandThroughRunner(stateManager, "comment-lens", "comment-lens")

    val lens = stateManager.getCurrentState
      .unsafeRunSync()
      .commentLensSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommentLens(lens) => Some(lens)
          case _                                => None
      }
      .getOrElse(fail("Expected comment lens"))
    lens.draft shouldBe "Comment"
    lens.target shouldBe Some(DocumentComment(CursorPosition(0, 0), CursorPosition(0, 7), "Comment"))

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            editing =
              state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 0)), selection = None)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-comment", "next-document-comment")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors shouldBe List(
      CursorPosition(0, 0)
    )

    executeCommandThroughRunner(stateManager, "delete-document-comment", "delete-document-comment")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).annotations.documentComments shouldBe Nil
  }

  it should "open the comment lens when navigating between document comments with the keyboard" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope("Opening paragraph\nSecond paragraph")),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 0))),
            annotations = state.persisted
              .buffers(bufferId)
              .annotations
              .copy(
                documentComments = List(
                  DocumentComment(CursorPosition(0, 0), CursorPosition(0, 7), "Revise opening"),
                  DocumentComment(CursorPosition(1, 0), CursorPosition(1, 6), "Tighten this")
                )
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "previous-document-comment", "previous-document-comment")

    val afterPrevious = stateManager.getCurrentState.unsafeRunSync()
    afterPrevious.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 0))
    val previousLens = afterPrevious.commentLensSurface
      .collect { case UiSurface(_, SurfaceContent.CommentLens(lens), _, _) => lens }
      .getOrElse(fail("Expected the comment lens to open after previous-document-comment"))
    previousLens.draft shouldBe "Revise opening"
    previousLens.target shouldBe Some(DocumentComment(CursorPosition(0, 0), CursorPosition(0, 7), "Revise opening"))

    executeCommandThroughRunner(stateManager, "next-document-comment", "next-document-comment")

    val afterNext = stateManager.getCurrentState.unsafeRunSync()
    afterNext.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(1, 0))
    val nextLens = afterNext.commentLensSurface
      .collect { case UiSurface(_, SurfaceContent.CommentLens(lens), _, _) => lens }
      .getOrElse(fail("Expected the comment lens to open after next-document-comment"))
    nextLens.draft shouldBe "Tighten this"
    nextLens.target shouldBe Some(DocumentComment(CursorPosition(1, 0), CursorPosition(1, 6), "Tighten this"))
  }

  it should "add custom authored document comments and update existing comments at the cursor" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document =
              state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("Opening paragraph")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                cursors = List(CursorPosition(0, 3)),
                selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 7)))
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "document-comment",
          "Add document comment.",
          CommandIntent.Comments(CommentsIntent.AddDocumentComment("Tighten this opening")),
          CommandCategory.Edit
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 0), CursorPosition(0, 7), "Tighten this opening")
    )

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            editing =
              state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 3)), selection = None)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "document-comment",
          "Update document comment.",
          CommandIntent.Comments(CommentsIntent.AddDocumentComment("Make this quieter")),
          CommandCategory.Edit
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 0), CursorPosition(0, 7), "Make this quieter")
    )
  }

  it should "snap authored document comment ranges to grapheme boundaries" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("cafe\u0301!")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                cursors = List(CursorPosition(0, 5)),
                selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 5)))
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "document-comment",
          "Add document comment.",
          CommandIntent.Comments(CommentsIntent.AddDocumentComment("Accent")),
          CommandCategory.Edit
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 3), CursorPosition(0, 5), "Accent")
    )

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document =
              state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("a\uD83D\uDE42b")),
            editing =
              state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 2)), selection = None),
            annotations = state.persisted.buffers(bufferId).annotations.copy(documentComments = Nil)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "document-comment",
          "Add document comment.",
          CommandIntent.Comments(CommentsIntent.AddDocumentComment("Point")),
          CommandCategory.Edit
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .buffers(bufferId)
      .annotations
      .documentComments should contain(
      DocumentComment(CursorPosition(0, 3), CursorPosition(0, 3), "Point")
    )

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope("a\uD83C\uDDFA\uD83C\uDDF8b")),
            editing =
              state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 3)), selection = None),
            annotations = state.persisted.buffers(bufferId).annotations.copy(documentComments = Nil)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "document-comment",
          "Add document comment.",
          CommandIntent.Comments(CommentsIntent.AddDocumentComment("Flag point")),
          CommandCategory.Edit
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .buffers(bufferId)
      .annotations
      .documentComments should contain(
      DocumentComment(CursorPosition(0, 5), CursorPosition(0, 5), "Flag point")
    )
  }
