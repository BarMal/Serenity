package com.serenity

import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationState, CharacterKey}
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

class CommandRunnerNavigationCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    configPersistencePath: Option[Path] = None,
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerNavigationCommandsSpec"))
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

  "Command runner" should "navigate between tabs through typed commands" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "new", "new")
    executeCommandThroughRunner(stateManager, "previous-tab", "previous-tab")

    stateManager.getCurrentState.unsafeRunSync().focusedBufferId shouldBe Some(BufferId(0))

    executeCommandThroughRunner(stateManager, "next-tab", "next-tab")

    stateManager.getCurrentState.unsafeRunSync().focusedBufferId shouldBe Some(BufferId(1))
  }

  it should "navigate to the next Markdown heading from the command runner" in {
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
                content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two\n\nText\n\n### Beat Three"),
                language = Some(LanguageId.Markdown)
              ),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 2)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    val updatedBuffer = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    updatedBuffer.editing.cursors shouldBe List(CursorPosition(4, 0))
    updatedBuffer.editing.selection shouldBe None
    updatedBuffer.editing.selections shouldBe Nil
  }

  it should "animate the target buffer after document symbol navigation" in {
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
                content = com.serenity.rope.Rope(
                  "# Chapter One\n\nBody\n\nMore\n\nStill more\n\nEven more\n\n## Scene Two\n\nText"
                ),
                language = Some(LanguageId.Markdown)
              ),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 2))),
            viewport = Viewport.default.copy(visibleLines = 4, visibleColumns = 40)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    val updatedBuffer = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    updatedBuffer.editing.cursors shouldBe List(CursorPosition(10, 0))
    updatedBuffer.viewport.topLine should be > 0
    val animations = stateManager.getBufferAnimations.unsafeRunSync().getOrElse(bufferId, AnimationState.empty)
    animations.activeAnimationCount should be > 0
  }

  it should "navigate to the previous Markdown heading from a command" in {
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
                content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two"),
                language = Some(LanguageId.Markdown)
              ),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "previous-document-symbol",
          "Go to the previous document symbol.",
          CommandIntent.Navigation(NavigationIntent.PreviousDocumentSymbol),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors shouldBe List(
      CursorPosition(4, 0)
    )
  }

  it should "navigate between plaintext sections from the command runner" in {
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
                content = com.serenity.rope.Rope("Opening\nbody\n\nSecond\nbody\n\nThird"),
                language = None
              ),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 0)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors shouldBe List(
      CursorPosition(3, 0)
    )
  }

  it should "toggle a bookmark at the active cursor from the command runner" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer =
          state.persisted
            .buffers(bufferId)
            .copy(
              document = state.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("alpha\nbravo\ncharlie")),
              editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(2, 4)))
            )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "toggle-bookmark", "toggle-bookmark")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).annotations.bookmarks shouldBe List(
      CursorPosition(2, 4)
    )

    executeCommandThroughRunner(stateManager, "toggle-bookmark", "toggle-bookmark")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).annotations.bookmarks shouldBe Nil
  }

  it should "navigate between explicit bookmarks from command runner commands" in {
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
              .copy(content = com.serenity.rope.Rope("alpha\nbravo\ncharlie\ndelta\necho")),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(2, 0))),
            annotations = state.persisted
              .buffers(bufferId)
              .annotations
              .copy(
                bookmarks = List(CursorPosition(0, 3), CursorPosition(4, 1))
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-bookmark", "next-bookmark")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors shouldBe List(
      CursorPosition(4, 1)
    )

    executeCommandThroughRunner(stateManager, "previous-bookmark", "previous-bookmark")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors shouldBe List(
      CursorPosition(0, 3)
    )
  }

  it should "animate the target buffer after bookmark navigation" in {
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
              .copy(content = com.serenity.rope.Rope("alpha\nbravo\ncharlie\ndelta\necho")),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 0))),
            annotations = state.persisted.buffers(bufferId).annotations.copy(bookmarks = List(CursorPosition(4, 1))),
            viewport = Viewport.default.copy(visibleLines = 8, visibleColumns = 40)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-bookmark", "next-bookmark")

    val updatedBuffer = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    updatedBuffer.editing.cursors shouldBe List(CursorPosition(4, 1))
    val animations = stateManager.getBufferAnimations.unsafeRunSync().getOrElse(bufferId, AnimationState.empty)
    animations.activeAnimationCount should be > 0
  }

  it should "animate the visible unwrapped slice after bookmark navigation" in {
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
              .copy(content = com.serenity.rope.Rope("0123456789abcdefghijklmnopqrstuvwxyz")),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
            annotations = state.persisted.buffers(bufferId).annotations.copy(bookmarks = List(CursorPosition(0, 12))),
            viewport = Viewport.default.copy(visibleLines = 1, visibleColumns = 5)
          )
        state.copy(persisted =
          state.persisted.copy(
            config = state.persisted.config.withWordWrap(false),
            buffers = state.persisted.buffers + (bufferId -> buffer)
          )
        )
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-bookmark", "next-bookmark")

    val updatedBuffer = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    val animations    = stateManager.getBufferAnimations.unsafeRunSync().getOrElse(bufferId, AnimationState.empty)
    updatedBuffer.editing.cursors.shouldBe(List(CursorPosition(0, 12)))
    updatedBuffer.viewport.leftColumn.should(be > 0)
    animations.animations.should(contain.key(CharacterKey(updatedBuffer.viewport.leftColumn, 0)))
    animations.animations.shouldNot(contain.key(CharacterKey(0, 0)))
  }

  it should "record document jumps in navigation history and move backward and forward" in {
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
                content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two"),
                language = Some(LanguageId.Markdown)
              ),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 2)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    val afterJump = stateManager.getCurrentState.unsafeRunSync()
    afterJump.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(4, 0))
    afterJump.runtime.navigation.backStack shouldBe List(NavigationPoint(PaneId(0), bufferId, CursorPosition(1, 2)))
    afterJump.runtime.navigation.forwardStack shouldBe Nil

    executeCommandThroughRunner(stateManager, "navigate-back", "navigate-back")

    val afterBack = stateManager.getCurrentState.unsafeRunSync()
    afterBack.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(1, 2))
    afterBack.runtime.navigation.backStack shouldBe Nil
    afterBack.runtime.navigation.forwardStack shouldBe List(NavigationPoint(PaneId(0), bufferId, CursorPosition(4, 0)))

    executeCommandThroughRunner(stateManager, "navigate-forward", "navigate-forward")

    val afterForward = stateManager.getCurrentState.unsafeRunSync()
    afterForward.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(4, 0))
    afterForward.runtime.navigation.backStack shouldBe List(NavigationPoint(PaneId(0), bufferId, CursorPosition(1, 2)))
    afterForward.runtime.navigation.forwardStack shouldBe Nil
  }

  it should "leave the cursor unchanged when document symbol navigation has no target" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document =
              state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("# Plain text only")),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 7)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors shouldBe List(
      CursorPosition(0, 7)
    )
  }
