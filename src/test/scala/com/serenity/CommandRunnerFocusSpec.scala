package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerFocusSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerFocusSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def currentRunner(stateManager: StateManager) =
    stateManager.getCurrentState
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner"))

  "Command runner focus ownership" should "keep submenu navigation inside the command-runner domain" in {
    val stateManager = createStateManager()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    (1 to 5).foreach(_ => stateManager.applyEvent(TabKey).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))

    stateManager.applyEvent(MoveDown).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe defined
    updatedState.commandRunnerSubmenuSurface shouldBe defined
    updatedState.focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))
    currentRunner(stateManager).activeSubmenu.map(_.selectedIndex) shouldBe Some(1)
  }

  it should "unwind escape from submenu edit mode to submenu, then parent, then closed" in {
    val stateManager = createStateManager()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    (1 to 5).foreach(_ => stateManager.applyEvent(TabKey).unsafeRunSync())
    "animation".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()

    currentRunner(stateManager).activeSubmenu.flatMap(_.editingItemId) shouldBe None

    stateManager.applyEvent(Escape).unsafeRunSync()
    val afterFirstEscape = stateManager.getCurrentState.unsafeRunSync()
    afterFirstEscape.commandRunnerSurface shouldBe defined
    afterFirstEscape.commandRunnerSubmenuSurface shouldBe defined
    afterFirstEscape.focus shouldBe Focus.Surface(afterFirstEscape.commandRunnerSurface.get.id)

    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(InsertChar('9')).unsafeRunSync()
    currentRunner(stateManager).activeSubmenu.flatMap(_.editingItemId) shouldBe Some("animation-duration")

    stateManager.applyEvent(Escape).unsafeRunSync()
    val afterSecondEscape = stateManager.getCurrentState.unsafeRunSync()
    afterSecondEscape.commandRunnerSurface shouldBe defined
    afterSecondEscape.commandRunnerSubmenuSurface shouldBe defined
    afterSecondEscape.focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))
    currentRunner(stateManager).activeSubmenu.flatMap(_.editingItemId) shouldBe None

    stateManager.applyEvent(Escape).unsafeRunSync()
    val afterThirdEscape = stateManager.getCurrentState.unsafeRunSync()
    afterThirdEscape.commandRunnerSurface shouldBe defined
    afterThirdEscape.commandRunnerSubmenuSurface shouldBe defined
    afterThirdEscape.focus shouldBe Focus.Surface(afterThirdEscape.commandRunnerSurface.get.id)

    stateManager.applyEvent(Escape).unsafeRunSync()
    val afterFourthEscape = stateManager.getCurrentState.unsafeRunSync()
    afterFourthEscape.commandRunnerSurface shouldBe None
    afterFourthEscape.commandRunnerSubmenuSurface shouldBe None
    afterFourthEscape.focus should not be Focus.Surface(SurfaceId("command-runner"))
    afterFourthEscape.focus should not be Focus.Surface(SurfaceId("command-runner-submenu"))
  }

  it should "close the runner even if focus has leaked back to the editor while the runner remains visible" in {
    val stateManager = createStateManager()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    stateManager.updateState(_.copy(focus = Focus.EditorPane(PaneId(0)))).unsafeRunSync()

    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.commandRunnerSurface shouldBe None
    state.commandRunnerSubmenuSurface shouldBe None
  }
