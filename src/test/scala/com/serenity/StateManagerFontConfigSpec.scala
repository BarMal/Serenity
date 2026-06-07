package com.serenity

import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.ui.fonts.FontLoader.FontConfig

class StateManagerFontConfigSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    openRunner(stateManager)
    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case com.serenity.state.models.SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                                               => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()

  private def openRunner(stateManager: StateManager): Unit =
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

  private def openTypographySubmenu(stateManager: StateManager): Unit =
    openRunner(stateManager)
    for _ <- 1 to 4 do stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

  "StateManager" should "invoke the runtime font callback when changing buffer font size from typography settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openTypographySubmenu(stateManager)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    List('1', '3').foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.fontSize shouldBe 13.0f
  }

  it should "invoke the runtime font callback when changing UI font size from typography settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openTypographySubmenu(stateManager)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    List('1', '5').foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.uiFontSize shouldBe 15.0f
  }

  it should "invoke the runtime font callback when changing ligature shaping from typography settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openTypographySubmenu(stateManager)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveRight).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.enableLigatures shouldBe false
  }
