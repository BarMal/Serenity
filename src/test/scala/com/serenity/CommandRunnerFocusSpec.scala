package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandCategory, CommandRunner}
import com.serenity.config.{AppConfig, MotionPreset}
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

  /** Read-only conveniences mirroring the old flat `CommandRunnerSubmenuState`'s accessors, on top of the page-stack
    * `activeSettingsSurface` that replaced it (issue #1059).
    */
  extension (runner: CommandRunner)
    private def activeSubmenuGroupId: Option[String] = runner.activeSettingsSurface.map(_.current.groupId)
    private def activeSubmenuEditingItemId: Option[String] =
      runner.activeSettingsSurface.flatMap(_.current.editingItemId)
    private def activeSubmenuParentGroupId: Option[String] =
      runner.activeSettingsSurface.flatMap(_.ancestors.headOption.map(_.groupId))

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

  private def moveSubmenuSelectionTo(stateManager: StateManager, itemId: String): Unit =
    val runner = currentRunner(stateManager)
    val items  = runner.focusedSubmenuItems
    val target = items.indexWhere(_.id == itemId)
    if target < 0 then fail(s"Expected submenu item $itemId")
    val moves = (target - runner.settingsSurfaceSelectedIndex + items.length) % items.length
    (1 to moves).foreach(_ => stateManager.applyEvent(MoveDown).unsafeRunSync())

  private def moveRootSelectionTo(stateManager: StateManager, itemId: String): Unit =
    val runner = currentRunner(stateManager)
    val items  = runner.visibleItems
    val target = items.indexWhere(_.id == itemId)
    if target < 0 then fail(s"Expected root item $itemId")
    val moves = (target - runner.selectedIndex + items.length) % items.length
    (1 to moves).foreach(_ => stateManager.applyEvent(MoveDown).unsafeRunSync())

  private def moveToCategory(stateManager: StateManager, category: CommandCategory): Unit =
    (1 to CommandCategory.values.length).foreach { _ =>
      if currentRunner(stateManager).activeCategory != category then stateManager.applyEvent(TabKey).unsafeRunSync()
    }
    currentRunner(stateManager).activeCategory shouldBe category

  // issue #1059: a drilled-in settings group renders on the one command-runner surface now, so there is no second
  // surface for focus to move to -- it stays on "command-runner" throughout.
  "Command runner focus ownership" should "keep submenu navigation on the one command-runner surface" in {
    val stateManager = createStateManager()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    moveToCategory(stateManager, CommandCategory.Settings)
    moveRootSelectionTo(stateManager, "settings-appearance-motion")
    stateManager.applyEvent(Enter).unsafeRunSync()

    val enteredState  = stateManager.getCurrentState.unsafeRunSync()
    val mainSurfaceId = enteredState.commandRunnerSurface.getOrElse(fail("Expected command runner surface")).id
    enteredState.runtime.uiSurfaces should have size 1
    enteredState.persisted.focus shouldBe Focus.Surface(mainSurfaceId)

    stateManager.applyEvent(MoveDown).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe defined
    updatedState.runtime.uiSurfaces should have size 1
    updatedState.persisted.focus shouldBe Focus.Surface(mainSurfaceId)
    currentRunner(stateManager).settingsSurfaceSelectedIndex shouldBe 1
  }

  it should "unwind escape from submenu edit mode to submenu, then parent, then closed" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state.copy(persisted = state.persisted.copy(config = AppConfig.default.withMotionPreset(MotionPreset.Custom)))
      )
      .unsafeRunSync()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    (1 to 5).foreach(_ => stateManager.applyEvent(TabKey).unsafeRunSync())
    (1 to 4).foreach(_ => stateManager.applyEvent(MoveDown).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    moveSubmenuSelectionTo(stateManager, "settings-animation")
    stateManager.applyEvent(Enter).unsafeRunSync()
    moveSubmenuSelectionTo(stateManager, "animation-duration")

    currentRunner(stateManager).activeSubmenuEditingItemId shouldBe None

    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(InsertChar('9')).unsafeRunSync()
    currentRunner(stateManager).activeSubmenuEditingItemId shouldBe Some("animation-duration")

    // Escape #1: cancels the in-progress edit, staying on the same (child) submenu page -- and, since issue #1059,
    // on the one command-runner surface (no second surface to move focus to anymore).
    stateManager.applyEvent(Escape).unsafeRunSync()
    val afterFirstEscape = stateManager.getCurrentState.unsafeRunSync()
    val mainSurfaceId    = afterFirstEscape.commandRunnerSurface.getOrElse(fail("Expected command runner surface")).id
    afterFirstEscape.commandRunnerSurface shouldBe defined
    afterFirstEscape.runtime.uiSurfaces should have size 1
    afterFirstEscape.persisted.focus shouldBe Focus.Surface(mainSurfaceId)
    currentRunner(stateManager).activeSubmenuEditingItemId shouldBe None
    currentRunner(stateManager).activeSubmenuGroupId shouldBe Some("settings-animation")

    // Escape #2: pops from the child submenu ("settings-animation") to its parent -- still on the one surface.
    stateManager.applyEvent(Escape).unsafeRunSync()
    val afterSecondEscape = stateManager.getCurrentState.unsafeRunSync()
    afterSecondEscape.commandRunnerSurface shouldBe defined
    afterSecondEscape.runtime.uiSurfaces should have size 1
    afterSecondEscape.persisted.focus shouldBe Focus.Surface(mainSurfaceId)
    currentRunner(stateManager).activeSubmenuParentGroupId shouldBe None

    // Escape #3: pops the parent (a top-level group, with no ancestor of its own) -- issue #1059's fix: this now
    // correctly clears the settings-surface stack in a single step. (The pre-fix `exitSubmenuToPreview` computed a
    // self-referential `parentGroupId` for a revealed top-level page, which used to take one extra, spurious Escape
    // to actually leave -- this test previously encoded that bug as 5 total escapes to fully dismiss; it's 4 now.)
    // Focus returns to the main command-runner surface.
    stateManager.applyEvent(Escape).unsafeRunSync()
    val afterThirdEscape = stateManager.getCurrentState.unsafeRunSync()
    afterThirdEscape.commandRunnerSurface shouldBe defined
    afterThirdEscape.persisted.focus shouldBe Focus.Surface(afterThirdEscape.commandRunnerSurface.get.id)
    currentRunner(stateManager).activeSettingsSurface shouldBe None

    // Escape #4: nothing left to pop, dismisses the whole command runner.
    stateManager.applyEvent(Escape).unsafeRunSync()
    val afterFourthEscape = stateManager.getCurrentState.unsafeRunSync()
    afterFourthEscape.commandRunnerSurface shouldBe None
    afterFourthEscape.persisted.focus should not be Focus.Surface(SurfaceId("command-runner"))
    afterFourthEscape.persisted.focus should not be Focus.Surface(SurfaceId("command-runner-submenu"))
  }

  it should "close the runner even if focus has leaked back to the editor while the runner remains visible" in {
    val stateManager = createStateManager()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    stateManager
      .updateState(state => state.copy(persisted = state.persisted.copy(focus = Focus.EditorPane(PaneId(0)))))
      .unsafeRunSync()

    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.commandRunnerSurface shouldBe None
  }
