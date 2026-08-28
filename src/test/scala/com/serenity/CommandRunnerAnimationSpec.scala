package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.command.{Command, CommandCategory, CommandIntent, MotionIntent, SettingsIntent}
import com.serenity.config.{AppConfig, MotionAccessibility, MotionPreset}
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerAnimationSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerAnimationSpec"))
    StateManager.apply(logger).unsafeRunSync()

  "Command runner open animation" should "start in Visible phase immediately (no buffer fade)" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.commandRunnerSurface shouldBe defined
    val surfaceId = state.commandRunnerSurface.get.id
    val animState = state.runtime.surfaceAnimations.get(surfaceId)
    animState shouldBe defined
    animState.get.phase shouldBe SurfacePhase.Visible
  }

  it should "set bufferFadeLength to zero (buffer is not animated)" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    val anim      = state.runtime.surfaceAnimations(surfaceId)
    anim.bufferFadeLength shouldBe 0
  }

  it should "have active overlay animations but no buffer animations after open" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state            = sm.getCurrentState.unsafeRunSync()
    val surfaceId        = state.commandRunnerSurface.get.id
    val bufferAnimations = sm.getBufferAnimations.unsafeRunSync()
    bufferAnimations.values.exists(_.hasActiveAnimations) shouldBe false
    state.runtime.surfaceAnimations(surfaceId).animationState.hasActiveAnimations shouldBe true
  }

  it should "skip command runner fade when the command runner animation is disabled" in {
    val sm = createStateManager()
    sm.updateState(state =>
      state.copy(persisted = state.persisted.copy(config = AppConfig.default.withCommandRunnerAnimation(None)))
    ).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    state.runtime.surfaceAnimations.get(surfaceId) shouldBe None
  }

  it should "skip command runner fade when the global animation speed is zero" in {
    val sm = createStateManager()
    sm.updateState { state =>
      state.copy(persisted =
        state.persisted.copy(config =
          AppConfig.default
            .withMotionPreset(MotionPreset.Smooth)
            .withElementTransitionSpeedScale(0.0)
        )
      )
    }.unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    state.runtime.surfaceAnimations.get(surfaceId) shouldBe None
  }

  it should "cancel all active animation state when a motion policy disables animation" in
    List(
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Reduced))),
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Off))),
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Reduced))),
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(0.0)))
    ).foreach { intent =>
      val sm = createStateManager()
      sm.updateState(state => state.copy(persisted = state.persisted.copy(config = AppConfig.withTestAnimations)))
        .unsafeRunSync()
      sm.applyEvent(InsertChar('a')).unsafeRunSync()
      sm.updateState(state =>
        state.copy(runtime = state.runtime.copy(themeTransition = Some(ThemeTransition(state.persisted.theme, 0, 2))))
      ).unsafeRunSync()
      sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
      advanceToVisible(sm)
      sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

      val activeState            = sm.getCurrentState.unsafeRunSync()
      val activeBufferAnimations = sm.getBufferAnimations.unsafeRunSync()
      activeBufferAnimations.values.exists(_.hasActiveAnimations) shouldBe true
      activeState.runtime.themeTransition shouldBe defined
      activeState.runtime.surfaceAnimations should not be empty
      activeState.runtime.uiSurfaces
        .exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe true

      sm.executeCommand(
        Command.typed(
          "disable-motion",
          "Disable motion",
          intent,
          CommandCategory.Settings
        )
      ).unsafeRunSync()

      val state = sm.getCurrentState.unsafeRunSync()
      sm.getBufferAnimations.unsafeRunSync().values.foreach(_.animations shouldBe Map.empty)
      state.runtime.themeTransition shouldBe None
      state.runtime.surfaceAnimations shouldBe Map.empty
      state.runtime.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe false
      sm.advanceAnimationsOnTick().unsafeRunSync() shouldBe false
    }

  it should "cancel only editor animations when the editor text family is disabled" in {
    val sm = createStateManager()
    sm.updateState(state => state.copy(persisted = state.persisted.copy(config = AppConfig.withTestAnimations)))
      .unsafeRunSync()
    sm.applyEvent(InsertChar('a')).unsafeRunSync()
    sm.updateState(state =>
      state.copy(runtime = state.runtime.copy(themeTransition = Some(ThemeTransition(state.persisted.theme, 0, 2))))
    ).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "editor-text-speed-scale",
        "Set editor text speed scale",
        CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetEditorTextTransitionSpeedScale(0.0))),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    sm.getBufferAnimations.unsafeRunSync().values.foreach(_.animations shouldBe Map.empty)
    state.runtime.themeTransition shouldBe defined
    state.runtime.surfaceAnimations should not be empty
    state.runtime.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe true
  }

  it should "scale command runner fade length with the global animation speed" in {
    val sm = createStateManager()
    sm.updateState { state =>
      state.copy(persisted =
        state.persisted.copy(config =
          AppConfig.default
            .withMotionPreset(MotionPreset.Smooth)
            .withElementTransitionSpeedScale(2.0)
        )
      )
    }.unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    val firstCell = state.runtime.surfaceAnimations(surfaceId).animationState.getCell(0, 0).get
    firstCell.backgroundSteps.length shouldBe AnimationConfig.Enabled.smooth.steps * 2
  }

  it should "use the command runner reveal kind for open choreography" in {
    val sm = createStateManager()
    sm.updateState { state =>
      state.copy(persisted =
        state.persisted.copy(config =
          AppConfig.default
            .withCommandRunnerTransitionKind(Some(TransitionKind.DirectionalSweep))
        )
      )
    }.unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state       = sm.getCurrentState.unsafeRunSync()
    val surfaceId   = state.commandRunnerSurface.get.id
    val animatedCol = state.runtime.surfaceAnimations(surfaceId).animationState.animations.keys.map(_.column).max
    animatedCol should be > 0
  }

  it should "transition to Visible after bufferFadeLength ticks" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state0    = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state0.commandRunnerSurface.get.id
    val fadeLen   = state0.runtime.surfaceAnimations(surfaceId).bufferFadeLength

    (1 to fadeLen).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

    val state1 = sm.getCurrentState.unsafeRunSync()
    state1.runtime.surfaceAnimations.get(surfaceId).map(_.phase) shouldBe Some(SurfacePhase.Visible)
  }

  it should "have overlay fade-in animation after transitioning to Visible" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state0  = sm.getCurrentState.unsafeRunSync()
    val surfId  = state0.commandRunnerSurface.get.id
    val fadeLen = state0.runtime.surfaceAnimations(surfId).bufferFadeLength

    (1 to fadeLen).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

    val state1 = sm.getCurrentState.unsafeRunSync()
    val anim   = state1.runtime.surfaceAnimations(surfId)
    anim.phase shouldBe SurfacePhase.Visible
    anim.animationState.hasActiveAnimations shouldBe true
  }

  "Command runner close animation" should "add a ghost overlay surface when closed" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.commandRunnerSurface shouldBe None
    val ghost = state.runtime.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay])
    ghost shouldBe defined
  }

  it should "mark the ghost surface as Exiting in surfaceAnimations" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    val ghost     = state.runtime.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay]).get
    val ghostAnim = state.runtime.surfaceAnimations.get(ghost.id)
    ghostAnim shouldBe defined
    ghostAnim.get.phase shouldBe SurfacePhase.Exiting
    ghostAnim.get.animationState.hasActiveAnimations shouldBe true
  }

  it should "not animate buffers on close (buffer remains static)" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)
    clearBufferAnimations(sm)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()
    sm.getBufferAnimations.unsafeRunSync().values.exists(_.hasActiveAnimations) shouldBe false
  }

  it should "fade the ghost overlay out without row-staggered collapse" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    val ghost        = state.runtime.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay]).get
    val ghostAnim    = state.runtime.surfaceAnimations(ghost.id).animationState
    val animatedRows = ghostAnim.animations.keys.map(_.line).toSet.toList.sorted

    val firstRowSteps = ghostAnim.getCell(0, animatedRows.head).map(_.backgroundSteps.length).getOrElse(0)
    animatedRows.foreach { row =>
      ghostAnim.getCell(0, row).map(_.backgroundSteps.length).getOrElse(0) shouldBe firstRowSteps
    }
  }

  it should "reverse a partial fade from the command runner's current opacity" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val openedState = sm.getCurrentState.unsafeRunSync()
    val surfaceId   = openedState.commandRunnerSurface.get.id

    (1 to 3).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

    val partialState       = sm.getCurrentState.unsafeRunSync()
    val partialFadeCell    = partialState.runtime.surfaceAnimations(surfaceId).animationState.getCell(0, 0).get
    val partialBackground  = partialFadeCell.currentBackground.get
    val totalFadeSteps     = com.serenity.animation.AnimationConfig.Enabled.smooth.steps
    val remainingFadeSteps = partialFadeCell.backgroundSteps.length

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val closedState = sm.getCurrentState.unsafeRunSync()
    val ghost       = closedState.runtime.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay]).get
    val ghostCell   = closedState.runtime.surfaceAnimations(ghost.id).animationState.getCell(0, 0).get

    ghostCell.currentBackground shouldBe Some(partialBackground)
    ghostCell.backgroundSteps.length shouldBe (totalFadeSteps - remainingFadeSteps + 1)
  }

  it should "reverse an exiting ghost into the reopened command runner" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val closingState = sm.getCurrentState.unsafeRunSync()
    val ghost = closingState.runtime.uiSurfaces
      .find(_.content.isInstanceOf[SurfaceContent.GhostOverlay])
      .getOrElse(fail("Expected exiting command runner ghost"))
    sm.advanceAnimationsOnTick().unsafeRunSync()
    val ghostBackground = sm.getCurrentState
      .unsafeRunSync()
      .runtime
      .surfaceAnimations(ghost.id)
      .animationState
      .getCell(0, 0)
      .flatMap(_.currentBackground)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val reopened  = sm.getCurrentState.unsafeRunSync()
    val surfaceId = reopened.commandRunnerSurface.map(_.id).getOrElse(fail("Expected reopened command runner"))
    reopened.runtime.uiSurfaces.exists(_.id == ghost.id) shouldBe false
    reopened.runtime
      .surfaceAnimations(surfaceId)
      .animationState
      .getCell(0, 0)
      .flatMap(_.currentBackground) shouldBe ghostBackground
  }

  it should "remove the ghost surface when Exiting animation completes" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    // Advance far enough to exhaust exiting animation
    (1 to 60).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

    val state = sm.getCurrentState.unsafeRunSync()
    state.runtime.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe false
  }

  it should "work via Escape key as well as toggle" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)

    sm.applyEvent(Escape).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.commandRunnerSurface shouldBe None
    state.runtime.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe true
  }

  it should "animate the submenu preview when settings browsing opens a child panel" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    openSettingsCategory(sm)

    val state   = sm.getCurrentState.unsafeRunSync()
    val submenu = state.commandRunnerSubmenuSurface.getOrElse(fail("Expected submenu preview surface"))

    state.runtime.surfaceAnimations.get(submenu.id) shouldBe defined
    state.runtime.surfaceAnimations(submenu.id).phase shouldBe SurfacePhase.Visible
  }

  it should "restart the submenu animation when the preview changes to another settings group" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    openSettingsCategory(sm)

    val firstState   = sm.getCurrentState.unsafeRunSync()
    val firstSubmenu = firstState.commandRunnerSubmenuSurface.getOrElse(fail("Expected submenu preview surface"))
    advanceSurfaceAnimations(sm)

    sm.getCurrentState.unsafeRunSync().runtime.surfaceAnimations.get(firstSubmenu.id) shouldBe None

    sm.applyEvent(MoveDown).unsafeRunSync()
    val updatedState   = sm.getCurrentState.unsafeRunSync()
    val updatedSubmenu = updatedState.commandRunnerSubmenuSurface.getOrElse(fail("Expected updated submenu preview"))

    updatedSubmenu.id shouldBe firstSubmenu.id
    updatedSubmenu.content should not be firstSubmenu.content
    updatedState.runtime.surfaceAnimations.get(updatedSubmenu.id) shouldBe defined
    updatedState.runtime.surfaceAnimations(updatedSubmenu.id).phase shouldBe SurfacePhase.Visible
  }

  it should "restart the submenu animation when a preview becomes the focused submenu" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    openSettingsCategory(sm)

    val previewState   = sm.getCurrentState.unsafeRunSync()
    val previewSubmenu = previewState.commandRunnerSubmenuSurface.getOrElse(fail("Expected submenu preview surface"))
    advanceSurfaceAnimations(sm)

    sm.getCurrentState.unsafeRunSync().runtime.surfaceAnimations.get(previewSubmenu.id) shouldBe None

    sm.applyEvent(Enter).unsafeRunSync()
    val focusedState   = sm.getCurrentState.unsafeRunSync()
    val focusedSubmenu = focusedState.commandRunnerSubmenuSurface.getOrElse(fail("Expected focused submenu surface"))

    focusedSubmenu.id shouldBe previewSubmenu.id
    focusedSubmenu.content should not be previewSubmenu.content
    focusedState.runtime.surfaceAnimations.get(focusedSubmenu.id) shouldBe defined
    focusedState.runtime.surfaceAnimations(focusedSubmenu.id).phase shouldBe SurfacePhase.Visible
  }

  it should "add a ghost overlay when the submenu preview is dismissed" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    openSettingsCategory(sm)
    advanceToVisible(sm)

    sm.applyEvent(TabKey).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.commandRunnerSubmenuSurface shouldBe None
    state.runtime.uiSurfaces.exists {
      case UiSurface(_, SurfaceContent.GhostOverlay(SurfaceContent.CommandPaletteSubmenu(_, _, _), _), _, _) => true
      case _                                                                                                 => false
    } shouldBe true
  }

  private def advanceToVisible(sm: StateManager): Unit =
    val state0  = sm.getCurrentState.unsafeRunSync()
    val surfId  = state0.commandRunnerSurface.get.id
    val fadeLen = state0.runtime.surfaceAnimations.get(surfId).map(_.bufferFadeLength).getOrElse(0)
    (1 to (fadeLen + 1)).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

  private def advanceSurfaceAnimations(sm: StateManager): Unit =
    (1 to 60).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

  private def openSettingsCategory(sm: StateManager): Unit =
    (1 to 5).foreach(_ => sm.applyEvent(TabKey).unsafeRunSync())

  private def clearBufferAnimations(sm: StateManager): Unit =
    // Advance until all buffer animations are complete
    (1 to 30).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())
