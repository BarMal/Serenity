package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.command.{Command, CommandCategory, CommandIntent}
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
    val animState = state.surfaceAnimations.get(surfaceId)
    animState shouldBe defined
    animState.get.phase shouldBe SurfacePhase.Visible
  }

  it should "set bufferFadeLength to zero (buffer is not animated)" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    val anim      = state.surfaceAnimations(surfaceId)
    anim.bufferFadeLength shouldBe 0
  }

  it should "have active overlay animations but no buffer animations after open" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    state.buffers.values.exists(_.animations.hasActiveAnimations) shouldBe false
    state.surfaceAnimations(surfaceId).animationState.hasActiveAnimations shouldBe true
  }

  it should "skip command runner fade when the command runner animation is disabled" in {
    val sm = createStateManager()
    sm.updateState(_.copy(config = AppConfig.default.withCommandRunnerAnimation(None)))
      .unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    state.surfaceAnimations.get(surfaceId) shouldBe None
  }

  it should "skip command runner fade when the global animation speed is zero" in {
    val sm = createStateManager()
    sm.updateState { state =>
      state.copy(config =
        AppConfig.default
          .withMotionPreset(MotionPreset.Smooth)
          .withElementTransitionSpeedScale(0.0)
      )
    }.unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    state.surfaceAnimations.get(surfaceId) shouldBe None
  }

  it should "cancel active command-surface animations when motion accessibility is reduced or off" in
    List(MotionAccessibility.Reduced, MotionAccessibility.Off).foreach { accessibility =>
      val sm = createStateManager()
      sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
      advanceToVisible(sm)
      sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

      sm.getCurrentState.unsafeRunSync().surfaceAnimations should not be empty
      sm.getCurrentState
        .unsafeRunSync()
        .uiSurfaces
        .exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe true

      sm.executeCommand(
        Command.typed(
          "motion-accessibility",
          "Set motion accessibility",
          CommandIntent.SetMotionAccessibility(accessibility),
          CommandCategory.Settings
        )
      ).unsafeRunSync()

      val state = sm.getCurrentState.unsafeRunSync()
      state.surfaceAnimations shouldBe Map.empty
      state.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe false
    }

  it should "scale command runner fade length with the global animation speed" in {
    val sm = createStateManager()
    sm.updateState { state =>
      state.copy(config =
        AppConfig.default
          .withMotionPreset(MotionPreset.Smooth)
          .withElementTransitionSpeedScale(2.0)
      )
    }.unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state     = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state.commandRunnerSurface.get.id
    val firstCell = state.surfaceAnimations(surfaceId).animationState.getCell(0, 0).get
    firstCell.backgroundSteps.length shouldBe AnimationConfig.smooth.get.steps * 2
  }

  it should "use the command runner reveal kind for open choreography" in {
    val sm = createStateManager()
    sm.updateState { state =>
      state.copy(config =
        AppConfig.default
          .withCommandRunnerTransitionKind(Some(TransitionKind.DirectionalSweep))
      )
    }.unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state       = sm.getCurrentState.unsafeRunSync()
    val surfaceId   = state.commandRunnerSurface.get.id
    val animatedCol = state.surfaceAnimations(surfaceId).animationState.animations.keys.map(_.column).max
    animatedCol should be > 0
  }

  it should "transition to Visible after bufferFadeLength ticks" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state0    = sm.getCurrentState.unsafeRunSync()
    val surfaceId = state0.commandRunnerSurface.get.id
    val fadeLen   = state0.surfaceAnimations(surfaceId).bufferFadeLength

    (1 to fadeLen).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

    val state1 = sm.getCurrentState.unsafeRunSync()
    state1.surfaceAnimations.get(surfaceId).map(_.phase) shouldBe Some(SurfacePhase.Visible)
  }

  it should "have overlay fade-in animation after transitioning to Visible" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state0  = sm.getCurrentState.unsafeRunSync()
    val surfId  = state0.commandRunnerSurface.get.id
    val fadeLen = state0.surfaceAnimations(surfId).bufferFadeLength

    (1 to fadeLen).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

    val state1 = sm.getCurrentState.unsafeRunSync()
    val anim   = state1.surfaceAnimations(surfId)
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
    val ghost = state.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay])
    ghost shouldBe defined
  }

  it should "mark the ghost surface as Exiting in surfaceAnimations" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    val ghost     = state.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay]).get
    val ghostAnim = state.surfaceAnimations.get(ghost.id)
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
    state.buffers.values.exists(_.animations.hasActiveAnimations) shouldBe false
  }

  it should "fade the ghost overlay out without row-staggered collapse" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    val ghost        = state.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay]).get
    val ghostAnim    = state.surfaceAnimations(ghost.id).animationState
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
    val partialFadeCell    = partialState.surfaceAnimations(surfaceId).animationState.getCell(0, 0).get
    val partialBackground  = partialFadeCell.currentBackground.get
    val totalFadeSteps     = com.serenity.animation.AnimationConfig.smooth.get.steps
    val remainingFadeSteps = partialFadeCell.backgroundSteps.length

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val closedState = sm.getCurrentState.unsafeRunSync()
    val ghost       = closedState.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay]).get
    val ghostCell   = closedState.surfaceAnimations(ghost.id).animationState.getCell(0, 0).get

    ghostCell.currentBackground shouldBe Some(partialBackground)
    ghostCell.backgroundSteps.length shouldBe (totalFadeSteps - remainingFadeSteps + 1)
  }

  it should "reverse an exiting ghost into the reopened command runner" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val closingState = sm.getCurrentState.unsafeRunSync()
    val ghost = closingState.uiSurfaces
      .find(_.content.isInstanceOf[SurfaceContent.GhostOverlay])
      .getOrElse(fail("Expected exiting command runner ghost"))
    sm.advanceAnimationsOnTick().unsafeRunSync()
    val ghostBackground = sm.getCurrentState
      .unsafeRunSync()
      .surfaceAnimations(ghost.id)
      .animationState
      .getCell(0, 0)
      .flatMap(_.currentBackground)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val reopened  = sm.getCurrentState.unsafeRunSync()
    val surfaceId = reopened.commandRunnerSurface.map(_.id).getOrElse(fail("Expected reopened command runner"))
    reopened.uiSurfaces.exists(_.id == ghost.id) shouldBe false
    reopened
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
    state.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe false
  }

  it should "work via Escape key as well as toggle" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    advanceToVisible(sm)

    sm.applyEvent(Escape).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.commandRunnerSurface shouldBe None
    state.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe true
  }

  it should "animate the submenu preview when settings browsing opens a child panel" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    openSettingsCategory(sm)

    val state   = sm.getCurrentState.unsafeRunSync()
    val submenu = state.commandRunnerSubmenuSurface.getOrElse(fail("Expected submenu preview surface"))

    state.surfaceAnimations.get(submenu.id) shouldBe defined
    state.surfaceAnimations(submenu.id).phase shouldBe SurfacePhase.Visible
  }

  it should "restart the submenu animation when the preview changes to another settings group" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    openSettingsCategory(sm)

    val firstState   = sm.getCurrentState.unsafeRunSync()
    val firstSubmenu = firstState.commandRunnerSubmenuSurface.getOrElse(fail("Expected submenu preview surface"))
    advanceSurfaceAnimations(sm)

    sm.getCurrentState.unsafeRunSync().surfaceAnimations.get(firstSubmenu.id) shouldBe None

    sm.applyEvent(MoveDown).unsafeRunSync()
    val updatedState   = sm.getCurrentState.unsafeRunSync()
    val updatedSubmenu = updatedState.commandRunnerSubmenuSurface.getOrElse(fail("Expected updated submenu preview"))

    updatedSubmenu.id shouldBe firstSubmenu.id
    updatedSubmenu.content should not be firstSubmenu.content
    updatedState.surfaceAnimations.get(updatedSubmenu.id) shouldBe defined
    updatedState.surfaceAnimations(updatedSubmenu.id).phase shouldBe SurfacePhase.Visible
  }

  it should "restart the submenu animation when a preview becomes the focused submenu" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    openSettingsCategory(sm)

    val previewState   = sm.getCurrentState.unsafeRunSync()
    val previewSubmenu = previewState.commandRunnerSubmenuSurface.getOrElse(fail("Expected submenu preview surface"))
    advanceSurfaceAnimations(sm)

    sm.getCurrentState.unsafeRunSync().surfaceAnimations.get(previewSubmenu.id) shouldBe None

    sm.applyEvent(Enter).unsafeRunSync()
    val focusedState   = sm.getCurrentState.unsafeRunSync()
    val focusedSubmenu = focusedState.commandRunnerSubmenuSurface.getOrElse(fail("Expected focused submenu surface"))

    focusedSubmenu.id shouldBe previewSubmenu.id
    focusedSubmenu.content should not be previewSubmenu.content
    focusedState.surfaceAnimations.get(focusedSubmenu.id) shouldBe defined
    focusedState.surfaceAnimations(focusedSubmenu.id).phase shouldBe SurfacePhase.Visible
  }

  it should "add a ghost overlay when the submenu preview is dismissed" in {
    val sm = createStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    openSettingsCategory(sm)
    advanceToVisible(sm)

    sm.applyEvent(TabKey).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.commandRunnerSubmenuSurface shouldBe None
    state.uiSurfaces.exists {
      case UiSurface(_, SurfaceContent.GhostOverlay(SurfaceContent.CommandPaletteSubmenu(_, _, _), _), _, _) => true
      case _                                                                                                 => false
    } shouldBe true
  }

  private def advanceToVisible(sm: StateManager): Unit =
    val state0  = sm.getCurrentState.unsafeRunSync()
    val surfId  = state0.commandRunnerSurface.get.id
    val fadeLen = state0.surfaceAnimations.get(surfId).map(_.bufferFadeLength).getOrElse(0)
    (1 to (fadeLen + 1)).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

  private def advanceSurfaceAnimations(sm: StateManager): Unit =
    (1 to 60).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

  private def openSettingsCategory(sm: StateManager): Unit =
    (1 to 5).foreach(_ => sm.applyEvent(TabKey).unsafeRunSync())

  private def clearBufferAnimations(sm: StateManager): Unit =
    // Advance until all buffer animations are complete
    (1 to 30).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())
