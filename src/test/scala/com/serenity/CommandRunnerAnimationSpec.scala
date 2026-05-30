package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
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
    val logger = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerAnimationSpec"))
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

    val ghost    = state.uiSurfaces.find(_.content.isInstanceOf[SurfaceContent.GhostOverlay]).get
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

  private def advanceToVisible(sm: StateManager): Unit =
    val state0  = sm.getCurrentState.unsafeRunSync()
    val surfId  = state0.commandRunnerSurface.get.id
    val fadeLen = state0.surfaceAnimations.get(surfId).map(_.bufferFadeLength).getOrElse(0)
    (1 to (fadeLen + 1)).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())

  private def clearBufferAnimations(sm: StateManager): Unit =
    // Advance until all buffer animations are complete
    (1 to 30).foreach(_ => sm.advanceAnimationsOnTick().unsafeRunSync())
