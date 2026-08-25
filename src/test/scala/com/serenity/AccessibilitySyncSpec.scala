package com.serenity

import java.awt.Color

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.AnimationState
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.accessibility.{AccessibilitySnapshot, AccessibilitySync}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AccessibilitySyncSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)

  "AccessibilitySync" should "compute the snapshot once for a state reference and reuse it on repeated calls" in {
    val state = AppState.initial
    val program = for
      sync      <- AccessibilitySync.empty
      callCount <- IO.ref(0)
      compute = (previous: Option[AccessibilitySnapshot]) =>
        callCount.update(_ + 1).as(AccessibilitySnapshot.from(state, viewport, previous))
      first  <- sync.sync(state)(compute)
      second <- sync.sync(state)(compute)
      calls  <- callCount.get
    yield (first, second, calls)

    val (first, second, calls) = program.unsafeRunSync()
    (second eq first) shouldBe true
    calls shouldBe 1
  }

  it should "recompute when the state reference changes, threading the previous snapshot through" in {
    val stateA = AppState.initial
    val stateB = AppState.initial.copy(focus = Focus.Surface(SurfaceId("changed")))
    val program = for
      sync         <- AccessibilitySync.empty
      seenPrevious <- IO.ref(List.empty[Option[AccessibilitySnapshot]])
      callCount    <- IO.ref(0)
      compute = (state: AppState) =>
        (previous: Option[AccessibilitySnapshot]) =>
          seenPrevious.update(_ :+ previous) >>
            callCount.update(_ + 1).as(AccessibilitySnapshot.from(state, viewport, previous))
      first  <- sync.sync(stateA)(compute(stateA))
      second <- sync.sync(stateB)(compute(stateB))
      seen   <- seenPrevious.get
      calls  <- callCount.get
    yield (first, second, seen, calls)

    val (first, second, seen, calls) = program.unsafeRunSync()
    calls shouldBe 2
    seen shouldBe List(None, Some(first))
    (second eq first) shouldBe false
  }

  it should "not recompute for a distinct but value-equal state" in {
    val stateA = AppState.initial
    val stateB = AppState.initial
    val program = for
      sync      <- AccessibilitySync.empty
      callCount <- IO.ref(0)
      compute = (state: AppState) =>
        (previous: Option[AccessibilitySnapshot]) =>
          callCount.update(_ + 1).as(AccessibilitySnapshot.from(state, viewport, previous))
      _     <- sync.sync(stateA)(compute(stateA))
      _     <- sync.sync(stateB)(compute(stateB))
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 1
  }

  it should "not recompute when only the decorative window sitter ticked" in {
    val stateA = AppState.initial
    val stateB = stateA.copy(windowSitter = stateA.windowSitter.copy(activeTicks = 5, frameIndex = 2))
    val program = for
      sync      <- AccessibilitySync.empty
      callCount <- IO.ref(0)
      compute = (state: AppState) =>
        (previous: Option[AccessibilitySnapshot]) =>
          callCount.update(_ + 1).as(AccessibilitySnapshot.from(state, viewport, previous))
      first  <- sync.sync(stateA)(compute(stateA))
      second <- sync.sync(stateB)(compute(stateB))
      calls  <- callCount.get
    yield (first, second, calls)

    val (first, second, calls) = program.unsafeRunSync()
    (second eq first) shouldBe true
    calls shouldBe 1
  }

  it should "not recompute when only a theme transition or surface animation ticked" in {
    val stateA = AppState.initial
    val stateB = stateA.copy(
      themeTransition = Some(ThemeTransition(stateA.theme, currentStep = 1, totalSteps = 5)),
      surfaceAnimations = Map(SurfaceId("runner") -> SurfaceAnimationState())
    )
    val program = for
      sync      <- AccessibilitySync.empty
      callCount <- IO.ref(0)
      compute = (state: AppState) =>
        (previous: Option[AccessibilitySnapshot]) =>
          callCount.update(_ + 1).as(AccessibilitySnapshot.from(state, viewport, previous))
      _     <- sync.sync(stateA)(compute(stateA))
      _     <- sync.sync(stateB)(compute(stateB))
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 1
  }

  it should "not recompute when only a buffer's decorative character-reveal animation ticked" in {
    val bufferId = BufferId(1)
    val stateA   = AppState.initial.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "hello")))
    val animatedBuffer = stateA
      .buffers(bufferId)
      .copy(
        animations = AnimationState.empty.addCharacterAnimation('h', 0, 0, Color.BLACK, Color.WHITE, 5)
      )
    val stateB = stateA.copy(buffers = Map(bufferId -> animatedBuffer))
    val program = for
      sync      <- AccessibilitySync.empty
      callCount <- IO.ref(0)
      compute = (state: AppState) =>
        (previous: Option[AccessibilitySnapshot]) =>
          callCount.update(_ + 1).as(AccessibilitySnapshot.from(state, viewport, previous))
      _     <- sync.sync(stateA)(compute(stateA))
      _     <- sync.sync(stateB)(compute(stateB))
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 1
  }

  it should "not recompute when only a buffer's markdown-preview commit generation caught up" in {
    val bufferId = BufferId(1)
    val stateA = AppState.initial.copy(buffers =
      Map(bufferId -> Buffer.fromString(bufferId, "# hello").copy(markdownPreviewEditGeneration = 3L))
    )
    val stateB =
      stateA.copy(buffers = Map(bufferId -> stateA.buffers(bufferId).copy(markdownPreviewCommittedGeneration = 3L)))
    val program = for
      sync      <- AccessibilitySync.empty
      callCount <- IO.ref(0)
      compute = (state: AppState) =>
        (previous: Option[AccessibilitySnapshot]) =>
          callCount.update(_ + 1).as(AccessibilitySnapshot.from(state, viewport, previous))
      _     <- sync.sync(stateA)(compute(stateA))
      _     <- sync.sync(stateB)(compute(stateB))
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 1
  }

  it should "still recompute a real change even while the window sitter is also ticking" in {
    val stateA = AppState.initial
    val stateB = AppState.initial
      .copy(
        focus = Focus.Surface(SurfaceId("changed")),
        windowSitter = AppState.initial.windowSitter.copy(activeTicks = 5)
      )
    val program = for
      sync      <- AccessibilitySync.empty
      callCount <- IO.ref(0)
      compute = (state: AppState) =>
        (previous: Option[AccessibilitySnapshot]) =>
          callCount.update(_ + 1).as(AccessibilitySnapshot.from(state, viewport, previous))
      _     <- sync.sync(stateA)(compute(stateA))
      _     <- sync.sync(stateB)(compute(stateB))
      calls <- callCount.get
    yield calls

    program.unsafeRunSync() shouldBe 2
  }
