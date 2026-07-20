package com.serenity

import java.awt.Color

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationState, CharacterKey, TransitionKind}
import com.serenity.config.{AppConfig, MotionPreset}
import com.serenity.keystroke.events.{InsertChar, Paste, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Verifies that character animations are keyed by buffer position (line, column), not screen position. Screen-position
  * keying causes animations to "jump" to wrong characters when the viewport scrolls or the terminal is resized.
  */
class BufferCoordinateAnimationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  "Character animation" should "store at buffer coordinates, not screen coordinates" in {
    val program = for
      sm       <- IO.pure(makeStateManager())
      _        <- sm.updateState(_.copy(config = AppConfig.withTestAnimations))
      bufferId <- sm.createBuffer("Hello")
      state    <- sm.getCurrentState
      paneId   <- IO.pure(state.layout.editorPanes.keys.head)
      _        <- sm.setBufferForPane(paneId, bufferId)
      _        <- sm.setCursorPosition(paneId, 0, 5)
      _        <- sm.applyEvent(InsertChar('a'))
      newState <- sm.getCurrentState
    yield
      val buffer = newState.buffers(bufferId)
      buffer.animations.animations should contain key CharacterKey(5, 0)
      buffer.animations.animations should have size 1

    program.unsafeRunSync()
  }

  it should "remain stable after viewport scrolling" in {
    val program = for
      sm               <- IO.pure(makeStateManager())
      _                <- sm.updateState(_.copy(config = AppConfig.withTestAnimations))
      bufferId         <- sm.createNewEmptyBuffer()
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.applyEvent(InsertChar('a'))
      stateAfterType   <- sm.getCurrentState
      _                <- sm.applyEvent(ScrollDown(5))
      stateAfterScroll <- sm.getCurrentState
    yield
      val typedBuffer    = stateAfterType.buffers(bufferId)
      val scrolledBuffer = stateAfterScroll.buffers(bufferId)
      typedBuffer.animations.animations should contain key CharacterKey(0, 0)
      scrolledBuffer.animations.animations should contain key CharacterKey(0, 0)
      scrolledBuffer.animations.animations should have size 1

    program.unsafeRunSync()
  }

  it should "key multi-line content at the correct buffer line" in {
    val program = for
      sm       <- IO.pure(makeStateManager())
      _        <- sm.updateState(_.copy(config = AppConfig.withTestAnimations))
      bufferId <- sm.createBuffer("line one\nline two")
      state    <- sm.getCurrentState
      paneId   <- IO.pure(state.layout.editorPanes.keys.head)
      _        <- sm.setBufferForPane(paneId, bufferId)
      _        <- sm.setCursorPosition(paneId, 1, 3)
      _        <- sm.applyEvent(InsertChar('X'))
      newState <- sm.getCurrentState
    yield
      val buffer = newState.buffers(bufferId)
      buffer.animations.animations should contain key CharacterKey(3, 1)
      buffer.animations.animations should have size 1

    program.unsafeRunSync()
  }

  it should "scale typed character animation length with the global animation speed" in {
    val program = for
      sm <- IO.pure(makeStateManager())
      _ <- sm.updateState(state =>
        state.copy(config =
          AppConfig.default
            .withMotionPreset(MotionPreset.Smooth)
            .withElementTransitionSpeedScale(2.0)
        )
      )
      bufferId <- sm.createBuffer("Hello")
      state    <- sm.getCurrentState
      paneId   <- IO.pure(state.layout.editorPanes.keys.head)
      _        <- sm.setBufferForPane(paneId, bufferId)
      _        <- sm.setCursorPosition(paneId, 0, 5)
      _        <- sm.applyEvent(InsertChar('a'))
      newState <- sm.getCurrentState
    yield
      val buffer = newState.buffers(bufferId)
      val cell   = buffer.animations.getCell(5, 0).get
      cell.foregroundSteps.length shouldBe AppConfig.default
        .withMotionPreset(MotionPreset.Smooth)
        .characterAnimation
        .get
        .steps * 2

    program.unsafeRunSync()
  }

  it should "skip typed character animation when editor insertion transitions are disabled" in {
    val program = for
      sm <- IO.pure(makeStateManager())
      _ <- sm.updateState(state =>
        state.copy(config =
          AppConfig.default
            .withMotionPreset(MotionPreset.Smooth)
            .withEditorInsertionTransitionKind(TransitionKind.Disabled)
        )
      )
      bufferId <- sm.createBuffer("Hello")
      state    <- sm.getCurrentState
      paneId   <- IO.pure(state.layout.editorPanes.keys.head)
      _        <- sm.setBufferForPane(paneId, bufferId)
      _        <- sm.setCursorPosition(paneId, 0, 5)
      _        <- sm.applyEvent(InsertChar('a'))
      newState <- sm.getCurrentState
    yield
      val buffer = newState.buffers(bufferId)
      buffer.content.collect() shouldBe "Helloa"
      buffer.animations.animations shouldBe empty

    program.unsafeRunSync()
  }

  it should "animate pasted inserted spans with editor insertion choreography" in {
    val program = for
      sm <- IO.pure(makeStateManager())
      _ <- sm.updateState(state =>
        state.copy(
          config = AppConfig.default
            .withMotionPreset(MotionPreset.Subtle)
            .withEditorInsertionTransitionKind(TransitionKind.DirectionalSweep),
          clipboard = Some("ab")
        )
      )
      bufferId <- sm.createBuffer("Hello")
      state    <- sm.getCurrentState
      paneId   <- IO.pure(state.layout.editorPanes.keys.head)
      _        <- sm.setBufferForPane(paneId, bufferId)
      _        <- sm.setCursorPosition(paneId, 0, 5)
      _        <- sm.applyEvent(Paste)
      newState <- sm.getCurrentState
    yield
      val buffer     = newState.buffers(bufferId)
      val firstCell  = buffer.animations.getCell(5, 0).getOrElse(fail("Expected first pasted cell animation"))
      val secondCell = buffer.animations.getCell(6, 0).getOrElse(fail("Expected second pasted cell animation"))
      buffer.content.collect() shouldBe "Helloab"
      buffer.animations.animations.keySet should contain allOf (CharacterKey(5, 0), CharacterKey(6, 0))
      firstCell.foregroundAnimation.map(animation => animation.steps -> animation.delayFrames) shouldBe Some(10 -> 0)
      secondCell.foregroundAnimation.map(animation => animation.steps -> animation.delayFrames) shouldBe Some(10 -> 1)

    program.unsafeRunSync()
  }

  it should "reveal default fade insertions in staggered character slices" in {
    val program = for
      sm <- IO.pure(makeStateManager())
      _ <- sm.updateState(state =>
        state.copy(
          config = AppConfig.default.withMotionPreset(MotionPreset.Smooth),
          clipboard = Some("abc")
        )
      )
      bufferId <- sm.createBuffer("Hello")
      state    <- sm.getCurrentState
      paneId   <- IO.pure(state.layout.editorPanes.keys.head)
      _        <- sm.setBufferForPane(paneId, bufferId)
      _        <- sm.setCursorPosition(paneId, 0, 5)
      _        <- sm.applyEvent(Paste)
      newState <- sm.getCurrentState
    yield
      val animations = newState.buffers(bufferId).animations
      List(5, 6, 7).map(column =>
        animations
          .getCell(column, 0)
          .flatMap(_.foregroundAnimation)
          .map(_.delayFrames)
          .getOrElse(fail(s"Expected an animation at column $column"))
      ) shouldBe List(0, 1, 2)

    program.unsafeRunSync()
  }

  it should "restart default fade slices at the start of each inserted line" in {
    val program = for
      sm <- IO.pure(makeStateManager())
      _ <- sm.updateState(state =>
        state.copy(
          config = AppConfig.default.withMotionPreset(MotionPreset.Smooth),
          clipboard = Some("abc\ndef")
        )
      )
      bufferId <- sm.createBuffer("prefix\nsuffix")
      state    <- sm.getCurrentState
      paneId   <- IO.pure(state.layout.editorPanes.keys.head)
      _        <- sm.setBufferForPane(paneId, bufferId)
      _        <- sm.setCursorPosition(paneId, 0, 3)
      _        <- sm.applyEvent(Paste)
      newState <- sm.getCurrentState
    yield
      val animations = newState.buffers(bufferId).animations
      def delayAt(column: Int, line: Int): Int =
        animations
          .getCell(column, line)
          .flatMap(_.foregroundAnimation)
          .map(_.delayFrames)
          .getOrElse(fail(s"Expected an animation at ($column, $line)"))

      List(delayAt(3, 0), delayAt(4, 0), delayAt(5, 0)) shouldBe List(0, 1, 2)
      List(delayAt(0, 1), delayAt(1, 1), delayAt(2, 1)) shouldBe List(0, 1, 2)

    program.unsafeRunSync()
  }

  it should "use the editor text speed scale for inserted span choreography" in {
    val program = for
      sm <- IO.pure(makeStateManager())
      _ <- sm.updateState(state =>
        state.copy(
          config = AppConfig.default
            .withMotionPreset(MotionPreset.Smooth)
            .withEditorInsertionTransitionKind(TransitionKind.DirectionalSweep)
            .withEditorTextTransitionSpeedScale(Some(0.5))
            .withUiTransitionSpeedScale(Some(2.0)),
          clipboard = Some("ab")
        )
      )
      bufferId <- sm.createBuffer("Hello")
      state    <- sm.getCurrentState
      paneId   <- IO.pure(state.layout.editorPanes.keys.head)
      _        <- sm.setBufferForPane(paneId, bufferId)
      _        <- sm.setCursorPosition(paneId, 0, 5)
      _        <- sm.applyEvent(Paste)
      newState <- sm.getCurrentState
    yield
      val buffer = newState.buffers(bufferId)
      buffer.animations
        .getCell(5, 0)
        .flatMap(_.foregroundAnimation)
        .map(animation => animation.steps -> animation.delayFrames) shouldBe
        Some(7 -> 0)
      buffer.animations
        .getCell(6, 0)
        .flatMap(_.foregroundAnimation)
        .map(animation => animation.steps -> animation.delayFrames) shouldBe
        Some(7 -> 1)

    program.unsafeRunSync()
  }

  "AnimationState" should "be queryable by buffer column and line" in {
    val anim = AnimationState.empty.addCharacterAnimation(
      'a',
      3,
      2,
      Color.BLACK,
      Color.WHITE,
      5
    )

    anim.getCharacterColor(3, 2) should be(defined)
    anim.getCharacterColor(0, 0) should not be defined
    anim.getCharacterColor(3, 0) should not be defined
    anim.getCharacterColor(0, 2) should not be defined
  }
