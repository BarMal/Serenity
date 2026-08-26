package com.serenity

import java.awt.Color

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationState, CharacterKey, TransitionKind}
import com.serenity.config.{AppConfig, MotionPreset}
import com.serenity.keystroke.events.{DeleteBackward, InsertChar, NewLine, Paste, ScrollDown}
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
      sm               <- IO.pure(makeStateManager())
      _                <- sm.updateState(_.copy(config = AppConfig.withTestAnimations))
      bufferId         <- sm.createBuffer("Hello")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 5)
      _                <- sm.applyEvent(InsertChar('a'))
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      animations.animations should contain key CharacterKey(5, 0)
      animations.animations should have size 1

    program.unsafeRunSync()
  }

  it should "remain stable after viewport scrolling" in {
    val program = for
      sm                    <- IO.pure(makeStateManager())
      _                     <- sm.updateState(_.copy(config = AppConfig.withTestAnimations))
      bufferId              <- sm.createNewEmptyBuffer()
      state                 <- sm.getCurrentState
      paneId                <- IO.pure(state.layout.editorPanes.keys.head)
      _                     <- sm.setBufferForPane(paneId, bufferId)
      _                     <- sm.applyEvent(InsertChar('a'))
      animationsAfterType   <- sm.getBufferAnimations
      _                     <- sm.applyEvent(ScrollDown(5))
      animationsAfterScroll <- sm.getBufferAnimations
    yield
      val typedAnimations    = animationsAfterType.getOrElse(bufferId, AnimationState.empty)
      val scrolledAnimations = animationsAfterScroll.getOrElse(bufferId, AnimationState.empty)
      typedAnimations.animations should contain key CharacterKey(0, 0)
      scrolledAnimations.animations should contain key CharacterKey(0, 0)
      scrolledAnimations.animations should have size 1

    program.unsafeRunSync()
  }

  it should "key multi-line content at the correct buffer line" in {
    val program = for
      sm               <- IO.pure(makeStateManager())
      _                <- sm.updateState(_.copy(config = AppConfig.withTestAnimations))
      bufferId         <- sm.createBuffer("line one\nline two")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 1, 3)
      _                <- sm.applyEvent(InsertChar('X'))
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      animations.animations should contain key CharacterKey(3, 1)
      animations.animations should have size 1

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
      bufferId         <- sm.createBuffer("Hello")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 5)
      _                <- sm.applyEvent(InsertChar('a'))
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      val cell       = animations.getCell(5, 0).get
      cell.foregroundAnimation.map(_.steps) shouldBe Some(
        AppConfig.default
          .withMotionPreset(MotionPreset.Smooth)
          .characterAnimation
          .get
          .steps * 2
      )

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
      bufferId         <- sm.createBuffer("Hello")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 5)
      _                <- sm.applyEvent(InsertChar('a'))
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val buffer     = newState.buffers(bufferId)
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      buffer.document.content.collect() shouldBe "Helloa"
      animations.animations shouldBe empty

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
      bufferId         <- sm.createBuffer("Hello")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 5)
      _                <- sm.applyEvent(Paste)
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val buffer     = newState.buffers(bufferId)
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      val firstCell  = animations.getCell(5, 0).getOrElse(fail("Expected first pasted cell animation"))
      val secondCell = animations.getCell(6, 0).getOrElse(fail("Expected second pasted cell animation"))
      buffer.document.content.collect() shouldBe "Helloab"
      animations.animations.keySet should contain allOf (CharacterKey(5, 0), CharacterKey(6, 0))
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
      bufferId         <- sm.createBuffer("Hello")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 5)
      _                <- sm.applyEvent(Paste)
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
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
      bufferId         <- sm.createBuffer("prefix\nsuffix")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 3)
      _                <- sm.applyEvent(Paste)
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
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
      bufferId         <- sm.createBuffer("Hello")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 5)
      _                <- sm.applyEvent(Paste)
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      animations
        .getCell(5, 0)
        .flatMap(_.foregroundAnimation)
        .map(animation => animation.steps -> animation.delayFrames) shouldBe
        Some(7 -> 0)
      animations
        .getCell(6, 0)
        .flatMap(_.foregroundAnimation)
        .map(animation => animation.steps -> animation.delayFrames) shouldBe
        Some(7 -> 1)

    program.unsafeRunSync()
  }

  it should "cap animation cells generated for a large paste, without truncating the pasted content" in {
    val largeText = "x" * 5000
    val program = for
      sm <- IO.pure(makeStateManager())
      _ <- sm.updateState(state =>
        state.copy(
          config = AppConfig.default.withMotionPreset(MotionPreset.Subtle),
          clipboard = Some(largeText)
        )
      )
      bufferId         <- sm.createBuffer("")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 0)
      _                <- sm.applyEvent(Paste)
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      val buffer     = newState.buffers(bufferId)
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      buffer.document.content.collect() shouldBe largeText
      animations.animations.size should be <=
        com.serenity.state.manager.VisibleBufferAnimationCells.DefaultMaxAnimatedCells

    program.unsafeRunSync()
  }

  it should "remap an animating character's key when an edit inserts a line above it" in {
    val program = for
      sm               <- IO.pure(makeStateManager())
      _                <- sm.updateState(_.copy(config = AppConfig.withTestAnimations))
      bufferId         <- sm.createBuffer("line one\nline two")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 1, 3)
      _                <- sm.applyEvent(InsertChar('X'))
      typedAnimations  <- sm.getBufferAnimations
      _                <- sm.setCursorPosition(paneId, 0, 0)
      _                <- sm.applyEvent(NewLine)
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      typedAnimations.getOrElse(bufferId, AnimationState.empty).animations should contain key CharacterKey(3, 1)

      val buffer     = newState.buffers(bufferId)
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      buffer.document.content.collect() shouldBe "\nline one\nlinXe two"
      animations.animations should contain key CharacterKey(3, 2)
      animations.animations should not contain key(CharacterKey(3, 1))
      animations.animations should have size 1

    program.unsafeRunSync()
  }

  it should "drop an animation when a later edit deletes that exact character" in {
    val program = for
      sm               <- IO.pure(makeStateManager())
      _                <- sm.updateState(_.copy(config = AppConfig.withTestAnimations))
      bufferId         <- sm.createBuffer("Hello")
      state            <- sm.getCurrentState
      paneId           <- IO.pure(state.layout.editorPanes.keys.head)
      _                <- sm.setBufferForPane(paneId, bufferId)
      _                <- sm.setCursorPosition(paneId, 0, 5)
      _                <- sm.applyEvent(InsertChar('a'))
      typedAnimations  <- sm.getBufferAnimations
      _                <- sm.applyEvent(DeleteBackward)
      newState         <- sm.getCurrentState
      bufferAnimations <- sm.getBufferAnimations
    yield
      typedAnimations.getOrElse(bufferId, AnimationState.empty).animations should contain key CharacterKey(5, 0)

      val buffer     = newState.buffers(bufferId)
      val animations = bufferAnimations.getOrElse(bufferId, AnimationState.empty)
      buffer.document.content.collect() shouldBe "Hello"
      animations.animations shouldBe empty

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
