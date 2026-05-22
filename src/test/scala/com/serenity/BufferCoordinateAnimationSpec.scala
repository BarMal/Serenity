package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.TextColor
import com.serenity.animation.{AnimationState, CharacterKey}
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{InsertChar, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Verifies that character animations are keyed by buffer position (line, column), not screen
  * position. Screen-position keying causes animations to "jump" to wrong characters when the
  * viewport scrolls or the terminal is resized.
  */
class BufferCoordinateAnimationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  "Character animation" should "store at buffer coordinates, not screen coordinates" in {
    val sm = makeStateManager()
    sm.updateState(_.copy(config = AppConfig.withTestAnimations)).unsafeRunSync()

    val bufferId = sm.createBuffer("Hello").unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()
    sm.setCursorPosition(paneId, 0, 5).unsafeRunSync()

    sm.applyEvent(InsertChar('a')).unsafeRunSync()

    val newState = sm.getCurrentState.unsafeRunSync()

    // Animation keyed at buffer column=5, line=0 — not at any screen coordinate
    newState.screenAnimations.animations should contain key CharacterKey(5, 0)
    // There should be exactly one animation entry
    newState.screenAnimations.animations should have size 1
  }

  it should "remain stable after viewport scrolling" in {
    val sm = makeStateManager()
    sm.updateState(_.copy(config = AppConfig.withTestAnimations)).unsafeRunSync()

    val bufferId = sm.createNewEmptyBuffer().unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()

    sm.applyEvent(InsertChar('a')).unsafeRunSync()

    val stateAfterType = sm.getCurrentState.unsafeRunSync()
    stateAfterType.screenAnimations.animations should contain key CharacterKey(0, 0)

    // Scroll the viewport
    sm.applyEvent(ScrollDown(5)).unsafeRunSync()

    val stateAfterScroll = sm.getCurrentState.unsafeRunSync()

    // Buffer-keyed animation survives viewport changes intact
    stateAfterScroll.screenAnimations.animations should contain key CharacterKey(0, 0)
    stateAfterScroll.screenAnimations.animations should have size 1
  }

  it should "key multi-line content at the correct buffer line" in {
    val sm = makeStateManager()
    sm.updateState(_.copy(config = AppConfig.withTestAnimations)).unsafeRunSync()

    val bufferId = sm.createBuffer("line one\nline two").unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()
    sm.setCursorPosition(paneId, 1, 3).unsafeRunSync()

    sm.applyEvent(InsertChar('X')).unsafeRunSync()

    val newState = sm.getCurrentState.unsafeRunSync()

    // Typed on line 1, column 3 → key is (column=3, line=1)
    newState.screenAnimations.animations should contain key CharacterKey(3, 1)
    newState.screenAnimations.animations should have size 1
  }

  "AnimationState" should "be queryable by buffer column and line" in {
    val anim = AnimationState.empty.addCharacterAnimation(
      'a', 3, 2, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 5
    )

    // Exact buffer position lookup succeeds
    anim.getCharacterColor(3, 2) should be(defined)
    // Adjacent positions return nothing
    anim.getCharacterColor(0, 0) should not be(defined)
    anim.getCharacterColor(3, 0) should not be(defined)
    anim.getCharacterColor(0, 2) should not be(defined)
  }
