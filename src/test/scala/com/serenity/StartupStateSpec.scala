package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StartupStateSpec extends AnyFlatSpec with Matchers:

  behavior of "Application Startup State"

  it should "start with exactly one pane and one buffer" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]

    // Given: Fresh StateManager (simulates app startup)
    val logger       = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager.apply(logger).unsafeRunSync()

    // When: Get the initial state
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should have exactly 1 pane and 1 buffer
    initialState.layout.editorPanes should have size 1
    initialState.buffers should have size 1

    // And the pane should have the buffer associated
    val paneId = initialState.layout.editorPanes.keys.head
    val pane   = initialState.layout.editorPanes(paneId)
    pane.bufferId shouldBe defined

    val bufferId = pane.bufferId.get
    initialState.buffers should contain key bufferId

    // And focus should be on the single pane
    initialState.focus shouldBe Focus.EditorPane(paneId)
    initialState.layout.activeEditorPaneId shouldBe Some(paneId)
  }

  it should "have correct initial IDs for next buffer and pane" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]

    val logger       = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager.apply(logger).unsafeRunSync()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Next IDs should be set correctly for future creations
    initialState.nextPaneId shouldBe PaneId(1)     // Since PaneId(0) is used
    initialState.nextBufferId shouldBe BufferId(1) // Since BufferId(0) is used
  }
