package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.NewTab
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class TabCreationBehaviorSpec extends AnyFlatSpec with Matchers:

  behavior of "Tab creation behavior"

  it should "create a new buffer without adding panes in a narrow viewport" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]
    val logger                      = LoggerFactory[IO].getLogger(using LoggerName("TabCreationBehaviorSpec"))
    val stateManager                = StateManager.apply(logger).unsafeRunSync()

    stateManager.updateState(_.copy(viewportSize = Some(ViewportSize(80, 24)))).unsafeRunSync()

    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val initialPanes = initialState.layout.editorPanes

    stateManager.applyEvent(NewTab).unsafeRunSync()
    val stateAfterNewTab = stateManager.getCurrentState.unsafeRunSync()

    val currentBufferId = BufferId(0)
    val nextBuffer      = stateAfterNewTab.nextBufferInOrder(currentBufferId)
    val prevBuffer      = stateAfterNewTab.previousBufferInOrder(currentBufferId)
    val focusedBufferId = stateAfterNewTab.focusedBufferId.getOrElse(fail("expected a focused buffer"))

    stateAfterNewTab.buffers.size shouldBe initialState.buffers.size + 1
    stateAfterNewTab.bufferOrder should contain theSameElementsAs stateAfterNewTab.buffers.keys
    stateAfterNewTab.layout.editorPanes.keySet shouldBe initialPanes.keySet
    stateAfterNewTab.focusedBufferId shouldBe stateAfterNewTab.bufferOrder.lastOption
    stateAfterNewTab.layout.editorPanes.values.flatMap(_.bufferId).toSet should contain(focusedBufferId)
    stateAfterNewTab.layout.activeEditorPaneId.map(paneId => Focus.EditorPane(paneId)) shouldBe Some(
      stateAfterNewTab.focus
    )
    nextBuffer shouldBe stateAfterNewTab.bufferOrder.lift(1)
    prevBuffer shouldBe stateAfterNewTab.bufferOrder.lastOption
  }
