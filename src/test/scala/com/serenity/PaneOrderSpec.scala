package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{BufferId, PaneId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class PaneOrderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  trait PaneFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val sm: StateManager    = StateManager.apply(logger).unsafeRunSync()
    val pane0: PaneId       = sm.getCurrentState.unsafeRunSync().layout.activeEditorPaneId.get

  behavior of "Pane tab order"

  it should "return the initial pane in getTabOrder" in new PaneFixture:
    sm.getTabOrder().unsafeRunSync() shouldBe List(pane0)

  it should "append new panes at the end when using createPane" in new PaneFixture:
    val pane1 = sm.createPane().unsafeRunSync()
    val pane2 = sm.createPane().unsafeRunSync()
    sm.getTabOrder().unsafeRunSync() shouldBe List(pane0, pane1, pane2)

  it should "insert a new pane immediately after the specified pane" in new PaneFixture:
    val pane1     = sm.createPane().unsafeRunSync()
    val pane2     = sm.createPane().unsafeRunSync()
    val newPane   = sm.createPaneAfter(pane0).unsafeRunSync()
    sm.getTabOrder().unsafeRunSync() shouldBe List(pane0, newPane, pane1, pane2)

  it should "insert after the last pane when afterPaneId is the last pane" in new PaneFixture:
    val pane1   = sm.createPane().unsafeRunSync()
    val newPane = sm.createPaneAfter(pane1).unsafeRunSync()
    sm.getTabOrder().unsafeRunSync() shouldBe List(pane0, pane1, newPane)

  it should "insert after a non-existent paneId by appending at the end" in new PaneFixture:
    val ghost   = PaneId(999)
    val newPane = sm.createPaneAfter(ghost).unsafeRunSync()
    sm.getTabOrder().unsafeRunSync() shouldBe List(pane0, newPane)

  it should "remove a closed pane from the tab order" in new PaneFixture:
    val pane1 = sm.createPane().unsafeRunSync()
    val pane2 = sm.createPane().unsafeRunSync()
    sm.closePane(pane1).unsafeRunSync()
    sm.getTabOrder().unsafeRunSync() shouldBe List(pane0, pane2)

  it should "insert splitPaneHorizontal result immediately after the split pane" in new PaneFixture:
    val pane1    = sm.createPane().unsafeRunSync()
    val split    = sm.splitPaneHorizontal(pane0).unsafeRunSync()
    sm.getTabOrder().unsafeRunSync() shouldBe List(pane0, split, pane1)

end PaneOrderSpec
