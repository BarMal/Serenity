package com.serenity.state.manager

import com.serenity.config.{AppConfig, ChromeElement}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Summoned chrome changes only `runtime.summons`, so the layout key and the scene cached under it must follow it. */
class SummonedChromeKeySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.default
      )
    )

  private def withSecondBuffer(state: AppState): AppState =
    val second = Buffer.fromString(BufferId(2), "second")
    state.copy(persisted =
      state.persisted.copy(
        buffers = state.persisted.buffers + (second.id -> second),
        bufferOrder = state.persisted.bufferOrder :+ second.id
      )
    )

  private def withSummons(state: AppState, summons: ChromeSummons): AppState =
    state.copy(runtime = state.runtime.copy(summons = summons))

  private val summonedKeyFields: List[(ChromeElement, MouseTargetLayoutKey => Boolean)] = List(
    ChromeElement.LineNumbers -> (_.showLineNumbers),
    ChromeElement.StatusBar   -> (_.statusLine.isShown),
    ChromeElement.TabStrip    -> (_.tabStripShown),
    ChromeElement.Scrollbar   -> (_.scrollbarShown),
    ChromeElement.PaneHeaders -> (_.showPaneHeaders)
  )

  summonedKeyFields.foreach { (element, shownIn) =>
    "The layout key" should s"show $element in the key and rebuild the scene once it is held, with nothing else changed" in {
      val hidden = withSecondBuffer(stateWith(Buffer.fromString(bufferId, "alpha\nbeta")))
      val held   = withSummons(hidden, hidden.runtime.summons.pressed(element))
      val size   = ViewportSize(80, 24)
      val scene  = AuthoritativeUiScene()

      val before = MouseTargetCache.fromState(hidden, size, scene)
      val after  = MouseTargetCache.fromState(held, size, scene)

      shownIn(scene.layoutKeyFor(hidden, size)) shouldBe false
      shownIn(scene.layoutKeyFor(held, size)) shouldBe true
      after.scene should not be theSameInstanceAs(before.scene)
    }

    "The layout key" should s"show a locked $element on the first scene built after a state without it" in {
      val hidden = withSecondBuffer(stateWith(Buffer.fromString(bufferId, "alpha\nbeta")))
      val locked = withSummons(hidden, ChromeSummons(locked = Set(element)))
      val size   = ViewportSize(80, 24)
      val scene  = AuthoritativeUiScene()

      scene.layoutKeyFor(hidden, size)

      shownIn(scene.layoutKeyFor(locked, size)) shouldBe true
    }
  }
