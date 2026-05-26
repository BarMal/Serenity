package com.serenity

import java.nio.file.Paths

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.events.{InsertChar, UnhandledEvent}
import com.serenity.keystroke.translators.Translator
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, PinnedPanelComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId = PaneId(0)
  private object NoopTranslator extends Translator[com.serenity.keystroke.events.Event]:
    val converters = List.empty

  private def baseState: AppState =
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello")
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId)
    )

  "PinnedPanelComponent" should "treat pinned ui surfaces as the live source of truth" in {
    val surface = UiSurface.fromPanelContent(
      SurfaceId("left-panel"),
      PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None),
      PanelPosition.Left,
      24
    )
    val state = baseState.copy(
      uiSurfaces = List(surface),
      focus = Focus.Surface(surface.id)
    )

    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(InsertChar('x'), state) shouldBe ComponentResult.FocusTransfer(Focus.EditorPane(paneId))
  }

  it should "ignore input when no pinned surface exists at the requested position" in {
    val component = PinnedPanelComponent(PanelPosition.Right)
    val event = UnhandledEvent(
      new com.googlecode.lanterna.input.KeyStroke(KeyType.Enter),
      NoopTranslator
    )

    component.processEvent(event, baseState) shouldBe ComponentResult.NoChange
  }
end PinnedPanelComponentSpec
