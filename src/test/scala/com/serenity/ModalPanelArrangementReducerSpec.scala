package com.serenity

import com.serenity.command.{CommandIntent, ViewIntent}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, ReducerResult}
import com.serenity.ui.layout.{PanelArrangementComposition, PanelPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Arrange Panels list: arrows pick a panel, Alt+arrows move it, Enter or Space shows or hides it, and Escape
  * closes the list. A move only asks for the panel to be placed; the list follows once the layout changes.
  */
class ModalPanelArrangementReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val layout =
    DockedPanelFixtures.dockAllContent(
      AppState.initial,
      List(
        (PanelId.Outline.surfaceId, SurfaceContent.Outline(Nil), PanelPosition.Right, 20),
        (PanelId.Comments.surfaceId, SurfaceContent.Comments(Nil), PanelPosition.Right, 20)
      )
    )

  private def opened(arrangement: PanelArrangement): AppState =
    layout.copy(
      persisted = layout.persisted.copy(focus = Focus.Surface(SurfaceId("arrange"))),
      runtime = layout.runtime.copy(uiSurfaces =
        layout.runtime.uiSurfaces :+ UiSurface(
          SurfaceId("arrange"),
          SurfaceContent.ModalWorkflow(Modal.PanelArrangement(arrangement)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

  private val atOutline = opened(PanelArrangement.of(layout))

  private def reduce(event: ModalInputEvent, state: AppState = atOutline): ReducerResult =
    ModalEventReducer.reduce(ModalType.PanelArrangement, event, state)

  private def shown(state: AppState): Option[PanelArrangement] =
    state.modalSurface.map(_.content).collect {
      case SurfaceContent.ModalWorkflow(Modal.PanelArrangement(arrangement)) => arrangement
    }

  private def placements(result: ReducerResult): List[ViewIntent] =
    result.effects.collect { case AppEffect.ExecuteCommand(command) => command.intent }.collect {
      case CommandIntent.View(intent) => intent
    }

  "The arrangement list" should "move its selection with the arrow keys, without moving anything" in {
    val result = reduce(ModalNavigate(Direction.Down))

    shown(result.state).flatMap(_.selected) shouldBe Some(PanelId.Comments)
    result.effects shouldBe Nil
  }

  it should "ask for the selected panel to be placed one step later when moved down" in {
    placements(reduce(ModalMove(Direction.Down))) shouldBe
      List(ViewIntent.PlacePanel(PanelId.Outline, Some(PanelPosition.Right), 1))
    shown(reduce(ModalMove(Direction.Down)).state).flatMap(_.selected) shouldBe Some(PanelId.Outline)
  }

  it should "show or hide the selected panel with Enter or Space" in {
    placements(reduce(ModalSubmit)) shouldBe List(ViewIntent.PlacePanel(PanelId.Outline, None, 0))
    placements(reduce(ModalInsertChar(' '))) shouldBe List(ViewIntent.PlacePanel(PanelId.Outline, None, 0))
  }

  it should "do nothing for a move with nowhere to go" in {
    val topped = DockedPanelFixtures.dock(
      AppState.initial,
      PanelId.Outline.surfaceId,
      SurfaceContent.Outline(Nil),
      PanelPosition.Top,
      10
    )
    val state = opened(PanelArrangement.of(topped))

    reduce(ModalMove(Direction.Up), state).effects shouldBe Nil
  }

  it should "select the panel a click lands on" in {
    val actionId = PanelArrangementComposition.rowActionId(PanelId.Comments).value

    shown(reduce(ModalClick(actionId, Some(actionId))).state).flatMap(_.selected) shouldBe Some(PanelId.Comments)
  }

  it should "close on Escape" in {
    shown(reduce(ModalDismiss).state) shouldBe None
  }

  it should "select its first or last panel with Home, End, Ctrl+Home and Ctrl+End" in {
    val rows = PanelArrangement.of(layout).rows

    shown(reduce(ModalLineEnd).state).flatMap(_.selected) shouldBe rows.lastOption
    shown(reduce(ModalLast).state).flatMap(_.selected) shouldBe rows.lastOption
    shown(reduce(ModalLineStart, reduce(ModalLast).state).state).flatMap(_.selected) shouldBe rows.headOption
    shown(reduce(ModalFirst, reduce(ModalLast).state).state).flatMap(_.selected) shouldBe rows.headOption
    reduce(ModalLast).effects shouldBe Nil
  }

  "Alt+Up and Alt+Down" should "move the selected panel while the list has focus, not move focus" in {
    val translator = com.serenity.input.FocusedInputTranslator.forState(atOutline)
    def altKey(key: com.serenity.keystroke.InputKey) =
      com.serenity.keystroke.KeyStrokeInfo(key, None, Set(com.serenity.keystroke.Modifier.Alt))

    translator.translate(altKey(com.serenity.keystroke.InputKey.ArrowUp)) shouldBe ModalMove(Direction.Up)
    translator.translate(altKey(com.serenity.keystroke.InputKey.ArrowDown)) shouldBe ModalMove(Direction.Down)
  }

  "An open list" should "follow the layout when a panel moves" in {
    val moved = DockedPanelFixtures.dock(
      atOutline,
      PanelId.Diagnostics.surfaceId,
      SurfaceContent.Diagnostics(Nil),
      PanelPosition.Bottom,
      10
    )

    shown(PanelArrangement.resyncedIn(moved)).map(_.panelsIn(ArrangementSection.Bottom)) shouldBe
      Some(Vector(PanelId.Diagnostics))
  }

  "The list" should "show each edge's panels under its heading, the hidden panels last, and its keys" in {
    val composition = PanelArrangementComposition.forArrangement(
      PanelArrangement.of(layout),
      com.serenity.ui.layout.LayoutRect(0, 0, 50, 24),
      com.serenity.config.ModalKeyAction.defaultBindings
    )
    val texts = composition.paintBoxes.flatMap(_.text)

    val hiddenLabels = PanelArrangement
      .of(layout)
      .panelsIn(ArrangementSection.Hidden)
      .toList
      .map(id => s"  ${PanelRegistry.registrationFor(id).label}")
    texts.dropRight(1) shouldBe List(
      "Arrange Panels",
      "Top",
      "  (none)",
      "Left",
      "  (none)",
      "Right",
      "  Outline",
      "  Comments",
      "Bottom",
      "  (none)",
      "Hidden"
    ) ++ hiddenLabels
    texts.lastOption shouldBe Some("alt+up/alt+down move · enter show/hide · esc close")
    composition.paintBoxes.filter(_.selected).flatMap(_.text) shouldBe List("  Outline")
    composition.hitRegions.flatMap(_.actionId).map(_.value) should contain(
      PanelArrangementComposition.rowActionId(PanelId.Comments).value
    )
  }
