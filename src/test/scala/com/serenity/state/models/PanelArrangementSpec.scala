package com.serenity.state.models

import com.serenity.DockedPanelFixtures
import com.serenity.config.AppMode
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Arrange Panels list reads each edge's panels in the order they sit on screen, then the hidden ones, and turns a
  * move of the selected panel into where it should go next.
  */
class PanelArrangementSpec extends AnyFlatSpec with Matchers:

  import ArrangementSection.*

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def docked(state: AppState, id: PanelId, content: SurfaceContent, position: PanelPosition): AppState =
    DockedPanelFixtures.dock(state, id.surfaceId, content, position, 20)

  private def inMode(mode: AppMode): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(config = AppState.initial.persisted.config.withAppMode(mode))
    )

  private val codeState = inMode(AppMode.Code)

  /** Outline then Comments on the right, Diagnostics at the bottom. */
  private val arranged =
    val withRight = docked(
      docked(codeState, PanelId.Outline, SurfaceContent.Outline(Nil), PanelPosition.Right),
      PanelId.Comments,
      SurfaceContent.Comments(Nil),
      PanelPosition.Right
    )
    docked(withRight, PanelId.Diagnostics, SurfaceContent.Diagnostics(Nil), PanelPosition.Bottom)

  private def selecting(state: AppState, id: PanelId): PanelArrangement =
    PanelArrangement.of(state).selecting(id)

  "An arrangement" should "list each edge's panels in screen order, then the hidden panels the mode offers" in {
    val arrangement = PanelArrangement.of(arranged)

    arrangement.panelsIn(Top) shouldBe Vector.empty
    arrangement.panelsIn(Right) shouldBe Vector(PanelId.Outline, PanelId.Comments)
    arrangement.panelsIn(Bottom) shouldBe Vector(PanelId.Diagnostics)
    arrangement.panelsIn(Hidden) should contain allOf (PanelId.Explorer, PanelId.ProjectOutput)
    arrangement.panelsIn(Hidden) should not contain PanelId.Outline
  }

  it should "leave out hidden panels the current mode has no use for" in {
    val prose = PanelArrangement.of(inMode(AppMode.Prose)).panelsIn(Hidden)

    prose should contain allOf (PanelId.Outline, PanelId.Companion)
    prose should contain noneOf (PanelId.Diagnostics, PanelId.ProjectOutput)
  }

  it should "leave out the docked Markdown preview in the terminal, which opens it in a window instead" in {
    val tui =
      codeState.copy(runtime = codeState.runtime.copy(capabilities = com.serenity.frontend.FrontendCapabilities.tui()))

    PanelArrangement.of(tui).panelsIn(Hidden) should not contain PanelId.MarkdownPreview
    PanelArrangement.of(codeState).panelsIn(Hidden) should contain(PanelId.MarkdownPreview)
  }

  it should "start on the first panel, and keep a selected panel selected wherever it moves" in {
    PanelArrangement.of(arranged).selected shouldBe Some(PanelId.Outline)
    PanelArrangement.of(arranged).selecting(PanelId.Comments).resynced(codeState).selected shouldBe
      Some(PanelId.Comments)
  }

  it should "move the selection through every panel in reading order, wrapping at the ends" in {
    val rows = PanelArrangement.of(arranged).rows
    rows.take(3) shouldBe Vector(PanelId.Outline, PanelId.Comments, PanelId.Diagnostics)

    val last = PanelArrangement.of(arranged).selecting(rows.last)
    last.selectionMoved(1).selected shouldBe rows.headOption
    PanelArrangement.of(arranged).selectionMoved(-1).selected shouldBe rows.lastOption
  }

  "Moving a panel" should "swap it with its neighbour on the same edge" in {
    selecting(arranged, PanelId.Comments).moved(-1) shouldBe
      Some(PanelPlacement(PanelId.Comments, Some(PanelPosition.Right), 0))
    selecting(arranged, PanelId.Outline).moved(1) shouldBe
      Some(PanelPlacement(PanelId.Outline, Some(PanelPosition.Right), 1))
  }

  it should "cross into the end of the edge before it, and the start of the edge after it" in {
    selecting(arranged, PanelId.Outline).moved(-1) shouldBe
      Some(PanelPlacement(PanelId.Outline, Some(PanelPosition.Left), 0))
    selecting(arranged, PanelId.Comments).moved(1) shouldBe
      Some(PanelPlacement(PanelId.Comments, Some(PanelPosition.Bottom), 0))
  }

  it should "hide a panel moved past the last edge, and show a hidden one moved back up at the end of the bottom edge" in {
    selecting(arranged, PanelId.Diagnostics).moved(1) shouldBe Some(PanelPlacement(PanelId.Diagnostics, None, 0))
    selecting(arranged, PanelId.Explorer).moved(-1) shouldBe
      Some(PanelPlacement(PanelId.Explorer, Some(PanelPosition.Bottom), 1))
  }

  it should "go nowhere from the very top, or further down from the hidden panels" in {
    val topped = docked(codeState, PanelId.Outline, SurfaceContent.Outline(Nil), PanelPosition.Top)
    selecting(topped, PanelId.Outline).moved(-1) shouldBe None
    selecting(arranged, PanelId.Explorer).moved(1) shouldBe None
  }

  "Showing or hiding the selected panel" should "hide a shown one, and show a hidden one at its default edge" in {
    selecting(arranged, PanelId.Outline).toggled shouldBe Some(PanelPlacement(PanelId.Outline, None, 0))

    val defaultEdge = PanelRegistry.registrationFor(PanelId.Explorer).defaultPosition
    val atEnd       = PanelArrangement.of(arranged).panelsIn(ArrangementSection.of(defaultEdge)).size
    selecting(arranged, PanelId.Explorer).toggled shouldBe
      Some(PanelPlacement(PanelId.Explorer, Some(defaultEdge), atEnd))
  }
