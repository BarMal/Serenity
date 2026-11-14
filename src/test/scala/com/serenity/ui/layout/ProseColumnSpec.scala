package com.serenity.ui.layout

import com.serenity.TestWorkspaceTrees
import com.serenity.config.{AppConfig, AppMode, Gutter, TextAreaInsets, WrapColumn}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, EditorPane, Focus, PaneId}
import com.serenity.testkit.ActivePane
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProseColumnSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def terminalState(config: AppConfig): AppState =
    val initial = AppState.initial(config.withLineNumbers(false))
    initial.copy(runtime = initial.runtime.copy(capabilities = FrontendCapabilities.tui()))

  private def textColumnWidth(state: AppState, viewportWidth: Int): Int =
    ActivePane.textRect(state, LayoutEngine.calculateLayout(state, ViewportSize(viewportWidth, 40))).width

  "A prose measure" should "hold the text column at the same number of characters however wide the window is" in {
    val state = terminalState(AppConfig.default.withAppMode(AppMode.Prose).withWrapColumn(WrapColumn.Ch(60)))

    List(100, 161, 240).map(textColumnWidth(state, _)) shouldBe List(60, 60, 60)
  }

  it should "centre the column, splitting the spare width between the two sides" in {
    val state  = terminalState(AppConfig.default.withAppMode(AppMode.Prose).withWrapColumn(WrapColumn.Ch(60)))
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 40))

    ActivePane.of(state, layout).leftSpacerRect.width shouldBe 20
    ActivePane.textRect(state, layout).x shouldBe 20
  }

  it should "give a window narrower than the measure its whole width" in {
    val state = terminalState(AppConfig.default.withAppMode(AppMode.Prose).withWrapColumn(WrapColumn.Ch(60)))

    textColumnWidth(state, 50) shouldBe 50
  }

  it should "hold a code workspace to the same column, as the wrap column applies in every mode" in {
    val state = terminalState(AppConfig.default.withAppMode(AppMode.Code).withWrapColumn(WrapColumn.Ch(60)))

    textColumnWidth(state, 100) shouldBe 60
  }

  it should "count cells as cells and, in a terminal, ch as cells too" in {
    val cells = terminalState(AppConfig.default.withWrapColumn(WrapColumn.Cells(60)))
    val ch    = terminalState(AppConfig.default.withWrapColumn(WrapColumn.Ch(60)))

    textColumnWidth(cells, 100) shouldBe textColumnWidth(ch, 100)
  }

  it should "still cap the text area when word wrap is off" in {
    val state = terminalState(AppConfig.default.withWordWrap(false).withWrapColumn(WrapColumn.Cells(60)))

    textColumnWidth(state, 100) shouldBe 60
  }

  it should "leave the text area to its edge when it is the edge" in {
    textColumnWidth(terminalState(AppConfig.default.withWrapColumn(WrapColumn.Edge)), 100) shouldBe 100
  }

  it should "centre in each split pane, from that pane's own width, with auto gutters" in {
    val base = terminalState(
      AppConfig.default
        .withWrapColumn(WrapColumn.Cells(30))
        .withTextAreaInsets(TextAreaInsets(Gutter.Auto, Gutter.Auto, Gutter.Zero, Gutter.Zero))
    )
    val panes = List(PaneId(0), PaneId(1))
    val state = base.copy(persisted =
      base.persisted.copy(
        layout = Layout(
          editorPanes = panes.map(id => id -> EditorPane.empty(id)).toMap,
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(panes*))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )
    val layouts =
      LayoutEngine.calculateEditorPaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(100, 40)))

    layouts.values.foreach { pane =>
      pane.contentRect.width shouldBe 30
      math.abs(pane.leftSpacerRect.width - pane.rightSpacerRect.width) should be <= 1
    }
  }

  it should "be one column's width in column mode, not a cap on the text area" in {
    val state = terminalState(
      AppConfig.default.withWordWrap(true).withColumnMode(true).withWrapColumn(WrapColumn.Cells(40))
    )

    ProseColumn.widthCells(state) shouldBe None
    textColumnWidth(state, 100) shouldBe 100
  }

  "Grid cells for a measure" should "be the measure in the prose font's ch advance, rounded up to whole cells" in {
    ProseColumn.cells(66, chWidthPx = 9.0, gridCellWidthPx = 10) shouldBe 60
    ProseColumn.cells(66, chWidthPx = 10.0, gridCellWidthPx = 10) shouldBe 66
    ProseColumn.cells(66, chWidthPx = 12.5, gridCellWidthPx = 10) shouldBe 83
  }
