package com.serenity.ui.layout

import com.serenity.TestWorkspaceTrees
import com.serenity.config.{AppConfig, AppMode, Gutter, TextAreaInsets}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, EditorPane, Focus, PaneId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The text area is the one calculation of how a workspace's width is shared between the gutters (the spacers the
  * insets ask for, and the prose column's centring), the line-number blocks and the text itself. These specs pin it
  * against the layout the engine produces today, so taking it out of the engine changes nothing.
  */
class TextAreaSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val noInsets = TextAreaInsets()

  "The horizontal text area" should "give the text the whole width when nothing narrows it" in {
    TextArea.horizontal(100, noInsets, leftBlock = 0, rightBlock = 0, columnCells = None) shouldBe
      TextArea(leftSpacer = 0, rightSpacer = 0, width = 100)
  }

  it should "take each inset as a share of the whole width, rounded down" in {
    val area = TextArea.horizontal(101, TextAreaInsets.percent(left = 0.1, right = 0.2), 0, 0, None)

    area shouldBe TextArea(leftSpacer = 10, rightSpacer = 20, width = 71)
  }

  it should "take the line-number blocks out of the text, not out of the spacers" in {
    val area =
      TextArea.horizontal(100, TextAreaInsets.percent(left = 0.1, right = 0.1), leftBlock = 6, rightBlock = 4, None)

    area shouldBe TextArea(leftSpacer = 10, rightSpacer = 10, width = 70)
  }

  it should "centre a column narrower than the room the blocks leave, splitting the spare width" in {
    val area = TextArea.horizontal(100, noInsets, leftBlock = 0, rightBlock = 0, columnCells = Some(60))

    area shouldBe TextArea(leftSpacer = 20, rightSpacer = 20, width = 60)
  }

  it should "measure the column's spare width after the blocks, giving the odd cell to the right" in {
    val area = TextArea.horizontal(100, noInsets, leftBlock = 5, rightBlock = 0, columnCells = Some(60))

    // 100 - 5 = 95 available, 35 spare: 17 left, 18 right.
    area shouldBe TextArea(leftSpacer = 17, rightSpacer = 18, width = 60)
  }

  it should "never narrow a gutter below the inset it was asked for" in {
    val area = TextArea.horizontal(100, TextAreaInsets.percent(left = 0.3, right = 0.0), 0, 0, columnCells = Some(60))

    area.leftSpacer shouldBe 30
    area.rightSpacer shouldBe 20
    area.width shouldBe 50
  }

  it should "leave a column wider than the room as wide as the room" in {
    TextArea.horizontal(50, noInsets, 0, 0, columnCells = Some(60)) shouldBe
      TextArea(leftSpacer = 0, rightSpacer = 0, width = 50)
  }

  it should "keep at least one cell of text however much is taken from it" in {
    TextArea
      .horizontal(10, TextAreaInsets.percent(left = 0.45, right = 0.45), leftBlock = 8, rightBlock = 8, None)
      .width shouldBe
      1
  }

  "A gutter in cells" should "take that many cells from its side, whatever the width" in {
    val insets = TextAreaInsets(Gutter.Cells(12), Gutter.Cells(5), Gutter.Percent(0.0), Gutter.Percent(0.0))

    TextArea.horizontal(100, insets, 0, 0, None) shouldBe TextArea(leftSpacer = 12, rightSpacer = 5, width = 83)
    TextArea.horizontal(40, insets, 0, 0, None) shouldBe TextArea(leftSpacer = 12, rightSpacer = 5, width = 23)
  }

  it should "come out of the width the line-number blocks leave, not the whole" in {
    val insets = TextAreaInsets(Gutter.Cells(12), Gutter.Percent(0.0), Gutter.Percent(0.0), Gutter.Percent(0.0))

    TextArea.horizontal(100, insets, leftBlock = 6, rightBlock = 4, None).width shouldBe 78
  }

  "A gutter in ch" should "be that many of the prose font's zero advance, in cells, rounded up" in {
    val insets = TextAreaInsets(Gutter.Ch(10), Gutter.Ch(10), Gutter.Percent(0.0), Gutter.Percent(0.0))

    // The Inter subset at 14 pt: a 8.832 px zero over a 8-px grid cell is 1.104 cells.
    TextArea.horizontal(100, insets, 0, 0, None, chInCells = 8.832 / 8.0) shouldBe
      TextArea(leftSpacer = 12, rightSpacer = 12, width = 76)
  }

  it should "equal cells in a terminal, where the advance is one cell" in {
    val insets = TextAreaInsets(Gutter.Ch(10), Gutter.Cells(10), Gutter.Percent(0.0), Gutter.Percent(0.0))
    val area   = TextArea.horizontal(100, insets, 0, 0, None)

    area.leftSpacer shouldBe area.rightSpacer
  }

  "An auto gutter" should "be nothing when there is no wrap point to leave spare width" in {
    val insets = TextAreaInsets(Gutter.Auto, Gutter.Auto, Gutter.Percent(0.0), Gutter.Percent(0.0))

    TextArea.horizontal(100, insets, 0, 0, None) shouldBe TextArea(leftSpacer = 0, rightSpacer = 0, width = 100)
  }

  it should "centre the column when both sides are auto, the odd cell going right" in {
    val insets = TextAreaInsets(Gutter.Auto, Gutter.Auto, Gutter.Percent(0.0), Gutter.Percent(0.0))

    TextArea.horizontal(100, insets, 0, 0, Some(60)) shouldBe TextArea(20, 20, 60)
    TextArea.horizontal(101, insets, 0, 0, Some(60)) shouldBe TextArea(20, 21, 60)
  }

  it should "take all that the column and the other gutter leave" in {
    val leftAuto  = TextAreaInsets(Gutter.Auto, Gutter.Percent(0.1), Gutter.Percent(0.0), Gutter.Percent(0.0))
    val rightAuto = TextAreaInsets(Gutter.Cells(10), Gutter.Auto, Gutter.Percent(0.0), Gutter.Percent(0.0))

    TextArea.horizontal(100, leftAuto, 0, 0, Some(60)) shouldBe TextArea(leftSpacer = 30, rightSpacer = 10, width = 60)
    TextArea.horizontal(100, rightAuto, 0, 0, Some(60)) shouldBe TextArea(leftSpacer = 10, rightSpacer = 30, width = 60)
  }

  it should "measure the spare width after the line-number blocks and the other gutter" in {
    val insets = TextAreaInsets(Gutter.Auto, Gutter.Cells(10), Gutter.Percent(0.0), Gutter.Percent(0.0))

    TextArea.horizontal(100, insets, leftBlock = 6, rightBlock = 4, Some(60)) shouldBe
      TextArea(leftSpacer = 20, rightSpacer = 10, width = 60)
  }

  it should "leave a column wider than the room as wide as the room, with the auto side at nothing" in {
    val insets = TextAreaInsets(Gutter.Auto, Gutter.Cells(10), Gutter.Percent(0.0), Gutter.Percent(0.0))

    TextArea.horizontal(50, insets, 0, 0, Some(60)) shouldBe TextArea(leftSpacer = 0, rightSpacer = 10, width = 40)
  }

  "A gutter that is not auto" should "keep the rule it had: at least its own width, with a column centred in the rest" in {
    val insets = TextAreaInsets(Gutter.Cells(30), Gutter.Percent(0.0), Gutter.Percent(0.0), Gutter.Percent(0.0))

    TextArea.horizontal(100, insets, 0, 0, Some(60)) shouldBe TextArea(leftSpacer = 30, rightSpacer = 20, width = 50)
  }

  "The vertical gutters" should "take cells as well as shares" in {
    TextArea.spacerHeights(
      40,
      TextAreaInsets(Gutter.Percent(0.0), Gutter.Percent(0.0), Gutter.Cells(3), Gutter.Cells(2))
    ) shouldBe
      (3, 2)
  }

  "The vertical text area" should "take each inset as a share of the height, rounded down" in {
    TextArea.spacerHeights(101, TextAreaInsets.percent(top = 0.1, bottom = 0.2)) shouldBe (10, 20)
  }

  it should "have no spacers when nothing narrows it" in {
    TextArea.spacerHeights(40, noInsets) shouldBe (0, 0)
  }

  // The engine's own layouts, for every workspace the text area is meant to describe.

  private def terminalState(config: AppConfig): AppState =
    val initial = AppState.initial(config)
    initial.copy(runtime = initial.runtime.copy(capabilities = FrontendCapabilities.tui()))

  private val settings: List[(String, AppConfig)] =
    List(
      "code, no gutters, no line numbers" -> AppConfig.default.withAppMode(AppMode.Code).withLineNumbers(false),
      "code with line numbers"            -> AppConfig.default.withAppMode(AppMode.Code).withLineNumbers(true),
      "code with insets" -> AppConfig.default
        .withLineNumbers(false)
        .withTextAreaInsets(TextAreaInsets.percent(left = 0.1, right = 0.15, top = 0.05, bottom = 0.1)),
      "code with insets and line numbers" -> AppConfig.default
        .withLineNumbers(true)
        .withTextAreaInsets(TextAreaInsets.percent(left = 0.1, right = 0.15, top = 0.05, bottom = 0.1)),
      "prose with a measure" -> AppConfig.default
        .withAppMode(AppMode.Prose)
        .withLineNumbers(false)
        .withProseMeasure(Some(60)),
      "prose with a measure and line numbers" -> AppConfig.default
        .withAppMode(AppMode.Prose)
        .withLineNumbers(true)
        .withProseMeasure(Some(60)),
      "prose with a measure and insets" -> AppConfig.default
        .withAppMode(AppMode.Prose)
        .withLineNumbers(false)
        .withProseMeasure(Some(60))
        .withTextAreaInsets(TextAreaInsets.percent(left = 0.3, right = 0.0)),
      "code with gutters in cells" -> AppConfig.default
        .withLineNumbers(false)
        .withTextAreaInsets(TextAreaInsets(Gutter.Cells(8), Gutter.Cells(6), Gutter.Cells(1), Gutter.Cells(2))),
      "code with gutters in ch" -> AppConfig.default
        .withLineNumbers(true)
        .withTextAreaInsets(TextAreaInsets(Gutter.Ch(8), Gutter.Ch(6), Gutter.Percent(0.0), Gutter.Percent(0.0))),
      "prose with auto gutters" -> AppConfig.default
        .withAppMode(AppMode.Prose)
        .withLineNumbers(false)
        .withProseMeasure(Some(60))
        .withTextAreaInsets(TextAreaInsets(Gutter.Auto, Gutter.Auto, Gutter.Percent(0.0), Gutter.Percent(0.0))),
      "prose with an auto and a cell gutter" -> AppConfig.default
        .withAppMode(AppMode.Prose)
        .withLineNumbers(true)
        .withProseMeasure(Some(60))
        .withTextAreaInsets(TextAreaInsets(Gutter.Cells(10), Gutter.Auto, Gutter.Percent(0.0), Gutter.Percent(0.0))),
      "column mode, as many 80-cell columns as fit" -> AppConfig.default
        .withLineNumbers(true)
        .withColumnMode(true)
        .withColumnTargetWidth(80)
    )

  settings.foreach { (name, config) =>
    List(60, 101, 161, 240).foreach { width =>
      it should s"describe the layout of a terminal $width cells wide: $name" in {
        val state      = terminalState(config)
        val viewport   = ViewportSize(width, 40)
        val layout     = LayoutEngine.calculateLayout(state, viewport)
        val insets     = config.surfaceConfig.textAreaInsets.normalized
        val leftBlock  = layout.editorPanelRect.x - layout.leftSpacerRect.right
        val rightBlock = layout.rightSpacerRect.x - layout.editorPanelRect.right
        val area =
          TextArea.horizontal(
            layout.leftSpacerRect.width + leftBlock + layout.editorPanelRect.width + rightBlock +
              layout.rightSpacerRect.width,
            insets,
            leftBlock,
            rightBlock,
            ProseColumn.widthCells(state)
          )

        area shouldBe TextArea(
          leftSpacer = layout.leftSpacerRect.width,
          rightSpacer = layout.rightSpacerRect.width,
          width = layout.editorPanelRect.width
        )
      }
    }
  }

  it should "keep a gutter given in cells that wide in a terminal, however wide the window" in {
    val config =
      AppConfig.default
        .withLineNumbers(false)
        .withTextAreaInsets(TextAreaInsets(Gutter.Cells(8), Gutter.Cells(6), Gutter.Percent(0.0), Gutter.Percent(0.0)))

    List(60, 101, 240).foreach { width =>
      val layout = LayoutEngine.calculateLayout(terminalState(config), ViewportSize(width, 40))

      (layout.leftSpacerRect.width, layout.rightSpacerRect.width, layout.editorPanelRect.width) shouldBe
        (8, 6, width - 14)
    }
  }

  it should "take a gutter given in ch as that many cells in a terminal" in {
    val config =
      AppConfig.default
        .withLineNumbers(false)
        .withTextAreaInsets(TextAreaInsets(Gutter.Ch(8), Gutter.Percent(0.0), Gutter.Percent(0.0), Gutter.Percent(0.0)))

    LayoutEngine.calculateLayout(terminalState(config), ViewportSize(100, 40)).leftSpacerRect.width shouldBe 8
  }

  it should "give a pane's top and bottom spacers the heights the engine gives them" in {
    val config =
      AppConfig.default.withLineNumbers(false).withTextAreaInsets(TextAreaInsets.percent(top = 0.05, bottom = 0.1))
    val state  = terminalState(config)
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(120, 41))
    val pane   = EditorPaneLayoutEngine.calculateEditorPaneLayouts(state, layout).values.head
    val content =
      pane.paneRect.height - pane.headerRect.height

    TextArea.spacerHeights(content, config.surfaceConfig.textAreaInsets.normalized) shouldBe
      (pane.topSpacerRect.height, pane.bottomSpacerRect.height)
  }

  it should "share one set of horizontal gutters between split panes, and give each its own vertical ones" in {
    val config =
      AppConfig.default
        .withLineNumbers(true)
        .withTextAreaInsets(TextAreaInsets.percent(left = 0.1, right = 0.15, top = 0.05, bottom = 0.1))
    val paneIds = List(PaneId(0), PaneId(1), PaneId(2))
    val base    = terminalState(config)
    val state = base.copy(persisted =
      base.persisted.copy(
        layout = Layout(
          editorPanes = paneIds.map(id => id -> EditorPane.empty(id)).toMap,
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneIds*))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(160, 41))
    val panes  = EditorPaneLayoutEngine.calculateEditorPaneLayouts(state, layout)
    val insets = config.surfaceConfig.textAreaInsets.normalized
    val blocks =
      (layout.editorPanelRect.x - layout.leftSpacerRect.right, layout.rightSpacerRect.x - layout.editorPanelRect.right)
    val area =
      TextArea.horizontal(
        layout.leftSpacerRect.width + blocks._1 + layout.editorPanelRect.width + blocks._2 +
          layout.rightSpacerRect.width,
        insets,
        blocks._1,
        blocks._2,
        None
      )

    layout.leftSpacerRect.width shouldBe area.leftSpacer
    layout.rightSpacerRect.width shouldBe area.rightSpacer
    layout.editorPanelRect.width shouldBe area.width
    panes should have size 3
    panes.values.foreach { pane =>
      TextArea.spacerHeights(pane.paneRect.height - pane.headerRect.height, insets) shouldBe
        (pane.topSpacerRect.height, pane.bottomSpacerRect.height)
    }
  }
