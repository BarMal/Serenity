package com.serenity.ui.layout

import com.serenity.config.{AppConfig, AppMode, TextAreaInsets}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
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
    val area = TextArea.horizontal(101, TextAreaInsets(left = 0.1, right = 0.2), 0, 0, None)

    area shouldBe TextArea(leftSpacer = 10, rightSpacer = 20, width = 71)
  }

  it should "take the line-number blocks out of the text, not out of the spacers" in {
    val area = TextArea.horizontal(100, TextAreaInsets(left = 0.1, right = 0.1), leftBlock = 6, rightBlock = 4, None)

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
    val area = TextArea.horizontal(100, TextAreaInsets(left = 0.3, right = 0.0), 0, 0, columnCells = Some(60))

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
      .horizontal(10, TextAreaInsets(left = 0.45, right = 0.45), leftBlock = 8, rightBlock = 8, None)
      .width shouldBe
      1
  }

  "The vertical text area" should "take each inset as a share of the height, rounded down" in {
    TextArea.spacerHeights(101, TextAreaInsets(top = 0.1, bottom = 0.2)) shouldBe (10, 20)
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
        .withTextAreaInsets(TextAreaInsets(left = 0.1, right = 0.15, top = 0.05, bottom = 0.1)),
      "code with insets and line numbers" -> AppConfig.default
        .withLineNumbers(true)
        .withTextAreaInsets(TextAreaInsets(left = 0.1, right = 0.15, top = 0.05, bottom = 0.1)),
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
        .withTextAreaInsets(TextAreaInsets(left = 0.3, right = 0.0)),
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

  it should "give a pane's top and bottom spacers the heights the engine gives them" in {
    val config = AppConfig.default.withLineNumbers(false).withTextAreaInsets(TextAreaInsets(top = 0.05, bottom = 0.1))
    val state  = terminalState(config)
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(120, 41))
    val pane   = EditorPaneLayoutEngine.calculateEditorPaneLayouts(state, layout).values.head
    val content =
      pane.paneRect.height - pane.headerRect.height

    TextArea.spacerHeights(content, config.surfaceConfig.textAreaInsets.normalized) shouldBe
      (pane.topSpacerRect.height, pane.bottomSpacerRect.height)
  }
