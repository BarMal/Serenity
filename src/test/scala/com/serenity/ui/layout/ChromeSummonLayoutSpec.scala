package com.serenity.ui.layout

import com.serenity.config.{AppConfig, ChromeElement}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The layout reserves room for summoned chrome and gives it back, so what the renderer paints is always where the
  * layout put it.
  */
class ChromeSummonLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)

  private def twoBuffers(config: AppConfig): AppState =
    val base   = AppState.initial(config)
    val second = Buffer.fromString(BufferId(1), "second")
    base.copy(persisted =
      base.persisted.copy(
        buffers = base.persisted.buffers + (second.id -> second),
        bufferOrder = base.persisted.bufferOrder :+ second.id
      )
    )

  private def summoned(state: AppState, summons: ChromeSummons): AppState =
    state.copy(runtime = state.runtime.copy(summons = summons))

  private val minimal = twoBuffers(AppConfig.default)

  private def layoutOf(state: AppState) = LayoutEngine.calculateLayout(state, viewport)

  private def bigEnough(state: AppState) = ScrollbarLayout.shown(state, LayoutRect(0, 0, 60, 20))

  "A minimal layout" should "reserve no chrome until some is summoned" in {
    val layout = layoutOf(minimal)

    layout.lineNumberRect shouldBe None
    layout.gutterRect shouldBe None
    layout.tabBarRect shouldBe None
    LayoutEngine.paneHeaderHeight(minimal) shouldBe 0
    bigEnough(minimal) shouldBe false
  }

  "Summoning line numbers" should "reserve their rail, and only that" in {
    val state  = summoned(minimal, ChromeSummons.empty.pressed(ChromeElement.LineNumbers))
    val layout = layoutOf(state)

    layout.lineNumberRect should not be None
    layout.gutterRect shouldBe None
    layout.tabBarRect shouldBe None
    LayoutEngine.paneHeaderHeight(state) shouldBe 0
    layout.editorPanelRect.x should be > layoutOf(minimal).editorPanelRect.x
  }

  "Summoning the status bar" should "reserve the pinned gutter row at the bottom" in {
    val state  = summoned(minimal, ChromeSummons.empty.pressed(ChromeElement.StatusBar))
    val layout = layoutOf(state)

    layout.gutterRect should not be None
    layout.editorPanelRect.height should be < layoutOf(minimal).editorPanelRect.height
    layout.lineNumberRect shouldBe None
  }

  "Summoning the tab strip" should "reserve its row at the top once two files are open" in {
    val state = summoned(minimal, ChromeSummons.empty.pressed(ChromeElement.TabStrip))

    layoutOf(state).tabBarRect shouldBe Some(LayoutRect(0, 0, viewport.width, 1))
    state.tabBarSurface should not be None
  }

  it should "show nothing with a single file open, as the configured strip does" in {
    val single = summoned(AppState.initial(AppConfig.default), ChromeSummons.empty.pressed(ChromeElement.TabStrip))

    layoutOf(single).tabBarRect shouldBe None
    single.tabBarSurface shouldBe None
  }

  "Summoning pane headers" should "reserve the header row" in {
    val state = summoned(minimal, ChromeSummons.empty.pressed(ChromeElement.PaneHeaders))

    LayoutEngine.paneHeaderHeight(state) shouldBe LayoutEngine.EditorPaneHeaderHeight
    layoutOf(state).lineNumberRect shouldBe None
  }

  "Summoning the scrollbar" should "show it on a pane wide enough to carry one" in {
    val state = summoned(minimal, ChromeSummons.empty.pressed(ChromeElement.Scrollbar))

    bigEnough(state) shouldBe true
    ScrollbarLayout.shown(state, LayoutRect(0, 0, 8, 20)) shouldBe false
  }

  "Releasing the chord" should "give every row and column back" in {
    val shown = summoned(
      minimal,
      ChromeElement.values.foldLeft(ChromeSummons.empty)(_.pressed(_))
    )
    val hidden = summoned(shown, shown.runtime.summons.releasedAll)

    layoutOf(shown) should not be layoutOf(minimal)
    layoutOf(hidden) shouldBe layoutOf(minimal)
    bigEnough(hidden) shouldBe false
  }

  "A locked element" should "stay reserved after release" in {
    val locked = ChromeSummons.empty.pressed(ChromeElement.LineNumbers).lockHeld.released(ChromeElement.LineNumbers)
    val state  = summoned(minimal, locked)

    layoutOf(state).lineNumberRect should not be None
  }

  "Summoning what the config already shows" should "change nothing" in {
    val conventional = twoBuffers(AppConfig.conventional)
    val everything   = summoned(conventional, ChromeElement.values.foldLeft(ChromeSummons.empty)(_.pressed(_)))

    layoutOf(everything) shouldBe layoutOf(conventional)
  }

  "Summoning" should "never change the config" in {
    val state = summoned(minimal, ChromeElement.values.foldLeft(ChromeSummons.empty)(_.pressed(_)).lockHeld)

    state.persisted.config shouldBe minimal.persisted.config
  }
