package com.serenity.ui.layout

import com.serenity.config.{AppConfig, DockedCornerStyle, InterfaceDensity, StatusLinePlacement, TabStripMode}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The extent a docked panel's share of the window is divided from: the viewport less what the floating chrome's inset
  * takes. With inset `i`, it is `max(1, width - 2i)` wide and `height - (2i + tabGap + gutterGap)` tall, where each gap
  * is `i` when its strip (tab strip, pinned status line) is shown and 0 when it is not.
  */
class ChromeFramePanelViewportSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(80, 24)

  private def state(
    gui: Boolean = true,
    corners: DockedCornerStyle = DockedCornerStyle.Floating,
    gap: Option[Double] = Some(2.0),
    buffers: Int = 2,
    status: StatusLinePlacement = StatusLinePlacement.Pinned,
    density: InterfaceDensity = InterfaceDensity.Comfortable,
    size: Option[ViewportSize] = Some(viewport)
  ): AppState =
    val config = AppConfig.default
      .withDockedCornerStyle(corners)
      .withUiElementGap(gap)
      .withStatusLinePlacement(status)
      .withInterfaceDensity(density)
      .withTabStrip(TabStripMode.Auto)
    val base = AppState.initial(config)
    base.copy(
      persisted = base.persisted.copy(bufferOrder = List.tabulate(buffers)(index => BufferId(index + 1))),
      runtime = base.runtime.copy(
        capabilities = if gui then FrontendCapabilities.gui else FrontendCapabilities.tui(),
        viewportSize = size
      )
    )

  private def sizeOf(s: AppState): Option[ViewportSize] = ChromeFrame.panelViewportSize(s)

  "ChromeFrame.panelViewportSize" should "be the whole viewport on a cell grid, whatever the corner style asks for" in {
    val s = state(gui = false)

    sizeOf(s) shouldBe Some(ViewportSize(80, 24))
  }

  it should "be the whole viewport when the corners are flush" in {
    val s = state(corners = DockedCornerStyle.Flush)

    sizeOf(s) shouldBe Some(ViewportSize(80, 24))
  }

  it should "be the whole viewport when the element gap is zero" in {
    val s = state(gap = Some(0.0))

    sizeOf(s) shouldBe Some(ViewportSize(80, 24))
  }

  it should "take the inset off both sides, and off the top and bottom with a tab strip and a pinned status line" in {
    val s = state()

    sizeOf(s) shouldBe Some(ViewportSize(76, 16))
  }

  it should "leave out the gap under a tab strip that is not shown" in {
    val s = state(buffers = 1)

    sizeOf(s) shouldBe Some(ViewportSize(76, 18))
  }

  it should "leave out the gap above a status line that is not pinned" in {
    val off      = state(status = StatusLinePlacement.Off)
    val floating = state(status = StatusLinePlacement.Floating)

    sizeOf(off) shouldBe Some(ViewportSize(76, 18))
    sizeOf(floating) shouldBe Some(ViewportSize(76, 18))
  }

  it should "leave out both gaps with neither strip shown" in {
    val s = state(buffers = 1, status = StatusLinePlacement.Off)

    sizeOf(s) shouldBe Some(ViewportSize(76, 20))
  }

  it should "round a fractional gap up to whole cells" in {
    val s = state(gap = Some(0.5))

    sizeOf(s) shouldBe Some(ViewportSize(78, 20))
  }

  it should "take its gap from the density when none is configured" in {
    val compact  = state(gap = None, density = InterfaceDensity.Compact)
    val spacious = state(gap = None, density = InterfaceDensity.Spacious)

    sizeOf(compact) shouldBe Some(ViewportSize(78, 20))
    sizeOf(spacious) shouldBe Some(ViewportSize(76, 16))
  }

  it should "keep at least one column however wide the inset is" in {
    val s = state(size = Some(ViewportSize(3, 10)))

    sizeOf(s) shouldBe Some(ViewportSize(1, 2))
  }

  it should "be None until the viewport is known" in {
    val s = state(size = None)

    sizeOf(s) shouldBe None
  }
