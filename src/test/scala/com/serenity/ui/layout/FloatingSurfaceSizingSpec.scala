package com.serenity.ui.layout

import com.serenity.command.*
import com.serenity.config.{AppConfig, InterfaceConfig, InterfaceDensity}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `FloatingSurfaceSizing` (issue #1683): the intrinsic width/height table moved out of
  * `FloatingSurfaceLayout` so that object carries no per-content-kind sizing match of its own. `FloatingSurfaceLayout`
  * delegates its own `calculateFloatingSurfaceHeight`/width to this object -- `CommandRunnerSurfaceCompositionSpec`
  * already pins that delegation down for the command palette; this spec covers the plain content kinds that have no
  * bespoke composition of their own to ask for a size instead.
  */
class FloatingSurfaceSizingSpec extends AnyFlatSpec with Matchers:

  private def stateForDensity(density: InterfaceDensity): AppState =
    AppState(
      persisted = Persisted(
        layout = Layout.empty,
        buffers = Map.empty,
        focus = Focus.Modal,
        config = AppConfig.default.copy(interfaceConfig = InterfaceConfig(density = density))
      )
    )

  private val state = stateForDensity(InterfaceDensity.Comfortable)

  "width" should "size a status line as wide as its text plus one cell of padding each side, never the whole pane" in {
    val width = FloatingSurfaceSizing.width(SurfaceContent.StatusLine("12:1"), state, LayoutRect(0, 0, 80, 10))

    width shouldBe "12:1".length + 2
  }

  it should "cap a command palette's width at 72 columns regardless of the available pane width" in {
    val runner = CommandRunner.empty
    val width  = FloatingSurfaceSizing.width(SurfaceContent.CommandPalette(runner), state, LayoutRect(0, 0, 200, 10))

    width shouldBe 72
  }

  it should "give any other content the full available width" in {
    val width = FloatingSurfaceSizing.width(SurfaceContent.QuickInfo("hi"), state, LayoutRect(0, 0, 40, 10))

    width shouldBe 40
  }

  "height" should "size to a quick-info popup's actual line count plus its border" in {
    val height = FloatingSurfaceSizing.height(SurfaceContent.QuickInfo("one\ntwo\nthree"), 40, 20, state)

    height shouldBe 5
  }

  it should "never size a status line taller than a single row" in {
    val height = FloatingSurfaceSizing.height(SurfaceContent.StatusLine("12:1"), 40, 20, state)

    height shouldBe 1
  }

  it should "fill the full available height for a start page" in {
    val height = FloatingSurfaceSizing.height(SurfaceContent.StartPage(StartupPage("Serenity")), 40, 15, state)

    height shouldBe 15
  }

  it should "never size any content below the shared floor of three rows" in {
    val height = FloatingSurfaceSizing.height(SurfaceContent.QuickInfo(""), 40, 20, state)

    height should be >= 3
  }
end FloatingSurfaceSizingSpec
