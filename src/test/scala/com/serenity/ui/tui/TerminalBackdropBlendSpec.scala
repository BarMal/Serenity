package com.serenity.ui.tui

import java.awt.Color
import java.io.StringWriter

import com.serenity.config.{AppConfig, ConfigFileFormat, ConfigRegistry}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A terminal cannot blend, so a colour with alpha between 0 and 255 is pre-blended towards `tui.backdrop`; alpha 0
  * stays the terminal's own background (SGR 49) and alpha 255 is untouched.
  */
class TerminalBackdropBlendSpec extends AnyFlatSpec with Matchers:

  "BackdropBlend.flatten" should "leave opaque and fully transparent colours alone" in {
    val solid = Color(10, 20, 30, 255)
    BackdropBlend.flatten(solid, Color.WHITE) shouldBe solid
    val clear = Color(10, 20, 30, 0)
    BackdropBlend.flatten(clear, Color.WHITE) shouldBe clear
  }

  it should "move a translucent colour towards the backdrop in proportion to its alpha, and make it opaque" in {
    val half = BackdropBlend.flatten(Color(200, 100, 0, 128), Color.BLACK)
    half.getAlpha shouldBe 255
    half.getRed shouldBe 100 +- 1
    half.getGreen shouldBe 50 +- 1
    half.getBlue shouldBe 0
    val overWhite = BackdropBlend.flatten(Color(0, 0, 0, 128), Color.WHITE)
    overWhite.getRed shouldBe 127 +- 1
  }

  it should "give a darker, less saturated colour as alpha falls over a black backdrop" in {
    val strong = BackdropBlend.flatten(Color(200, 40, 40, 220), Color.BLACK)
    val weak   = BackdropBlend.flatten(Color(200, 40, 40, 60), Color.BLACK)
    weak.getRed should be < strong.getRed
  }

  private def drawn(background: RenderColor, backdrop: Color): TerminalEmulator =
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(4, 1, writer, CellMetrics.cellUnit, backdrop = backdrop)
    surface.setForegroundColor(RenderColor.fromRgba(255, 255, 255, 255))
    surface.setBackgroundColor(background)
    surface.putString(0, 0, "ab")
    surface.flush()
    TerminalEmulator.blank(4, 1).consume(writer.toString)

  "A TerminalRenderSurface" should "emit a translucent background as the colour blended over the backdrop" in {
    val cell = drawn(RenderColor.fromRgba(200, 100, 0, 128), Color.BLACK).cellAt(0, 0)
    cell.bg.getAlpha shouldBe 255
    cell.bg.getRed shouldBe 100 +- 1
    cell.bg.getGreen shouldBe 50 +- 1
  }

  it should "blend towards a configured backdrop rather than black" in {
    val cell = drawn(RenderColor.fromRgba(0, 0, 0, 128), Color(255, 255, 255)).cellAt(0, 0)
    cell.bg.getRed shouldBe 127 +- 1
  }

  it should "keep alpha 0 as the terminal's own background (SGR 49), never an explicit RGB" in {
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(4, 1, writer, CellMetrics.cellUnit, backdrop = Color.WHITE)
    surface.setBackgroundColor(RenderColor.fromRgba(0, 0, 0, 0))
    surface.putString(0, 0, "ab")
    surface.flush()
    writer.toString should include("49")
    TerminalEmulator.blank(4, 1).consume(writer.toString).cellAt(0, 0).bg.getAlpha shouldBe 0
  }

  "tui.backdrop" should "default to black and be read and written as a colour" in {
    AppConfig.default.tuiConfig.backdrop shouldBe Color.BLACK
    val read = ConfigRegistry.read(AppConfig.default, "tui.backdrop", "#102030")
    read.map(_.tuiConfig.backdrop) shouldBe Some(Color(0x10, 0x20, 0x30))
    ConfigFileFormat.settings(read.getOrElse(AppConfig.default)).toMap.get("tui.backdrop").map(_.rendered) shouldBe
      Some("\"#102030\"")
  }

  it should "reject text that is not a colour" in {
    ConfigRegistry.read(AppConfig.default, "tui.backdrop", "dark") shouldBe None
  }
