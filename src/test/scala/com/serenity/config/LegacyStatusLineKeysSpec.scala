package com.serenity.config

import java.nio.file.Files

import com.serenity.config.{StatusLineConfig, StatusLinePlacement, StatusSegment}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A config file written before `status.*` existed reads as the same status row the user saw, and the old keys are
  * reported as deprecated with their replacement.
  */
class LegacyStatusLineKeysSpec extends AnyFlatSpec with Matchers:

  private def load(text: String): ConfigLoadResult =
    val file = Files.createTempFile("serenity-legacy-status", ".conf")
    Files.writeString(file, text)
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  "the old gutter defaults" should "read as today's default pinned row" in {
    val result = load("display.gutter = true\ncursor.info_bar.segments = off\n")

    result.config.statusLine shouldBe AppConfig.default.statusLine
    result.report.deprecatedEntries.map(entry => entry.key -> entry.replacement) should contain allOf (
      "display.gutter"           -> "status.placement",
      "cursor.info_bar.segments" -> "status.segments"
    )
  }

  "a switched-off gutter with no info bar" should "read as no status line" in {
    load("display.gutter = false\n").config.statusLine.placement shouldBe StatusLinePlacement.Off
  }

  "an explicit info bar" should "keep its segments and floating default, with the mode the corner glyph showed" in {
    val statusLine =
      load("cursor.info_bar.segments = \"position, word_count\"\ndisplay.gutter = true\n").config.statusLine

    statusLine.segments shouldBe List(StatusSegment.Position, StatusSegment.WordCount, StatusSegment.Mode)
    statusLine.placement shouldBe StatusLinePlacement.Floating
  }

  it should "keep a pinned placement" in {
    val statusLine =
      load(
        "cursor.info_bar.segments = \"position,title\"\ncursor.info_bar.placement = pinned-bottom\n"
      ).config.statusLine

    statusLine.segments shouldBe List(StatusSegment.Position, StatusSegment.Title, StatusSegment.Mode)
    statusLine.placement shouldBe StatusLinePlacement.Pinned
  }

  "the word-count toggle" should "append its three segments to whichever row applies" in {
    val statusLine = load("display.word_count = true\n").config.statusLine

    statusLine.segments shouldBe StatusLineConfig.defaultSegments ++
      List(StatusSegment.WordCount, StatusSegment.CharCount, StatusSegment.ReadingTime)
    statusLine.placement shouldBe StatusLinePlacement.Pinned
  }

  "the colour and alpha overrides" should "carry over unchanged" in {
    val text   = "cursor.info_bar.foreground_color = \"#112233\"\ndisplay.cursor_info_bar_background_alpha = 0.5\n"
    val colors = load(text).config.statusLine.colors

    colors.foreground.map(_.getRGB & 0xffffff) shouldBe Some(0x112233)
    colors.backgroundAlpha shouldBe Some(0.5)
  }

  "current keys" should "win over any old key in the same file" in {
    val statusLine = load("status.placement = floating\ndisplay.gutter = false\n").config.statusLine

    statusLine.placement shouldBe StatusLinePlacement.Floating
  }

  "the mode corner key" should "be reported as replaced by the mode segment" in {
    load("widget.mode_tab_corner = top-left\n").report.deprecatedEntries
      .map(entry => entry.key -> entry.replacement) should contain("widget.mode_tab_corner" -> "status.segments")
  }
