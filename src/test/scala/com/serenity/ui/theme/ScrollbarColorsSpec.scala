package com.serenity.ui.theme

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The scrollbar takes its colours from the theme it is drawn in. */
class ScrollbarColorsSpec extends AnyFlatSpec with Matchers:

  private val themes = List(Theme.light, Theme.dark)

  "A theme's scrollbar" should "be visible against its background without competing with the text" in
    themes.foreach { theme =>
      val thumb = Theme.contrastRatio(theme.scrollbar.thumb, theme.background)

      withClue(s"${theme.name}: ") {
        thumb should be > 1.2
        thumb should be < Theme.contrastRatio(theme.foreground, theme.background)
      }
    }

  it should "stand out more while the thumb is held" in
    themes.foreach { theme =>
      withClue(s"${theme.name}: ") {
        Theme.contrastRatio(theme.scrollbar.thumbHeld, theme.background) should be >
          Theme.contrastRatio(theme.scrollbar.thumb, theme.background)
      }
    }

  it should "draw a terminal track that shows against the background but stays dimmer than the text" in
    themes.foreach { theme =>
      val track = Theme.contrastRatio(theme.scrollbar.track, theme.background)

      withClue(s"${theme.name}: ") {
        track should be > 1.2
        track should be < Theme.contrastRatio(theme.foreground, theme.background)
      }
    }

  it should "follow the theme: the light and dark themes get different colours" in {
    Theme.light.scrollbar.thumb should not be Theme.dark.scrollbar.thumb
  }
