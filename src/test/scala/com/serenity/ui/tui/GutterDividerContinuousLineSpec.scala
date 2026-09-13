package com.serenity.ui.tui

import TuiScenarios.*

/** #1483: the vertical divider between the line-number gutter and the document body rendered as short per-line dashes
  * instead of one smooth continuous line. [[com.serenity.ui.renderer.RendererGutter.renderLineNumbers]] now paints that
  * boundary as a single full-height background fill rather than leaving it to agree with itself across each row's own
  * (independent) fill -- so every body row must show the exact same colour at the gutter's last column, and that colour
  * must be visibly distinct from the plain document background, not just incidentally uniform.
  */
class GutterDividerContinuousLineSpec extends TuiSpec:

  /** The gutter's last column for this fixture: three numbered lines format as `" 1 "`, `" 2 "`, `" 3 "` -- a 3-wide
    * gutter (see `TuiEditingSpec.ContentColumn`) -- so column index 2 is the boundary against the document body.
    */
  private val DividerColumn = 2

  "the gutter" should "paint one continuous divider colour down every body row" in runTui() {
    for
      _ <- typeDocument("first line", "second line", "third line")
      _ <- verify("continuous divider") { screen =>
        val bodyRows               = 1 until (screen.height - 1)
        val dividerBackgrounds     = bodyRows.map(row => screen.backgroundAt(DividerColumn, row)).toSet
        val documentBodyBackground = screen.backgroundAt(DividerColumn + 2, 1)

        withClue(s"divider column backgrounds: $dividerBackgrounds") {
          dividerBackgrounds should have size 1
        }
        dividerBackgrounds.head should not be documentBodyBackground
      }
    yield ()
  }

  it should "keep the same divider colour on rows with and without a line number" in runTui() {
    for
      _ <- typeDocument(
        "a line long enough that word wrap folds it onto a continuation row down below",
        "second paragraph"
      )
      _ <- verify("continuous divider across continuation rows") { screen =>
        val bodyRows           = 1 until (screen.height - 1)
        val dividerBackgrounds = bodyRows.map(row => screen.backgroundAt(DividerColumn, row)).toSet
        withClue(s"divider column backgrounds: $dividerBackgrounds") {
          dividerBackgrounds should have size 1
        }
      }
    yield ()
  }
