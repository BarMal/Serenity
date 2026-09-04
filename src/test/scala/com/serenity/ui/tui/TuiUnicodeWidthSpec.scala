package com.serenity.ui.tui

import cats.syntax.all.*

/** Content that does not fit one cell per character. A terminal grid has no sub-cell geometry, so a wide glyph occupies
  * its own cell and reserves the one to its right ([[CellSpan.Wide]] / [[CellSpan.Continuation]]) -- and every column
  * calculation downstream of that, the caret's included, has to agree.
  */
class TuiUnicodeWidthSpec extends TuiSpec:

  private val ContentColumn = 3
  private val FirstLineRow  = 1

  "a CJK document" should "place each wide glyph in one cell and reserve the next" in
    runTui(TuiEnvironment.withFile("漢字")) {
      verify("wide glyphs") { screen =>
        screen.cellAt(ContentColumn, FirstLineRow).text shouldBe "漢"
        screen.cellAt(ContentColumn, FirstLineRow).span shouldBe CellSpan.Wide
        screen.cellAt(ContentColumn + 1, FirstLineRow).span shouldBe CellSpan.Continuation
        screen.cellAt(ContentColumn + 2, FirstLineRow).text shouldBe "字"
        screen.cellAt(ContentColumn + 3, FirstLineRow).span shouldBe CellSpan.Continuation
      }
    }

  it should "advance the caret by each glyph's own display width" in runTui() {
    for
      _ <- typeText("漢")
      _ <- verify("after one wide glyph") { screen =>
        // Past the glyph's continuation cell, not on it: the caret's column is a display column, so it agrees with
        // both the painted cells and the status bar's own count.
        screen.caret shouldBe (ContentColumn + 2, FirstLineRow)
        screen.cellAt(ContentColumn + 1, FirstLineRow).span shouldBe CellSpan.Continuation
        screen.statusBar should include("Col 2")
      }
      _ <- typeText("字")
      _ <- verify("after two")(screen => screen.caret shouldBe (ContentColumn + 4, FirstLineRow))
      _ <- typeText("a")
      _ <- verify("after a narrow one")(screen => screen.caret shouldBe (ContentColumn + 5, FirstLineRow))
    yield ()
  }

  it should "delete a whole wide glyph on one Backspace, freeing both cells" in runTui() {
    for
      _    <- typeText("a漢b")
      _    <- arrowLeft
      _    <- backspace
      text <- documentText
      _ <- verify("repaired row") { screen =>
        screen.cellAt(ContentColumn, FirstLineRow).text shouldBe "a"
        screen.cellAt(ContentColumn + 1, FirstLineRow).text shouldBe "b"
        // The continuation cell the wide glyph reserved must not be left behind as a fragment.
        screen.cellAt(ContentColumn + 2, FirstLineRow).span shouldBe CellSpan.Narrow
      }
    yield text shouldBe Some("ab")
  }

  "mixed-width text" should "keep narrow characters aligned after wide ones" in
    runTui(TuiEnvironment.withFile("ab漢cd字ef")) {
      verify("mixed row") { screen =>
        screen.rowText(FirstLineRow).stripTrailing shouldBe " 1 ab漢cd字ef"
        screen.cellAt(ContentColumn + 2, FirstLineRow).span shouldBe CellSpan.Wide
        screen.cellAt(ContentColumn + 4, FirstLineRow).text shouldBe "c"
        screen.cellAt(ContentColumn + 6, FirstLineRow).span shouldBe CellSpan.Wide
        screen.cellAt(ContentColumn + 8, FirstLineRow).text shouldBe "e"
      }
    }

  "an emoji from a block CharWidth knows" should "occupy two cells, the same as any other wide glyph" in
    runTui(TuiEnvironment.withFile("😀 go")) {
      verify("emoji row") { screen =>
        screen.cellAt(ContentColumn, FirstLineRow).text shouldBe "😀"
        screen.cellAt(ContentColumn, FirstLineRow).span shouldBe CellSpan.Wide
        screen.cellAt(ContentColumn + 1, FirstLineRow).span shouldBe CellSpan.Continuation
        screen.cellAt(ContentColumn + 3, FirstLineRow).text shouldBe "g"
      }
    }

  "an emoji from the transport and map block" should "occupy two cells like every other wide glyph" in
    runTui(TuiEnvironment.withFile("🚀 go")) {
      verify("transport emoji") { screen =>
        screen.cellAt(ContentColumn, FirstLineRow).text shouldBe "🚀"
        screen.cellAt(ContentColumn, FirstLineRow).span shouldBe CellSpan.Wide
        screen.cellAt(ContentColumn + 1, FirstLineRow).span shouldBe CellSpan.Continuation
        screen.cellAt(ContentColumn + 3, FirstLineRow).text shouldBe "g"
      }
    }

  "an accented character" should "stay one cell wide" in runTui(TuiEnvironment.withFile("café")) {
    verify("narrow accents") { screen =>
      screen.rowText(FirstLineRow).stripTrailing shouldBe " 1 café"
      screen.cellAt(ContentColumn + 3, FirstLineRow).text shouldBe "é"
      screen.cellAt(ContentColumn + 3, FirstLineRow).span shouldBe CellSpan.Narrow
    }
  }

  "a document of only wide glyphs" should "fill the row two cells at a time and wrap on a glyph boundary" in
    runTui(TuiEnvironment.withFile("漢" * 200).withViewport(TuiViewport.Small)) {
      verify("wide wrap") { screen =>
        val contentCells = 80 - ContentColumn
        val perRow       = contentCells / 2
        screen.rowText(FirstLineRow).count(_ == '漢') shouldBe perRow
        // A wrapped row must not split a glyph across the boundary: the last cell is either a full glyph or blank.
        screen.cellAt(79, FirstLineRow).span should not be CellSpan.Wide
      }
    }

  "typing a wide glyph into narrow text" should "reflow the cells after it" in
    runTui(TuiEnvironment.withFile("abcd")) {
      for
        _ <- arrowRight >> arrowRight
        _ <- typeText("漢")
        _ <- verify("reflowed") { screen =>
          screen.rowText(FirstLineRow).stripTrailing shouldBe " 1 ab漢cd"
          screen.caret shouldBe (ContentColumn + 4, FirstLineRow)
        }
        text <- documentText
      yield text shouldBe Some("ab漢cd")
    }
end TuiUnicodeWidthSpec
