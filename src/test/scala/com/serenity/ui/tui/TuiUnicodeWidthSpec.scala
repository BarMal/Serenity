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

  /** KNOWN DEFECT, asserted as it behaves today.
    *
    * The hardware caret is positioned from the cursor's character column, not its display column, so every wide glyph
    * before it costs one cell of drift: after typing a single CJK character the terminal's block cursor sits on that
    * glyph's own continuation cell rather than after it. The status bar's "Col 2" is right; the caret is not. The
    * document text and the painted cells are unaffected -- this is the caret alone. Change these expectations to
    * `ContentColumn + 2 * glyphs` when the column mapping is fixed.
    */
  it should "advance the caret by one cell per wide glyph -- one short of the glyph's own width" in runTui() {
    for
      _ <- typeText("漢")
      _ <- verify("after one wide glyph") { screen =>
        screen.caret shouldBe (ContentColumn + 1, FirstLineRow)
        screen.cellAt(ContentColumn + 1, FirstLineRow).span shouldBe CellSpan.Continuation
        screen.statusBar should include("Col 2")
      }
      _ <- typeText("字")
      _ <- verify("after two")(screen => screen.caret shouldBe (ContentColumn + 2, FirstLineRow))
      _ <- typeText("a")
      _ <- verify("after a narrow one")(screen => screen.caret shouldBe (ContentColumn + 3, FirstLineRow))
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

  /** KNOWN GAP, asserted as it behaves today.
    *
    * `CharWidth`'s table covers Miscellaneous Symbols and Pictographs (U+1F300-U+1F64F) and Supplemental Symbols
    * (U+1F900-U+1F9FF), but not Transport and Map Symbols (U+1F680-U+1F6FF) in between -- so a rocket is treated as one
    * cell wide while every real terminal draws it as two, putting the app's column arithmetic out of step with the
    * screen for that block. Flip this to `Wide` when the range is added.
    */
  "an emoji from a block CharWidth's table omits" should "be treated as a narrow glyph" in
    runTui(TuiEnvironment.withFile("🚀 go")) {
      verify("transport emoji") { screen =>
        screen.cellAt(ContentColumn, FirstLineRow).text shouldBe "🚀"
        screen.cellAt(ContentColumn, FirstLineRow).span shouldBe CellSpan.Narrow
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
          // One cell short of the glyph's width, per the caret defect documented above.
          screen.caret shouldBe (ContentColumn + 3, FirstLineRow)
        }
        text <- documentText
      yield text shouldBe Some("ab漢cd")
    }
end TuiUnicodeWidthSpec
