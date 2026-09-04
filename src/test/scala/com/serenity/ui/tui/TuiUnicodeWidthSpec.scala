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

  /** PENDING -- OPEN DEFECT. Asserts the behaviour that is wanted, and is reported pending until it holds.
    *
    * The hardware caret is positioned from the cursor's character column rather than its display column, so every wide
    * glyph before it costs a cell of drift: after typing a single CJK character the terminal's block cursor sits on
    * that glyph's own continuation cell instead of after it. The status bar's "Col 2" is right; the caret is not. The
    * document text and the painted cells are unaffected -- this is the caret alone.
    *
    * The fix is a display-column mapping for the caret, in the same area as #1266's `NavigationGeometry` work; when it
    * lands, this test starts passing and `pendingUntilFixed` itself fails, asking for the marker to be removed.
    */
  it should "advance the caret past the whole glyph, two cells per wide character" in pendingUntilFixed {
    runTui() {
      for
        _ <- typeText("漢")
        _ <- verify("after one wide glyph") { screen =>
          screen.caret shouldBe (ContentColumn + 2, FirstLineRow)
          screen.statusBar should include("Col 2")
        }
        _ <- typeText("字")
        _ <- verify("after two")(screen => screen.caret shouldBe (ContentColumn + 4, FirstLineRow))
        _ <- typeText("a")
        _ <- verify("after a narrow one")(screen => screen.caret shouldBe (ContentColumn + 5, FirstLineRow))
      yield ()
    }
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

  /** Transport and Map Symbols (U+1F680-U+1F6FF) sat in the gap between `CharWidth`'s Miscellaneous Symbols range and
    * its Supplemental Symbols range, so a rocket was measured as one cell while every real terminal draws it as two.
    * The range is now in the table; this is its regression cover.
    */
  "an emoji from the transport and map symbols block" should "be two cells wide, like any other emoji" in
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
        _    <- arrowRight >> arrowRight
        _    <- typeText("漢")
        _    <- verify("reflowed")(screen => screen.rowText(FirstLineRow).stripTrailing shouldBe " 1 ab漢cd")
        text <- documentText
      yield text shouldBe Some("ab漢cd")
    }

  /** PENDING -- OPEN DEFECT: the same caret drift as above, seen after an insertion rather than at the line start. */
  it should "leave the caret past the inserted glyph, not inside it" in pendingUntilFixed {
    runTui(TuiEnvironment.withFile("abcd")) {
      for
        _ <- arrowRight >> arrowRight
        _ <- typeText("漢")
        _ <- verify("caret past the glyph")(screen => screen.caret shouldBe (ContentColumn + 4, FirstLineRow))
      yield ()
    }
  }
end TuiUnicodeWidthSpec
