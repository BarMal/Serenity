package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.state.models.{AppState, CursorPosition}
import com.serenity.ui.layout.ViewportSize

/** In column mode the page is a set of columns side by side: moving the caret past the last row of the last column has
  * to turn to the next page and show the caret in that page's first column, rather than leaving the viewport put with
  * the caret off screen.
  */
class TuiColumnPagingSpec extends TuiSpec:

  private val terminalSize = ViewportSize(60, 14)

  private val shortLines = (0 until 400).map(index => f"L$index%04d").mkString("\n")

  private val wrappingLines =
    (0 until 200).map(index => f"P$index%04d " + "alpha beta gamma delta " * 3).mkString("\n")

  private def environment(content: String) =
    TuiEnvironment
      .withFile(content)
      .withViewport(terminalSize)
      .withConfig(_.withWordWrap(true).withColumnMode(true).withColumnCount(Some(columns)).withLineNumbers(false))

  private def cursorOf(current: AppState): Option[CursorPosition] =
    focusedBuffer(current).flatMap(_.editing.cursorPositions.headOption)

  private val settled: TuiScript[(TuiScreen, Option[CursorPosition])] =
    settledScreen.flatMap(screen => state.map(current => (screen, cursorOf(current))))

  /** Why the caret is not shown on the cursor's character, if it is not: off the body, hidden, or on another cell.
    * Whitespace is not compared: a space hung at a wrap boundary is drawn where the row ends, not on its own cell.
    */
  private def caretProblem(screen: TuiScreen, current: AppState): Option[String] =
    for
      buffer <- focusedBuffer(current)
      cursor <- buffer.editing.cursorPositions.headOption
      line   <- buffer.document.content.getLine(cursor.line)
      (col, row) = screen.caret
      problem <-
        if !screen.caretVisible || row < 1 || row >= screen.height - 1 then
          Some(s"caret at ($col,$row) is not on the body of the screen")
        else
          line.lift(cursor.column).filterNot(_.isWhitespace).flatMap { expected =>
            val shown = screen.cellAt(col, row).text
            Option.when(shown != expected.toString)(s"caret at ($col,$row) is on '$shown', not '$expected'")
          }
    yield s"cursor (${cursor.line},${cursor.column}): $problem"

  private def checked(key: TuiKey): TuiScript[Option[String]] =
    press(key) >> settledScreen.flatMap(screen => state.map(current => caretProblem(screen, current)))

  private def keepsCaretShown(keys: List[TuiKey]): TuiScript[List[String]] =
    keys.traverse(checked).map(_.flatten)

  /** The column and row a label such as `L0007` starts at on screen, if it is shown. */
  private def labelAt(screen: TuiScreen, label: String): Option[(Int, Int)] =
    screen.rows.zipWithIndex.collectFirst {
      case (text, row) if text.contains(label) => (text.indexOf(label), row)
    }

  private val columns = 2

  /** The rows of one page: what a column shows, times the columns beside each other. */
  private def pageRowsOf(screen: TuiScreen): Int =
    columns * screen.rows.count(_.contains("L0"))

  "Down on the last row of the last column" should "turn to the next page, with the caret in its first column" in
    runTui(environment(shortLines)) {
      for
        first <- settledScreen
        rows = pageRowsOf(first)
        _      <- (0 until rows - 1).toList.traverse_(_ => press(TuiKeys.ArrowDown))
        onLast <- settled
        _      <- press(TuiKeys.ArrowDown)
        after  <- settled
      yield
        rows should be > 20
        onLast._2.map(_.line) shouldBe Some(rows - 1)
        after._2.map(_.line) shouldBe Some(rows)
        labelAt(after._1, f"L$rows%04d") shouldBe labelAt(first, "L0000")
        labelAt(after._1, f"L${rows - 1}%04d") shouldBe None
    }

  "Right at the end of the last column" should "turn to the next page, with the caret in its first column" in
    runTui(environment(shortLines)) {
      for
        first <- settledScreen
        rows = pageRowsOf(first)
        _     <- (0 until rows * 6).toList.traverse_(_ => press(TuiKeys.ArrowRight))
        after <- settled
      yield
        after._2 shouldBe Some(CursorPosition(rows, 0))
        labelAt(after._1, f"L$rows%04d") shouldBe labelAt(first, "L0000")
    }

  "Up and Left from the first row of the next page" should "turn back to the previous page" in
    runTui(environment(shortLines)) {
      for
        first <- settledScreen
        rows = pageRowsOf(first)
        _      <- (0 until rows).toList.traverse_(_ => press(TuiKeys.ArrowDown))
        up     <- checked(TuiKeys.ArrowUp)
        upSeen <- settled
        _      <- press(TuiKeys.ArrowDown)
        left   <- (0 until 6).toList.traverse(_ => checked(TuiKeys.ArrowLeft))
        back   <- settled
      yield
        up shouldBe None
        upSeen._2.map(_.line) shouldBe Some(rows - 1)
        labelAt(upSeen._1, "L0000") shouldBe labelAt(first, "L0000")
        left.flatten shouldBe Nil
        back._2.map(_.line) shouldBe Some(rows - 1)
    }

  "PageDown and PageUp across a page boundary" should "keep the caret on screen" in
    runTui(environment(shortLines)) {
      for
        down <- keepsCaretShown(List.fill(30)(TuiKeys.PageDown))
        up   <- keepsCaretShown(List.fill(30)(TuiKeys.PageUp))
      yield (down ++ up).take(5) shouldBe Nil
    }

  "Down and Right through word-wrapped lines in columns" should "keep the caret on screen across page boundaries" in
    runTui(environment(wrappingLines)) {
      for
        down  <- keepsCaretShown(List.fill(120)(TuiKeys.ArrowDown))
        right <- keepsCaretShown(List.fill(400)(TuiKeys.ArrowRight))
        up    <- keepsCaretShown(List.fill(120)(TuiKeys.ArrowUp))
        left  <- keepsCaretShown(List.fill(400)(TuiKeys.ArrowLeft))
      yield (down ++ right ++ up ++ left).take(5) shouldBe Nil
    }
