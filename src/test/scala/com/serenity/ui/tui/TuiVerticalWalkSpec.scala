package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.ui.layout.ViewportSize

import TuiScenarios.*

/** Walking Down and then Up through a long wrapped document must keep the terminal caret on the character the cursor is
  * on, in every layout: the viewport and the renderer have to agree on how many rows each line wraps to.
  */
class TuiVerticalWalkSpec extends TuiSpec:

  private val words = Vector(
    "the",
    "quick",
    "brown",
    "fox",
    "jumps",
    "over",
    "lazy",
    "dog",
    "and",
    "keeps",
    "running",
    "onward",
    "extraordinarily"
  )

  private def paragraph(index: Int): String =
    val length = (index * 37) % 90 + 3
    s"P$index " + (0 until length).map(step => words((step * 7 + index) % words.length)).mkString(" ")

  private val prose = (0 until 40).map(paragraph).mkString("\n\n")

  private val layouts: List[(String, ViewportSize, AppConfig => AppConfig)] = List(
    ("word wrap", TuiViewport.Default, _.withWordWrap(true)),
    ("word wrap in a small terminal", TuiViewport.Small, _.withWordWrap(true)),
    ("word wrap with typewriter scrolling", TuiViewport.Default, _.withWordWrap(true).withTypewriterScrolling(true)),
    ("column mode", TuiViewport.Default, _.withWordWrap(true).withColumnMode(true)),
    ("column mode in a small terminal", TuiViewport.Small, _.withWordWrap(true).withColumnMode(true)),
    (
      "column mode, one pinned column",
      TuiViewport.Default,
      _.withWordWrap(true).withColumnMode(true).withColumnCount(Some(1))
    )
  )

  /** What the caret is sitting on, against what the cursor says it should be on. `None` when there is nothing to
    * compare: the cursor is at a line end or on a space, where the cell under the caret is blank either way.
    */
  private def caretMismatch(screen: TuiScreen, current: com.serenity.state.models.AppState): Option[String] =
    for
      buffer   <- focusedBuffer(current)
      cursor   <- buffer.editing.cursorPositions.headOption
      line     <- buffer.document.content.getLine(cursor.line)
      expected <- line.lift(cursor.column).filterNot(_.isWhitespace)
      (col, row) = screen.caret
      shown      = screen.cellAt(col, row).text
      if shown != expected.toString
    yield s"cursor (${cursor.line},${cursor.column}) is '$expected' but the caret at ($col,$row) is on '$shown'"

  private def walk(direction: TuiScript[Unit], presses: Int): TuiScript[List[String]] =
    (0 until presses).toList
      .traverse { step =>
        direction >> settledScreen.flatMap(screen =>
          state.map(current => caretMismatch(screen, current).map(m => s"after press ${step + 1}: $m"))
        )
      }
      .map(_.flatten)

  for (name, size, configure) <- layouts do
    s"Down and Up under $name" should "keep the caret on the cursor's character throughout" in
      runTui(TuiEnvironment.withFile(prose).withViewport(size).withConfig(configure)) {
        for
          _     <- pressAll(List.fill(30)(TuiKeys.ArrowRight)*)
          downs <- walk(arrowDown, 130)
          ups   <- walk(arrowUp, 130)
        yield (downs ++ ups).take(5) shouldBe Nil
      }

  "Down and Up after column mode is switched off" should "keep the caret on the cursor's character throughout" in
    runTui(TuiEnvironment.withFile(prose).withConfig(_.withWordWrap(true).withColumnMode(true))) {
      for
        _     <- runCommand("toggle column mode")
        _     <- dismissSurfaces()
        _     <- verifyState("column mode is off")(_.persisted.config.surfaceConfig.columnModeEnabled shouldBe false)
        _     <- pressAll(List.fill(30)(TuiKeys.ArrowRight)*)
        downs <- walk(arrowDown, 130)
        ups   <- walk(arrowUp, 130)
      yield (downs ++ ups).take(5) shouldBe Nil
    }
end TuiVerticalWalkSpec
