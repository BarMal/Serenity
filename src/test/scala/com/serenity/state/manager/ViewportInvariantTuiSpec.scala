package com.serenity.state.manager

import scala.util.Random

import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.state.models.AppState
import com.serenity.ui.layout.{ColumnPaging, ScrollLimits, ViewportSize, WrappedLineCache}
import com.serenity.ui.tui.{TuiEnvironment, TuiKey, TuiKeys, TuiScreen, TuiScript, TuiSpec}

/** The invariant #2570 items 2-5 broke in different ways: after every step a shown buffer's viewport is a place it may
  * rest -- inside the row-mode scroll limits, or on a page start in column mode -- and after every key that moves the
  * caret the caret is on screen. The wheel may leave the caret off screen; the next key brings it back.
  */
class ViewportInvariantTuiSpec extends TuiSpec:

  private val terminalSize = ViewportSize(60, 14)

  private val document =
    (0 until 240)
      .map(index => if index % 7 == 3 then f"P$index%04d " + "alpha beta gamma delta " * 4 else f"L$index%04d")
      .mkString("\n")

  private val wheelDown = TuiKeys.wheelDown(10, 5)
  private val wheelUp   = TuiKeys.wheelUp(10, 5)

  private val caretKeys: Vector[TuiKey] =
    Vector(ArrowDown, ArrowDown, ArrowUp, ArrowRight, ArrowLeft, PageDown, PageUp, End, Home, TuiKeys.text("x"))

  private def walk(seed: Long, steps: Int): Vector[TuiKey] =
    val random = Random(seed)
    Vector.fill(steps) {
      random.nextInt(10) match
        case 0 | 1 => wheelDown
        case 2     => wheelUp
        case _     => caretKeys(random.nextInt(caretKeys.length))
    }

  private def environment(config: AppConfig => AppConfig) =
    TuiEnvironment.withFile(document).withViewport(terminalSize).withConfig(config)

  private def viewportProblem(current: AppState): Option[String] =
    for
      buffer <- focusedBuffer(current)
      top = ColumnPaging.ColumnStart(buffer.viewport.topLine, buffer.viewport.topVisualLine)
      resting <-
        val surface = current.persisted.config.surfaceConfig
        if surface.columnModeEnabled && surface.wordWrapEnabled then
          Some(CursorViewport.ColumnPages.of(buffer, current, WrappedLineCache.Uncached).pageHolding(top))
        else
          val limit = ScrollLimits.last(
            CursorViewport.visualRowsFor(buffer, current, WrappedLineCache.Uncached),
            CursorViewport.rowsThatFit(buffer, current)
          )
          val (line, row) = limit.clamp(top.line, top.row)
          Some(ColumnPaging.ColumnStart(line, row))
      problem <- Option.when(resting != top)(s"viewport rests at $top, not $resting")
    yield problem

  private def caretProblem(screen: TuiScreen): Option[String] =
    val (col, row) = screen.caret
    Option.when(!screen.caretVisible || row < 1 || row >= screen.height - 1)(
      s"caret at ($col,$row) (visible=${screen.caretVisible}) is off the body:\n${screen.rows.mkString("\n")}"
    )

  private def checkedStep(key: TuiKey, index: Int): TuiScript[List[String]] =
    for
      _       <- press(key)
      screen  <- settledScreen
      current <- state
    yield
      val caret  = if key == wheelDown || key == wheelUp then None else caretProblem(screen)
      val cursor = focusedBuffer(current).map(buffer => (buffer.editing.cursorPositions.headOption, buffer.viewport))
      (viewportProblem(current).toList ++ caret.toList).map(problem => s"step $index (${key.name}) $cursor: $problem")

  private def holdsInvariant(keys: Vector[TuiKey]): TuiScript[List[String]] =
    keys.zipWithIndex.toList.traverse((key, index) => checkedStep(key, index)).map(_.flatten)

  "A random walk of caret keys and wheel notches with word wrap" should "keep the viewport resting and the caret shown" in
    runTui(environment(_.withWordWrap(true).withLineNumbers(false))) {
      holdsInvariant(walk(seed = 2570L, steps = 160)).map(problems => problems.take(5) shouldBe Nil)
    }

  it should "hold with typewriter scrolling on" in
    runTui(environment(_.withWordWrap(true).withTypewriterScrolling(true).withLineNumbers(false))) {
      holdsInvariant(walk(seed = 2571L, steps = 160)).map(problems => problems.take(5) shouldBe Nil)
    }

  it should "hold in column mode" in
    runTui(
      environment(_.withWordWrap(true).withColumnMode(true).withColumnCount(Some(2)).withLineNumbers(false))
    ) {
      holdsInvariant(walk(seed = 2572L, steps = 160)).map(problems => problems.take(5) shouldBe Nil)
    }

  "A caret key that leaves the caret put, after the wheel scrolled it away" should "bring the caret back on screen" in
    runTui(environment(_.withWordWrap(true).withLineNumbers(false))) {
      for
        _      <- (0 until 10).toList.traverse_(_ => press(wheelDown))
        away   <- settledScreen
        _      <- press(Home)
        screen <- settledScreen
      yield
        caretProblem(away) should not be None
        caretProblem(screen) shouldBe None
    }

  "A view the wheel scrolled" should "stay put while no key goes to its pane" in
    runTui(environment(_.withWordWrap(true).withLineNumbers(false))) {
      for
        _      <- (0 until 10).toList.traverse_(_ => press(wheelDown))
        before <- state
        _      <- press(wheelDown)
        after  <- state
      yield focusedBuffer(after).map(_.viewport.topLine) should be > focusedBuffer(before).map(_.viewport.topLine)
    }
