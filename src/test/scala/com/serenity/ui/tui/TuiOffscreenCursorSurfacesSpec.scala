package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.config.AppConfigOps.withOffscreenCursorSurfaces
import com.serenity.config.OffscreenCursorSurfaces
import com.serenity.state.models.{AppState, CursorPosition, SurfacePresentation}
import com.serenity.ui.layout.{LayoutEngine, LayoutRect, ViewportSize}

/** #2570 item 3: a surface anchored to the caret -- the command palette, the contextual toolbar -- opened while the
  * wheel has the caret scrolled out of the pane used to take focus without being laid out or painted at all. Now it is
  * always laid out inside the pane, and `editor.cursor.offscreen_surfaces` decides what opening it does to the view or
  * the caret.
  */
class TuiOffscreenCursorSurfacesSpec extends TuiSpec:

  private val terminalSize = ViewportSize(60, 14)
  private val document     = (0 until 400).map(index => f"L$index%04d").mkString("\n")

  private def environment(choice: OffscreenCursorSurfaces) =
    TuiEnvironment
      .withFile(document)
      .withViewport(terminalSize)
      .withConfig(_.withLineNumbers(false).withOffscreenCursorSurfaces(choice))

  private val wheelDown = TuiKeys.wheelDown(10, 5)
  private val wheelUp   = TuiKeys.wheelUp(10, 5)

  /** The caret on line 0, the view ten notches below it. */
  private val caretAbove: TuiScript[Unit] = (0 until 10).toList.traverse_(_ => press(wheelDown))

  /** The caret on line 60, the view scrolled back up to the top. */
  private val caretBelow: TuiScript[Unit] =
    (0 until 60).toList.traverse_(_ => press(ArrowDown)) >> (0 until 30).toList.traverse_(_ => press(wheelUp))

  private def editorBuffer(current: AppState) =
    current.persisted.layout.activeEditorPaneId
      .flatMap(current.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(current.persisted.buffers.get)

  private def top(current: AppState): Option[Int]                = editorBuffer(current).map(_.viewport.topLine)
  private def caret(current: AppState): Option[CursorPosition] =
    editorBuffer(current).flatMap(_.editing.cursorPositions.headOption)

  /** Every cursor-anchored rect laid out, and the editor pane's content rect they must sit inside. */
  private def laidOut(current: AppState): (List[LayoutRect], Option[LayoutRect]) =
    val layout = LayoutEngine.calculateLayoutWithUI(current, terminalSize)
    val pane = current.persisted.layout.activeEditorPaneId.flatMap(id =>
      LayoutEngine.laidOutPanes(current, layout).layouts.get(id).map(_.contentRect)
    )
    ((layout.belowCursorOverlayStack ++ layout.aboveCursorOverlayStack).map(_._2), pane)

  private def inside(rect: LayoutRect, pane: LayoutRect): Boolean =
    rect.y >= pane.y && rect.bottom <= pane.bottom && rect.x >= pane.x && rect.right <= pane.right

  private val openPalette: TuiScript[Unit] = ctrl('p')
  private val openToolbar: TuiScript[Unit] = ctrlShift('t')

  private def opened(scrolled: TuiScript[Unit], open: TuiScript[Unit]) =
    for
      _      <- scrolled
      before <- state
      _      <- open
      _      <- settledScreen
      after  <- state
    yield (before, after)

  private def shownInside(after: AppState): Unit =
    val (rects, pane) = laidOut(after)
    after.runtime.uiSurfaces.map(_.presentation).collect { case floating: SurfacePresentation.Floating => floating } should
      not be empty
    rects should not be empty
    pane.foreach(p => rects.filterNot(inside(_, p)) shouldBe Nil)

  for
    (label, scrolled) <- List("above" -> caretAbove, "below" -> caretBelow)
    (name, open)      <- List("palette" -> openPalette, "contextual toolbar" -> openToolbar)
  do
    s"The $name opened with the caret scrolled $label the pane, set to clamp" should
      "be laid out inside the pane, leaving the view and the caret put" in
      runTui(environment(OffscreenCursorSurfaces.Clamp)) {
        opened(scrolled, open).map { (before, after) =>
          shownInside(after)
          top(after) shouldBe top(before)
          caret(after) shouldBe caret(before)
        }
      }

    s"The $name opened with the caret scrolled $label the pane, set to reveal the cursor" should
      "bring the caret's line into the pane, leaving the caret put" in
      runTui(environment(OffscreenCursorSurfaces.RevealCursor)) {
        opened(scrolled, open).map { (before, after) =>
          shownInside(after)
          caret(after) shouldBe caret(before)
          val (shownTop, line) = (top(after).getOrElse(-1), caret(after).map(_.line).getOrElse(-1))
          line should (be >= shownTop and be < shownTop + 12)
        }
      }

    s"The $name opened with the caret scrolled $label the pane, set to summon the cursor" should
      "move the caret to the pane's middle row, leaving the view put" in
      runTui(environment(OffscreenCursorSurfaces.SummonCursor)) {
        opened(scrolled, open).map { (before, after) =>
          shownInside(after)
          top(after) shouldBe top(before)
          caret(after).map(_.line) shouldBe top(before).map(_ + 6)
        }
      }

  "An open palette" should "stay inside the pane while the wheel scrolls the caret away" in
    runTui(environment(OffscreenCursorSurfaces.Clamp)) {
      for
        _     <- openPalette
        _     <- (0 until 10).toList.traverse_(_ => press(wheelDown))
        _     <- settledScreen
        after <- state
      yield shownInside(after)
    }
