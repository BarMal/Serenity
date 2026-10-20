package com.serenity

import com.serenity.state.models.{AppState, CursorPosition, PaneId}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CalculatedLayout, LayoutEngine}

/** In the windowed editor a row is as tall as its font measures, so the cell row a caret sits in is not its line
  * number: the two part ways after some rows by an amount that depends on the platform's font metrics. A spec that
  * counts a caret's row as its line number holds only where they happen to coincide; one that asks where the caret is
  * painted holds everywhere.
  */
object MeasuredRows:

  /** A font size at which the bundled code font's measured rows drift from the cells on Linux, as they do at the
    * default size on Windows.
    */
  val driftingFontConfig: FontLoader.FontConfig = FontLoader.FontConfig(fontSize = 14f, textFontSize = 14f)

  /** The cell row `cursor` is painted in, as popups anchor to it. */
  def caretRow(state: AppState, layout: CalculatedLayout, paneId: PaneId, cursor: CursorPosition): Option[Int] =
    LayoutEngine.laidOutPanes(state, layout).screenPosition(paneId, cursor).map(_.y)
