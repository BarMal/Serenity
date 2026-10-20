package com.serenity.state.manager

import com.serenity.config.LayoutPin
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer

/** How a launch's recovery decisions show up in the first state: the safe-mode flag the status line reads, and the
  * prompt explaining a start that safe mode took over after starts that did not finish.
  */
object StartupRecoveryState:

  def applied(
    state: AppState,
    safeMode: Boolean,
    crashLoopAfter: Option[Int],
    unexpectedExit: Option[String] = None,
    layoutPins: List[LayoutPin] = Nil
  ): AppState =
    val flagged = state.copy(runtime = state.runtime.copy(safeMode = safeMode))
    val prompt = crashLoopAfter
      .map(ConfirmPrompt.startedInSafeMode)
      .orElse(unexpectedExit.map(ConfirmPrompt.closedUnexpectedly))
    val layoutPrompt = Option.when(layoutPins.nonEmpty)(
      ConfirmPrompt.layoutOverrideAtStartup(flagged.persisted.config.uiLayout, layoutPins)
    )
    // The layout question waits under the more urgent one, which is answered first.
    (layoutPrompt.toList ++ prompt.toList).foldLeft(flagged)((current, shown) =>
      ModalStateReducer.show(Modal.Confirm(shown), current).state
    )
