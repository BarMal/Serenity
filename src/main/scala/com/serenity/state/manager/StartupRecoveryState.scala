package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer

/** How a launch's recovery decisions show up in the first state: the safe-mode flag the status line reads, and the
  * prompt explaining a start that safe mode took over after starts that did not finish.
  */
object StartupRecoveryState:

  def applied(state: AppState, safeMode: Boolean, crashLoopAfter: Option[Int]): AppState =
    val flagged = state.copy(runtime = state.runtime.copy(safeMode = safeMode))
    crashLoopAfter.fold(flagged) { unfinished =>
      ModalStateReducer.show(Modal.Confirm(ConfirmPrompt.startedInSafeMode(unfinished)), flagged).state
    }
