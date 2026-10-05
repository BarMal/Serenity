package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer

/** How a launch's recovery decisions show up in the first state: the safe-mode flag the status line reads, and the
  * prompt offering safe mode after starts that did not finish.
  */
object StartupRecoveryState:

  def applied(state: AppState, safeMode: Boolean, offeredAfter: Option[Int]): AppState =
    val flagged = state.copy(runtime = state.runtime.copy(safeMode = safeMode))
    offeredAfter.fold(flagged) { unfinished =>
      ModalStateReducer.show(Modal.Confirm(ConfirmPrompt.offerSafeMode(unfinished)), flagged).state
    }
