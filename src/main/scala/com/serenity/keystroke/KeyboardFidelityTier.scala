package com.serenity.keystroke

/** How faithfully the active input surface can represent a recorded key chord -- the state-layer counterpart to
  * `com.serenity.ui.tui.TerminalShell.KeyboardProtocolTier` (issue #1109/#1194), carried into `AppState.Runtime` and
  * `CommandRunner` so a warning can be raised at the point a binding is recorded rather than only discovered when it
  * silently fails to fire.
  *
  * GUI mode never negotiates a terminal protocol at all -- every key event, including a lone modifier press/release,
  * reaches Swing's key listeners directly -- so it is always [[Full]].
  */
enum KeyboardFidelityTier:
  /** Every recordable chord fires as recorded, bare-modifier double-taps included: GUI mode, or a TUI session that
    * negotiated the kitty keyboard protocol.
    */
  case Full

  /** A TUI session that fell back to xterm's `modifyOtherKeys`/`formatOtherKeys` (or never got a CSI-u response at all,
    * indistinguishable from that fallback at negotiation time -- see `TerminalShell.negotiateKeyboardProtocol`'s doc).
    * Combo keys decode, but no bare-modifier press/release event exists in that wire format.
    */
  case ModifyOtherKeys
