package com.serenity.state.models

/** Whether a viewport rests where it was placed, or is waiting for the caret to place it. `FollowCaret` is what a typed
  * key leaves behind; `ViewportResolution` turns it back into `Placed` once the run of keys has settled. `Scrolled` is
  * a view the wheel or the scrollbar moved off the caret: the next caret or edit key in its pane places it again even
  * when the caret does not move, such as `Home` already at the start of a line (#2570).
  */
enum ViewportPlacement:
  case Placed, FollowCaret, Scrolled
