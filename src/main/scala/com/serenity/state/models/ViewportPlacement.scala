package com.serenity.state.models

/** Whether a viewport rests where it was placed, or is waiting for the caret to place it. `FollowCaret` is what a typed
  * key leaves behind; `ViewportResolution` turns it back into `Placed` once the run of keys has settled.
  */
enum ViewportPlacement:
  case Placed, FollowCaret
