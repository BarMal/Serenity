package com.serenity.state.models

import com.serenity.config.CornerPosition

enum SurfacePlacement:
  case AboveCursor
  case BelowCursor

  /** A screen corner, for a floating overlay panel assigned an edge/corner position (issue #1310, mode 3) rather than
    * anchored to the cursor -- laid out by `LayoutEngine.calculateCornerOverlayStack`, stacking as a list when more
    * than one surface shares the same corner.
    */
  case Corner(position: CornerPosition)
