package com.serenity.app

import com.serenity.config.PreferredWindowSize

/** The size a renderer trial gives its window before the workload (#2618): as much of the screen as there is, so the
  * canvas has the pixels the 2x target is judged on, less the width the resize step grows it by, which the window
  * manager would otherwise refuse.
  */
object TrialWindowSize:

  def forScreen(usableWidth: Int, usableHeight: Int, resizeStep: Int): PreferredWindowSize =
    PreferredWindowSize(usableWidth - resizeStep, usableHeight).normalized
