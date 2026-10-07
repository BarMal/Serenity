package com.serenity.ui.terminal

/** Where the window puts its menu bar. */
enum MenuBarPlacement:
  case ScreenMenuBar
  case FrameMenuBar

  /** A `JFrame` menu bar would sit above the custom title bar, so it goes in the content pane beneath it. */
  case UnderCustomTitleBar
