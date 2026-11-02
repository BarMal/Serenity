package com.serenity.ui.benchmark

/** The things a person does that the renderers are timed on, each the same on every backend. `judged` is whether the
  * scenario takes part in the verdict's rule; the warm full frame and the resize are reported only.
  */
enum Scenario(val label: String, val judged: Boolean = true):
  case ColdFullFrame   extends Scenario("cold full frame")
  case WarmFullFrame   extends Scenario("warm full frame", judged = false)
  case TypeCharacter   extends Scenario("type one character")
  case ScrollOneLine   extends Scenario("scroll one line")
  case ScrollFiveLines extends Scenario("scroll five lines")
  case PageDown        extends Scenario("page down")
  case CaretBlink      extends Scenario("caret blink")
  case ResizeWindow    extends Scenario("first frame after a resize", judged = false)
