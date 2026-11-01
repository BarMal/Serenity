package com.serenity.ui.benchmark

/** The things a person does that the two renderers are timed on, each the same on both. */
enum Scenario(val label: String):
  case ColdFullFrame   extends Scenario("cold full frame")
  case TypeCharacter   extends Scenario("type one character")
  case ScrollOneLine   extends Scenario("scroll one line")
  case ScrollFiveLines extends Scenario("scroll five lines")
  case PageDown        extends Scenario("page down")
  case CaretBlink      extends Scenario("caret blink")
