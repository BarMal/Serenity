package com.serenity

import java.nio.file.{Files, Path}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Structural coverage for issue #1112's AC that the TUI path constructs no `SwingWindow`: `TuiRuntime` owns the entire
  * TUI capability bundle, so it never mentioning `SwingWindow` keeps a window out of a terminal launch. That `Main`
  * runs the terminal only for a terminal [[com.serenity.app.GraphicsStartup.Display]] is `GraphicsStartupSpec`'s
  * `Display.route` tests, and the compiler checks that every display is routed.
  */
class TuiLaunchWiringSpec extends AnyFlatSpec with Matchers:

  private def read(path: String): String =
    Files.readString(Path.of(path))

  "TuiRuntime" should "construct no SwingWindow" in {
    read("src/main/scala/com/serenity/ui/tui/TuiRuntime.scala") should not include "SwingWindow"
  }
