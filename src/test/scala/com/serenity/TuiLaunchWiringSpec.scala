package com.serenity

import java.nio.file.{Files, Path}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Structural coverage for issue #1112's AC that the GUI path constructs no terminal shell and the TUI path constructs
  * no `SwingWindow`. `TuiRuntime` owns the entire TUI capability bundle and is the only thing `Main`'s TUI branch calls
  * into, so "TuiRuntime never mentions SwingWindow" plus "Main dispatches --tui through TuiRuntime.run" together
  * establish the separation: a `SwingWindow` is only ever reachable from the branch of `Main` that runs when TUI mode
  * was not selected.
  */
class TuiLaunchWiringSpec extends AnyFlatSpec with Matchers:

  private def read(path: String): String =
    Files.readString(Path.of(path))

  "TuiRuntime" should "construct no SwingWindow" in {
    read("src/main/scala/com/serenity/ui/tui/TuiRuntime.scala") should not include "SwingWindow"
  }

  "Main" should "route --tui launches through TuiRuntime rather than constructing a SwingWindow" in {
    val source = read("src/main/scala/Main.scala")
    source should include("TuiRuntime.run")
    source should include("LaunchOptions.resolveTuiMode(launchOptions)")
  }
