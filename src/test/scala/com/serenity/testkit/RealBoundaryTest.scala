package com.serenity.testkit

import org.scalatest.Tag

/** Marks a test that crosses a genuine OS boundary -- a real loopback socket, a real OS signal delivered through
  * `sun.misc.Signal.raise` -- rather than the in-memory doubles (`QueueBytePipe`, `FakeTerminalReader`,
  * `Terminal.raise()`) the rest of the suite uses everywhere the same boundary shows up. Excluded from the default
  * `sbt test` run (see `build.sbt`'s `Test / testOptions`) and run instead via the `realBoundaryTest` command, since
  * binding a real port or hooking a process-wide native signal handler is exactly the kind of real OS state the fast
  * parallel suite must never touch -- not because these tests are themselves flaky.
  */
object RealBoundaryTest extends Tag("com.serenity.testkit.RealBoundaryTest")
