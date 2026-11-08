package com.serenity.testkit

import org.scalatest.Tag

/** Marks a test that is only true on a JVM whose AWT is headless. Every `sbt test` leg has a display (the Linux one
  * runs under Xvfb, Windows and macOS have one), so an `assume` on headlessness there is cancelled on every leg and
  * checks nothing. Excluded from `sbt test` (see `build.sbt`'s `Test / test / testOptions`) and run by the
  * `headlessAwtTest` command, on a runner with no display, where the test asserts the environment instead of assuming
  * it.
  */
object HeadlessAwtTest extends Tag("com.serenity.testkit.HeadlessAwtTest")
