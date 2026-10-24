package com.serenity.testkit

import org.scalatest.Tag

/** Marks a test that loads the real `libSDL3` through the FFM binding and drives it on SDL's `dummy` video driver.
  * Excluded from the default `sbt test` run (see `build.sbt`) because the library is a build product of
  * `ci/sdl3/build-sdl3.sh`, not a dependency; run through the `sdlNativeTest` task with
  * `-Dserenity.sdl3.library.path=<directory holding libSDL3.so.0>`.
  */
object SdlNativeTest extends Tag("com.serenity.testkit.SdlNativeTest")
