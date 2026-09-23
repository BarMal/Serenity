package com.serenity.testkit

import org.scalatest.Tag

/** Marks a test that reads or reconfigures [[com.serenity.ui.renderer.RendererFrameState]]'s process-wide, JVM-shared
  * cache capacity and asserts on the exact value or on capacity-dependent eviction behaviour. That capacity is a single
  * `AtomicInteger` for the whole test process, and `StateManager.apply` -- called by the large majority of specs in
  * this suite -- resets it to whatever `AppConfig` it was given on every construction. Under sbt's default
  * parallel-suite execution, any one of those specs can reset the shared capacity between two statements of a test
  * tagged here, breaking an assertion that depends on the capacity not moving underneath it (issue #1433's flake on
  * Windows CI, e.g. runs 35805605031/35644671509). Excluded from the default `sbt test` run (see `build.sbt`'s
  * `Test / testOptions`) and run instead, alone, via the `cacheCapacityTest` command -- not because these tests are
  * themselves flaky, but because nothing else in the JVM may touch this singleton while they run.
  */
object CacheCapacityIsolatedTest extends Tag("com.serenity.testkit.CacheCapacityIsolatedTest")
