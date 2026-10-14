package com.serenity.testkit

import org.scalatest.Tag

/** Marks a test that reads the JVM-wide used heap and forces full collections to do it. In the parallel suite another
  * suite's allocation lands inside the reading, and the forced collections stall every other suite's IO runtime
  * (`AppRuntimeFirstFrameThemeSpec` missed its 20 s startup budget beside `FootprintBenchmarksSpec`). Excluded from
  * `sbt test` (see `build.sbt`'s `Test / test / testOptions`) and run alone by the `heapMeasurementTest` command.
  */
object HeapMeasurementTest extends Tag("com.serenity.testkit.HeapMeasurementTest")
