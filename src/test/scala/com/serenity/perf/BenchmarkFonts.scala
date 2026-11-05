package com.serenity.perf

import java.awt.Font
import java.io.ByteArrayInputStream

import cats.effect.unsafe.implicits.global
import com.serenity.exporting.BundledFonts

/** The bundled fonts the benchmarks measure with, read from the classpath: `serenity-perf.jar` runs on a laptop with no
  * source tree, so a path under `src/main/resources` is not there to read.
  */
private[perf] object BenchmarkFonts:

  val CourierPrimeRegular: String = "/fonts/CourierPrime-Regular.ttf"

  def courierPrimeRegular(): Font = load(CourierPrimeRegular)

  private def load(resource: String): Font =
    val bytes = BundledFonts.readResource(resource).unsafeRunSync()
    Font.createFont(Font.TRUETYPE_FONT, new ByteArrayInputStream(bytes))
