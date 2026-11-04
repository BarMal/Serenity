package com.serenity.ui.skia

import cats.effect.unsafe.implicits.global
import com.serenity.app.RenderBackendSelection.Probe
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaProbeSpec extends AnyFlatSpec with Matchers:

  "SkiaProbe.run" should "report the outcome of loading Skiko instead of throwing, whatever this machine lacks" in {
    SkiaProbe.run.unsafeRunSync() match
      case Probe.Loaded        => succeed
      case Probe.Failed(cause) => cause should fullyMatch regex """(\w+: .+|no raster surface)"""
      case Probe.NotRun        => fail("the probe always runs")
  }

  it should "give the same outcome when asked again, so the choice of backend does not depend on who asked first" in {
    val first  = SkiaProbe.run.unsafeRunSync()
    val second = SkiaProbe.run.unsafeRunSync()

    second shouldBe first
  }
