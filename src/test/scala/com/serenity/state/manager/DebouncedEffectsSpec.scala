package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.EcoMode
import com.serenity.config.AppConfig
import com.serenity.state.effects.EffectLanes
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1886: the waits before background analysis and the outline re-parse come from the config, so eco can lengthen them.
  */
class DebouncedEffectsSpec extends AnyFlatSpec with Matchers:

  private val eco = EcoMode.overlay(AppConfig.default)

  private def startOffsetOfSingleRequest(effect: DebouncedEffect): FiniteDuration =
    runVirtual(
      Ref.of[IO, Vector[FiniteDuration]](Vector.empty).flatMap { ran =>
        EffectLanes.resource((_, _) => IO.unit).use { lanes =>
          val job = IO.monotonic.flatMap(at => ran.update(_ :+ at))
          lanes.submitAfter(effect.lane, effect.delay, job, effect.maxWait) >> lanes.drain >> ran.get
        }
      }
    ).headOption.getOrElse(Duration.Zero)

  "DebouncedEffects.documentAnalysis" should "wait the configured analysis debounce before the job runs" in {
    startOffsetOfSingleRequest(DebouncedEffects.documentAnalysis(AppConfig.default.analysisDebounce)) shouldBe
      150.millis
    startOffsetOfSingleRequest(DebouncedEffects.documentAnalysis(eco.analysisDebounce)) shouldBe
      EcoMode.AnalysisDebounceMillis.millis
  }

  it should "keep the default one-second bound for the default debounce" in {
    DebouncedEffects.documentAnalysis(AppConfig.default.analysisDebounce).maxWait shouldBe Some(1.second)
  }

  it should "bound a never-quiet burst by more than the debounce when the debounce is longer" in {
    val effect = DebouncedEffects.documentAnalysis(eco.analysisDebounce)
    effect.maxWait.exists(_ > effect.delay) shouldBe true
    effect.maxWait.exists(_ > 1.second) shouldBe true
  }

  "DebouncedEffects.outlineRefresh" should "wait the configured outline debounce before the job runs" in {
    startOffsetOfSingleRequest(DebouncedEffects.outlineRefresh(AppConfig.default.outlineDebounce)) shouldBe
      150.millis
    startOffsetOfSingleRequest(DebouncedEffects.outlineRefresh(eco.outlineDebounce)) shouldBe
      EcoMode.OutlineDebounceMillis.millis
  }

  it should "run once for a burst of requests spaced closer than the debounce" in {
    val effect = DebouncedEffects.outlineRefresh(eco.outlineDebounce)
    val runs = runVirtual(
      Ref.of[IO, Int](0).flatMap { ran =>
        EffectLanes.resource((_, _) => IO.unit).use { lanes =>
          (1 to 5).toList.traverse_(_ =>
            lanes.submitAfter(effect.lane, effect.delay, ran.update(_ + 1), effect.maxWait) >> IO.sleep(200.millis)
          ) >> lanes.drain >> ran.get
        }
      }
    )
    runs shouldBe 1
  }
