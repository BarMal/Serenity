package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.state.effects.EffectLanes
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1929: typing faster than the debounce must not postpone spell check for as long as the typing lasts. */
class DocumentAnalysisDebounceSpec extends AnyFlatSpec with Matchers:

  "Document analysis" should "start within a bounded time of the first edit while keys keep arriving" in {
    val effect = DebouncedEffects.DocumentAnalysis
    val started = runVirtual(
      Ref.of[IO, Vector[FiniteDuration]](Vector.empty).flatMap { ran =>
        EffectLanes.resource((_, _) => IO.unit).use { lanes =>
          val job = IO.monotonic.flatMap(at => ran.update(_ :+ at))
          (1 to 100).toList.traverse_(_ =>
            lanes.submitAfter(effect.lane, effect.delay, job, effect.maxWait) >> IO.sleep(100.millis)
          ) >> lanes.drain >> ran.get
        }
      }
    )
    started.headOption.exists(_ <= 2.seconds) shouldBe true
    started.size should be > 3
  }
