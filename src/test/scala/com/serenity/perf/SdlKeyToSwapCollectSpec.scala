package com.serenity.perf

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.diagnostics.{KeyLatency, LatencyWindow}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Collecting the painted keys of a run (#2380). A key a frame has taken is no longer pending but is not painted until
  * that frame's paint finishes, so a run that stops at "nothing pending" can drain before the last paint and lose it.
  */
class SdlKeyToSwapCollectSpec extends AnyFlatSpec with Matchers:

  private def key(seq: Long): KeyLatency = KeyLatency(seq, 0L, Vector.empty, 0L)

  private def window(keys: Seq[Long], unpainted: Int = 0): LatencyWindow =
    LatencyWindow(0.0, keys.map(key).toVector, unpainted)

  /** Each drain returns the next window of `windows`, then empty windows. */
  private def drainOf(windows: List[LatencyWindow]): IO[IO[LatencyWindow]] =
    Ref.of[IO, List[LatencyWindow]](windows).map { remaining =>
      remaining.modify {
        case next :: rest => (rest, next)
        case Nil          => (Nil, window(Nil))
      }
    }

  private def collect(windows: List[LatencyWindow], expected: Int, within: FiniteDuration = 5.seconds) =
    drainOf(windows).flatMap(SdlKeyToSwapBenchmark.collectPainted(_, expected, within, 1.milli)).unsafeRunSync()

  "collectPainted" should "keep draining until every expected key is painted" in {
    val collected = collect(List(window(1L to 19L), window(Nil), window(List(20L))), expected = 20)

    collected.keys.map(_.seq) shouldBe (1L to 20L).toVector
  }

  it should "count a key reported unpainted as accounted for, rather than wait for it" in {
    val collected = collect(List(window(1L to 18L, unpainted = 2)), expected = 20)

    collected.keys should have size 18
    collected.unpainted shouldBe 2
  }

  it should "give up after the time allowed, reporting what it has" in {
    val collected = collect(List(window(1L to 19L)), expected = 20, within = 50.millis)

    collected.keys should have size 19
  }
  "measuredKeys" should "leave out the warm-up keys and the flush key that follows the run" in {
    val allKeys = (1L to 26L).toVector

    SdlKeyToSwapBenchmark.measuredKeys(allKeys, warmup = 5, keys = 20) shouldBe (6L to 25L).toVector
  }

  it should "give what there is when the run ended short" in {
    SdlKeyToSwapBenchmark.measuredKeys((1L to 12L).toVector, warmup = 5, keys = 20) shouldBe (6L to 12L).toVector
    SdlKeyToSwapBenchmark.measuredKeys((1L to 3L).toVector, warmup = 5, keys = 20) shouldBe Vector.empty
  }
end SdlKeyToSwapCollectSpec
