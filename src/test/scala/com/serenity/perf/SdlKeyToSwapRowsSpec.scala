package com.serenity.perf

import com.serenity.diagnostics.PhaseStats
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How a typed key's clock readings become the rows the SDL3 key-to-swap benchmark prints (#2380). Every reading is on
  * SDL's clock, in nanoseconds, so a row is a plain difference with no clock to convert.
  */
class SdlKeyToSwapRowsSpec extends AnyFlatSpec with Matchers:

  private def ms(value: Double): Long = (value * 1_000_000.0).toLong

  /** A key SDL stamped at 0, the pump picked up after 1 ms and the swap showed after `shownMs`. */
  private def key(shownMs: Double): KeySwapStamps = KeySwapStamps(event = 0L, pickup = ms(1.0), shown = ms(shownMs))

  "KeySwapRows" should "take key_to_swap from the event's own timestamp to the swap" in {
    val rows = KeySwapRows.of((1 to 20).map(shown => key(shown.toDouble)).toVector)

    rows.keyToSwap.count shouldBe 20
    rows.keyToSwap.p50Ms shouldBe 10.0 +- 1e-9
    rows.keyToSwap.p95Ms shouldBe 19.0 +- 1e-9
  }

  it should "take event_pickup from the event's timestamp to the pump reading it" in {
    val rows = KeySwapRows.of((1 to 20).map(shown => key(shown.toDouble)).toVector)

    rows.eventPickup.p50Ms shouldBe 1.0 +- 1e-9
    rows.eventPickup.p95Ms shouldBe 1.0 +- 1e-9
  }

  it should "report no rows for no keys" in {
    val rows = KeySwapRows.of(Vector.empty)

    rows.keyToSwap.count shouldBe 0
    rows.outOfOrder shouldBe 0
  }

  it should "count a key whose readings are out of order, and say so on the line" in {
    val backwards  = KeySwapStamps(event = ms(5.0), pickup = ms(2.0), shown = ms(9.0))
    val shownEarly = KeySwapStamps(event = 0L, pickup = ms(4.0), shown = ms(3.0))
    val rows       = KeySwapRows.of(Vector(key(8.0), backwards, shownEarly))

    rows.outOfOrder shouldBe 2
    rows.lines("swap_interval=1").head should include("out_of_order=2")
  }

  it should "not mention the order when every key is in order" in {
    KeySwapRows.of(Vector(key(8.0))).lines("swap_interval=1").mkString should not include "out_of_order"
  }

  it should "print one [sdl3-key] line per row, each with its p50 and p95 in milliseconds" in {
    val lines = KeySwapRows.of((1 to 20).map(shown => key(shown.toDouble)).toVector).lines("swap_interval=1")

    lines should contain(
      "[sdl3-key] swap_interval=1 key_to_swap p50=10.00 p95=19.00 max=20.00 n=20"
    )
    lines should contain("[sdl3-key] swap_interval=1 event_pickup p50=1.00 p95=1.00 max=1.00 n=20")
  }

  it should "print the same shape for a row measured elsewhere, such as draw or a stage of the key's wait" in {
    KeySwapRows.line("swap_interval=0", "frame_wait", count = 60, p50Ms = 3.456, p95Ms = 7.0, maxMs = 8.999) shouldBe
      "[sdl3-key] swap_interval=0 frame_wait p50=3.46 p95=7.00 max=9.00 n=60"
  }

  private val someStats = PhaseStats(count = 10, p50Ms = 1.0, p95Ms = 2.0, maxMs = 3.0)

  private def outcome(stages: String*): SdlKeyToSwapBenchmark.Outcome =
    SdlKeyToSwapBenchmark.Outcome(
      swapInterval = 1,
      renderFpsTarget = "60",
      sent = 1,
      arrived = 2,
      keys = Vector(key(8.0)),
      frames = Vector.empty,
      stages = stages.toList.map(_ -> someStats),
      unpainted = 0
    )

  "SdlKeyToSwapBenchmark.Outcome" should "print the trace's stages and the key rows" in {
    val lines = outcome("frame_wait", "apply_decode").lines

    lines.exists(_.contains(" frame_wait p50=")) shouldBe true
    lines.exists(_.contains(" apply_decode p50=")) shouldBe true
    lines.exists(_.contains(" key_to_swap p50=")) shouldBe true
  }

  it should "leave out the render splits this path does not measure, which would read 0 and look like data" in {
    val lines = outcome("frame_wait", "render_layout", "render_draw", "render_flush").lines

    lines.exists(_.contains(" frame_wait p50=")) shouldBe true
    List("render_layout", "render_draw", "render_flush").foreach { split =>
      lines.exists(_.contains(s" $split ")) shouldBe false
    }
  }

  it should "say how many keys were sent, shown and left unpainted, and at what frame rate target" in {
    outcome("frame_wait").lines.head shouldBe
      "[sdl3-key] swap_interval=1 keys sent=1 shown=1 unpainted=0 frames=0 render_fps_target=60"
  }

  "The header notes" should "say the flush key is left out of the rows and what it does to the last key, for either injector" in
    SdlKeyToSwapBenchmark.Injector.values.toList.foreach { injector =>
      val options = SdlKeyToSwapBenchmark.Options.default.copy(injector = injector)
      val notes   = SdlKeyToSwapBenchmark.headerNotes(options).mkString("\n")

      notes should include("flush")
      notes should include("left out of the rows")
      notes should include("last measured key")
    }

  it should "say the pump's wait is not the compositor's hop only for in-process keys" in {
    val inProcess  = SdlKeyToSwapBenchmark.Options.default.copy(injector = SdlKeyToSwapBenchmark.Injector.InProcess)
    val compositor = SdlKeyToSwapBenchmark.Options.default.copy(injector = SdlKeyToSwapBenchmark.Injector.Compositor)

    SdlKeyToSwapBenchmark.headerNotes(inProcess).mkString should include("compositor hop")
    SdlKeyToSwapBenchmark.headerNotes(compositor).mkString should not include "compositor hop"
  }
