package com.serenity.ui.layout

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.testkit.TestMeasure
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.FontSpec
import org.jetbrains.skia.shaper.Shaper
import org.scalatest.flatspec.AnyFlatSpec

/** SCRATCH, never merged: timings of the certificate under the SERENITY_EXP flags. */
class ExpCertProfileSpec extends AnyFlatSpec:
  given Balance = Balance.default

  private def ms[A](body: => A): (A, Double) =
    val started = System.nanoTime()
    val result  = body
    (result, (System.nanoTime() - started) / 1e6)

  "the certificate" should "report its costs" in {
    val flags = sys.env.getOrElse("SERENITY_EXP", "")
    val tag   = if flags.isEmpty then "none" else flags
    val shaperCosts = (1 to 5).map(_ => ms(Shaper.Companion.make())._2)
    println(f"PROFILE[$tag] shaper creation ms: ${shaperCosts.map(c => f"$c%.1f").mkString(" ")}")
    val buffer = Buffer.fromString(BufferId(1), "  val value = compute(first, second) + 12345")
    Seq(12.0f, 14.0f, 16.0f).foreach { size =>
      val awt     = FontLoader.previewCodeFont(FontLoader.FontConfig().copy(fontSize = size, enableLigatures = false))
      val spec    = FontSpec.fromAwt(awt)
      val metrics = TestMeasure.cellMetrics(awt)
      val (cert1, cold) = ms(NominalAdvances.of(spec, TestMeasure.measurer))
      val (_, again)    = ms(NominalAdvances.of(spec, TestMeasure.measurer))
      val (_, firstWrap) = ms(VisualRowCounts.forBuffer(buffer, 1472, awt, Some(metrics), false, TestMeasure.wrapCache()))
      val (_, nextWrap)  = ms(VisualRowCounts.forBuffer(buffer, 1472, awt, Some(metrics), false, TestMeasure.wrapCache()))
      println(
        f"PROFILE[$tag] ${size}%.0fpt cert=${cert1.isDefined} build-cold-ms=$cold%.1f build-again-ms=$again%.1f first-wrap-ms=$firstWrap%.1f next-fresh-cache-wrap-ms=$nextWrap%.1f"
      )
    }
  }
