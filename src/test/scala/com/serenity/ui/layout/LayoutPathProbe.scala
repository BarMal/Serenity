package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.fonts.FontLoader

/** Which layout path (`shouldUseMeasuredLayout`) a font takes, and why: monospaced, ligatures, and the fractional
  * advance drift check that is private to the layout package (#2447 B2). Test-only, no product code.
  */
object LayoutPathProbe:

  private def describe(label: String, font: Font): Unit =
    val cache    = WrappedLineCache.bounded()
    val measured = TextLayoutSnapshot.shouldUseMeasuredLayout(font, TextLayoutSnapshot.defaultFontRenderContext(), cache.shapedText.probes)
    val cell     = CellMetrics.fromFont(font)
    println(
      f"  $label%-44s monospaced=${FontLoader.isMonospacedFont(font)}%-5s ligatures=${FontLoader.ligaturesEnabled(font)}%-5s cell ${cell.charWidth}x${cell.lineHeight}  -> measured layout path = $measured"
    )

  def main(args: Array[String]): Unit =
    println(s"java ${System.getProperty("java.version")}, ${System.getProperty("os.name")}")
    val defaults = FontLoader.FontConfig()
    for size <- List(11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 18.0f) do
      val on  = FontLoader.previewCodeFont(defaults.copy(fontSize = size, enableLigatures = true))
      val off = FontLoader.previewCodeFont(defaults.copy(fontSize = size, enableLigatures = false))
      describe(f"bundled code font $size%4.1f pt, ligatures ON", on)
      describe(f"bundled code font $size%4.1f pt, ligatures OFF", off)
    for size <- List(12, 14, 16) do describe(s"Font.MONOSPACED $size pt", Font(Font.MONOSPACED, Font.PLAIN, size))
    describe("text font (proportional)", FontLoader.previewTextFont(defaults))
