package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.TextAttribute

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/** Wrapping reuses the carets it measured while finding a row's break point instead of measuring the row again, so each
  * wrapped row must still carry exactly the geometry that measuring its text alone gives.
  */
class WrappedRowMeasurementSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private val kerned =
    Font(Font.SERIF, Font.PLAIN, 15).deriveFont(java.util.Map.of(TextAttribute.KERNING, TextAttribute.KERNING_ON))

  private val fonts = Table(
    "font",
    Font(Font.SANS_SERIF, Font.PLAIN, 13),
    Font(Font.SERIF, Font.ITALIC, 17),
    Font(Font.MONOSPACED, Font.PLAIN, 12),
    kerned
  )

  private val lines = Table(
    "line",
    "The quick brown fox jumps over the lazy dog, again and again, until the row finally has to wrap somewhere.",
    "AVAVAVA WAWAWA To Ty Yo -- kerning pairs repeated across the wrap point Tw Te Ta AV AW AY " * 3,
    "Supercalifragilisticexpialidocious" * 5,
    "tab\tseparated\tcolumns\tthat\twrap\tacross\tthe\tpanel\twidth\tseveral\ttimes",
    "zero​width​spaces​between​every​word​of​this​line " * 3,
    "été café résumé with combining accents everywhere " * 3,
    "Emoji 👍🏽 and flags 🇬🇧 between words that wrap 🎉 across rows " * 3,
    "日本語のテキストは空白なしで折り返されるので、行の途中で改行できる必要があります。" * 2,
    "مرحبا بالعالم هذا نص عربي طويل يلتف عبر عدة أسطر " * 2
  )

  "Wrapped rows" should "carry the same carets and width as measuring each row's text alone" in
    forAll(fonts) { font =>
      forAll(lines) { line =>
        Seq(97, 160, 233).foreach { widthPx =>
          val rows = TextLayoutSnapshot.boundedVisualLinesForText(line, 4, widthPx, font, frc)
          rows.map(_.text).mkString shouldBe line
          rows.foreach { row =>
            row shouldBe TextLayoutSnapshot.visualLineForText(row.text, 4, font, frc, startColumn = row.startColumn)
          }
        }
      }
    }
