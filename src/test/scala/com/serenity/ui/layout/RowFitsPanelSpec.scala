package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A row fits the panel when the row itself does: its trailing edge, which no character of the next row pulls on, and
  * not the caret of the character that starts the next row, which carries that character's kerning with this row's
  * last. The measurer kerns every face (#2660 item 2, step D2); the faces asked for kerning here are those AWT kerned.
  */
class RowFitsPanelSpec extends AnyFlatSpec with Matchers:

  private def kerned(face: Font): Font =
    face.deriveFont(java.util.Map.of(TextAttribute.KERNING, TextAttribute.KERNING_ON))

  private val faces = List(
    "kerned Inter"      -> kerned(BundledInterFont.plain(13f)),
    "kerned serif"      -> kerned(Font(Font.SERIF, Font.PLAIN, 15)),
    "kerned Liberation" -> kerned(Font("Liberation Serif", Font.PLAIN, 14))
  )

  private val prose =
    ("Tales of AVAST waves: Ty and Yo went To Wayne, Va. for War and Peace, while Tom's Tyrannosaurus yawned. " +
      "Wavering voices sang Away, away, over Typhoon Tracks. ") * 4

  "A wrapped row" should "never be wider than the panel, unless a single character is" in
    faces.foreach {
      case (name, face) =>
        (60 to 330).foreach { widthPx =>
          TextLayoutSnapshot
            .boundedVisualLinesForText(prose, 3, widthPx, face, wrapCache = TestMeasure.wrapCache())
            .foreach { row =>
              withClue(s"$name at $widthPx px, '${row.text}': ") {
                if row.caretStops.length > 2 then row.widthPx should be <= widthPx.toFloat + 0.01f
                else succeed
              }
            }
        }
    }

  "A trailing edge" should "be the width of the characters before it shaped on their own" in
    faces.foreach {
      case (name, face) =>
        val text  = "Hello, wide world: AVAWAY To WWWW Ty Yo iiii"
        val fonts = LineFonts(FontSpec.fromAwt(face), Vector.empty)
        val edges = TestMeasure.measurer.exactCaretsAndEdges(text, 0, fonts)._2

        (1 to text.length).foreach { count =>
          withClue(s"$name, ${text.take(count)}: ") {
            edges(count) shouldBe TestMeasure.measurer.exactCaretXs(text.take(count), 0, fonts).last +- 0.01f
          }
        }
    }
