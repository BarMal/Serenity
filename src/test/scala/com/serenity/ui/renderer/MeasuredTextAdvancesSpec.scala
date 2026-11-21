package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MeasuredTextAdvancesSpec extends AnyFlatSpec with Matchers:

  private val font = new Font(Font.SANS_SERIF, Font.PLAIN, 14)
  private val frc  = com.serenity.testkit.TestMeasure.awtFontRenderContext

  "MeasuredTextAdvances.advanceOf" should "match the last caret stop of the text" in {
    val advances = MeasuredTextAdvances(16, TestMeasure.layoutMemos())

    List("", "a", "Open file...", "wide  gaps\tand tabs").foreach { text =>
      advances.advanceOf(text, font) shouldBe
        TextLayoutSnapshot.caretXsForText(text, font, TestMeasure.layoutMemos()).lastOption.getOrElse(0.0f)
    }
  }

  it should "measure a repeated text only once" in {
    val advances = MeasuredTextAdvances(16, TestMeasure.layoutMemos())

    val first = advances.advanceOf("Open file...", font)
    val again = advances.advanceOf("Open file...", font)

    again shouldBe first
    advances.measurements shouldBe 1L
  }

  it should "measure again for a different text or font" in {
    val advances = MeasuredTextAdvances(16, TestMeasure.layoutMemos())
    val _        = advances.advanceOf("Open file...", font)

    advances.advanceOf("Open files...", font)
    advances.advanceOf("Open file...", font.deriveFont(20.0f))

    advances.measurements shouldBe 3L
  }

  it should "forget the oldest advances beyond its capacity" in {
    val advances = MeasuredTextAdvances(2, TestMeasure.layoutMemos())

    List("one", "two", "three").foreach(advances.advanceOf(_, font))
    advances.advanceOf("three", font)
    advances.advanceOf("one", font)

    advances.measurements shouldBe 4L
  }
