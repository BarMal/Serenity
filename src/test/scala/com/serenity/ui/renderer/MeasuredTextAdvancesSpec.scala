package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MeasuredTextAdvancesSpec extends AnyFlatSpec with Matchers:

  private val font = new Font(Font.SANS_SERIF, Font.PLAIN, 14)
  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()

  "MeasuredTextAdvances.advanceOf" should "match the last caret stop of the text" in {
    val advances = MeasuredTextAdvances(16)

    List("", "a", "Open file...", "wide  gaps\tand tabs").foreach { text =>
      advances.advanceOf(text, font, frc) shouldBe
        TextLayoutSnapshot.caretXsForText(text, font, frc).lastOption.getOrElse(0.0f)
    }
  }

  it should "measure a repeated text only once" in {
    val advances = MeasuredTextAdvances(16)

    val first = advances.advanceOf("Open file...", font, frc)
    val again = advances.advanceOf("Open file...", font, frc)

    again shouldBe first
    advances.measurements shouldBe 1L
  }

  it should "measure again for a different text or font" in {
    val advances = MeasuredTextAdvances(16)
    val _        = advances.advanceOf("Open file...", font, frc)

    advances.advanceOf("Open files...", font, frc)
    advances.advanceOf("Open file...", font.deriveFont(20.0f), frc)

    advances.measurements shouldBe 3L
  }

  it should "forget the oldest advances beyond its capacity" in {
    val advances = MeasuredTextAdvances(2)

    List("one", "two", "three").foreach(advances.advanceOf(_, font, frc))
    advances.advanceOf("three", font, frc)
    advances.advanceOf("one", font, frc)

    advances.measurements shouldBe 4L
  }
