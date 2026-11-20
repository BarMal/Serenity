package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Courier Prime and Monaspace lay out exactly as they did when AWT measured them: wrap rows, caret xs and line heights
  * of a fixed multi-line fixture are bit for bit the numbers pinned before layout moved to the Skia measurer.
  */
class MonospacedLayoutPinSpec extends AnyFlatSpec with Matchers:

  "The bundled monospaced fonts" should "lay the fixture out exactly as the pinned AWT-era layout" in {
    val pinned = MonospacedLayoutPin.load().getOrElse(fail(s"no ${MonospacedLayoutPin.Resource} on the classpath"))
    val now    = MonospacedLayoutPin.entries()

    MonospacedLayoutPin.fonts.map(_.label) shouldBe MonospacedLayoutPin.Labels
    val moved = pinned.zipAll(now, "-" -> "-", "-" -> "-").filter(_ != _)
    withClue(moved.take(20).map((was, is) => s"pinned $was, now $is").mkString("\n")) {
      now shouldBe pinned
    }
  }
