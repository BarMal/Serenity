package com.serenity.ui.sdl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Scala constants must equal what the SDL headers say (`ci/sdl3/layout.c`): a wrong offset in an FFM binding is
  * silent memory corruption, so it is a failing test instead.
  */
class SdlLayoutsSpec extends AnyFlatSpec with Matchers:

  "SdlLayouts" should "name exactly the entries the SDL headers print" in {
    SdlLayouts.entries.keySet.diff(SdlGolden.entries.keySet) shouldBe empty
    SdlGolden.entries.keySet.diff(SdlLayouts.entries.keySet) shouldBe empty
  }

  it should "agree with the headers on every offset, size and constant" in {
    val wrong = SdlGolden.entries.collect {
      case (name, expected) if !SdlLayouts.entries.get(name).contains(expected) =>
        s"$name: header says $expected, binding says ${SdlLayouts.entries.get(name)}"
    }

    wrong shouldBe empty
  }
