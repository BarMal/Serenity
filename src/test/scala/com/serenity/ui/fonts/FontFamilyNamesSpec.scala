package com.serenity.ui.fonts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The family names the platform reports are the one input to the settings font lists that we do not control. */
class FontFamilyNamesSpec extends AnyFlatSpec with Matchers:

  "FontLoader.usableFamilyNames" should "drop a null name the platform reports and sort the rest" in {
    FontLoader.usableFamilyNames(Array("Menlo", null, "Arial", null)) shouldBe List("Arial", "Menlo")
  }

  it should "keep every name when none is null" in {
    FontLoader.usableFamilyNames(Array("Courier", "Arial", "Zapfino")) shouldBe List("Arial", "Courier", "Zapfino")
  }

  it should "report nothing when the platform reports nothing" in {
    FontLoader.usableFamilyNames(Array.empty[String]) shouldBe Nil
  }

  "Sorting the names as reported" should "throw on a null name, which is why they are filtered first" in {
    an[NullPointerException] should be thrownBy Array("Menlo", null, "Arial").toList.sorted
  }
