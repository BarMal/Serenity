package com.serenity

import com.serenity.rope.{Rope, Balance}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnderscoreStorageSpec extends AnyFlatSpec with Matchers:

  "Rope" should "correctly store and retrieve underscore characters" in {
    given Balance = Balance.default
    
    val textWithUnderscores = "test_with_underscores"
    val rope = Rope(textWithUnderscores)
    
    rope.collect() shouldBe textWithUnderscores
    
    // Verify character by character
    rope.charAt(4) shouldBe '_'  // first underscore
    rope.charAt(9) shouldBe '_'  // second underscore
  }

  it should "correctly insert underscore characters" in {
    given Balance = Balance.default
    
    val originalText = "test"
    val rope = Rope(originalText)
    
    val updatedRope = rope.insert(4, "_added")
    updatedRope.collect() shouldBe "test_added"
    updatedRope.charAt(4) shouldBe '_'
  }

  it should "correctly retrieve lines containing underscores" in {
    given Balance = Balance.default
    
    val textWithUnderscores = "first_line\nsecond_line_with_more"
    val rope = Rope(textWithUnderscores)
    
    val firstLine = rope.getLine(0)
    firstLine shouldBe Some("first_line")
    
    val secondLine = rope.getLine(1)  
    secondLine shouldBe Some("second_line_with_more")
  }