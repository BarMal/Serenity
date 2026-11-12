package com.serenity

import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import com.serenity.richtext.InlineMark
import com.serenity.session.given
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A saved session names marks by their spelling, so the new mark must not disturb the old ones. */
class ShadowMarkSessionCodecSpec extends AnyFlatSpec with Matchers:

  "The InlineMark session codec" should "write and read the Shadow mark" in {
    InlineMark.Shadow.asJson.noSpaces shouldBe "\"Shadow\""
    decode[InlineMark]("\"Shadow\"") shouldBe Right(InlineMark.Shadow)
  }

  it should "still read every mark a session saved before Shadow existed" in {
    List("Bold", "Italic", "Underline").map(name => decode[InlineMark](s"\"$name\"")) shouldBe
      List(Right(InlineMark.Bold), Right(InlineMark.Italic), Right(InlineMark.Underline))
  }

  it should "still refuse a mark it does not know" in {
    decode[InlineMark]("\"Sparkle\"").isLeft shouldBe true
  }
end ShadowMarkSessionCodecSpec
