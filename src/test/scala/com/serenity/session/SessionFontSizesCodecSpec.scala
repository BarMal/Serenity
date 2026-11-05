package com.serenity.session

import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.fonts.FontSizes
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2405: the sizes the last size change replaced survive a restored session, and a session written before they were
  * kept still loads.
  */
class SessionFontSizesCodecSpec extends AnyFlatSpec with Matchers:

  "A session's font config" should "carry the previous sizes through a round trip" in {
    val fonts = FontConfig(previousSizes = Some(FontSizes(36.0f, 12.0f, 11.0f)))

    decode[FontConfig](fonts.asJson.noSpaces) shouldBe Right(fonts)
  }

  it should "carry none through a round trip" in {
    decode[FontConfig](FontConfig().asJson.noSpaces) shouldBe Right(FontConfig())
  }

  it should "load without previous sizes from a session written before they were kept" in {
    decode[FontConfig]("""{"textFontSize": 20.0}""").map(_.previousSizes) shouldBe Right(None)
  }

  it should "clamp previous sizes outside the range sizes accept" in {
    decode[FontConfig]("""{"previousSizes": {"prose": 60.0, "code": 2.0, "ui": 11.0}}""").map(_.previousSizes) shouldBe
      Right(Some(FontSizes(48.0f, 8.0f, 11.0f)))
  }
