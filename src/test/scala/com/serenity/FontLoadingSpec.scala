package com.serenity

import cats.effect.{IO, unsafe}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.effect.unsafe.implicits.global

class FontLoadingSpec extends AnyFlatSpec with Matchers:

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "FontLoader" should "load system fonts when custom fonts disabled" in {
    val config = FontConfig(useCustomFont = false)
    val fonts = FontLoader.loadMonaspaceNeon(config).unsafeRunSync()
    fonts.should(not).be(empty)
    fonts.head.getName.should(not).be(empty)
  }

  it should "attempt to load Monaspace fonts when custom fonts enabled" in {
    val config = FontConfig(useCustomFont = true, fontSize = 12.0f)
    val fonts = FontLoader.loadMonaspaceNeon(config).unsafeRunSync()
    fonts.should(not).be(empty)
  }

  it should "handle missing font resources gracefully" in {
    val config = FontConfig(useCustomFont = true)
    // This should fall back to system fonts if Monaspace fonts can't be loaded
    val result = FontLoader.loadMonaspaceNeon(config)
    noException.should(be).thrownBy(result.unsafeRunSync())
  }