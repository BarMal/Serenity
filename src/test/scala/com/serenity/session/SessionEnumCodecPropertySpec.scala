package com.serenity.session

import _root_.io.circe.syntax.*
import _root_.io.circe.{Decoder, Encoder}
import com.serenity.config.*
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Round-trip coverage for every session-persisted enum that carries a `configKey`.
  *
  * Each property iterates the enum's own `values` array rather than a hand-picked sample, so adding a new case to one
  * of these enums without extending its codec surfaces here as a failing test instead of silently producing an enum
  * value that fails to decode (or, worse, one that decodes as some other case).
  */
class SessionEnumCodecPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  private def roundTripsAllCases[A](name: String, values: Array[A], configKey: A => String)(using
    enc: Encoder[A],
    dec: Decoder[A]
  ): Unit =
    property(s"$name round-trips every case through its configKey-derived codec") {
      forAll(Gen.oneOf(values.toIndexedSeq)) { value =>
        val encoded = value.asJson

        encoded shouldBe configKey(value).asJson
        encoded.as[A] shouldBe Right(value)
      }
    }

    property(s"$name decodes every case's legacy toString spelling") {
      forAll(Gen.oneOf(values.toIndexedSeq))(value => value.toString.asJson.as[A] shouldBe Right(value))
    }

  roundTripsAllCases("CursorMode", CursorMode.values, _.configKey)
  roundTripsAllCases("CursorInfoBarMode", CursorInfoBarMode.values, _.configKey)
  roundTripsAllCases("CursorInfoBarPlacement", CursorInfoBarPlacement.values, _.configKey)
  roundTripsAllCases("WindowChromeMode", WindowChromeMode.values, _.configKey)
  roundTripsAllCases("MarkdownViewMode", MarkdownViewMode.values, _.configKey)
  roundTripsAllCases("DefaultDocumentMode", DefaultDocumentMode.values, _.configKey)
  roundTripsAllCases("InterfaceDensity", InterfaceDensity.values, _.configKey)
  roundTripsAllCases("MaterialPreset", MaterialPreset.values, _.configKey)
  roundTripsAllCases("MotionPreset", MotionPreset.values, _.configKey)
  roundTripsAllCases("MotionAccessibility", MotionAccessibility.values, _.configKey)
  roundTripsAllCases("MotionFamily", MotionFamily.values, _.configKey)
