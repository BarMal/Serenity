package com.serenity.input

import java.nio.charset.StandardCharsets
import java.util.Base64

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Osc52Spec extends AnyFlatSpec with Matchers:

  private val esc = 0x1b.toChar
  private val bel = 0x07.toChar

  "Osc52.encode" should "frame a base64 payload between OSC 52 and BEL" in {
    val expectedPayload = Base64.getEncoder.encodeToString("hello".getBytes(StandardCharsets.UTF_8))
    Osc52.encode("hello") shouldBe Right(s"$esc]52;c;$expectedPayload$bel")
  }

  it should "base64-encode UTF-8 bytes, not the raw string" in {
    val expectedPayload = Base64.getEncoder.encodeToString("héllo".getBytes(StandardCharsets.UTF_8))
    Osc52.encode("héllo") shouldBe Right(s"$esc]52;c;$expectedPayload$bel")
  }

  it should "accept a payload exactly at the size limit" in {
    val payload = Base64.getEncoder.encodeToString("ab".getBytes(StandardCharsets.UTF_8))
    Osc52.encode("ab", maxEncodedBytes = payload.length) shouldBe Right(s"$esc]52;c;$payload$bel")
  }

  it should "report an oversized payload rather than truncating it" in {
    val payload = Base64.getEncoder.encodeToString("abc".getBytes(StandardCharsets.UTF_8))
    Osc52.encode("abc", maxEncodedBytes = payload.length - 1) shouldBe
      Left(Osc52.PayloadTooLarge(payload.length, payload.length - 1))
  }
