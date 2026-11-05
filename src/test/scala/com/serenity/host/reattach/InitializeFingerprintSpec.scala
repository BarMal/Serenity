package com.serenity.host.reattach

import com.serenity.host.reattach.ReattachFixtures.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InitializeFingerprintSpec extends AnyFlatSpec with Matchers:

  private def json(text: String): Json = parse(text).fold(throw _, identity)

  "InitializeFingerprint" should "ignore the client's process id, which differs on every launch" in {
    InitializeFingerprint.of(initializeParams(processId = 1)) shouldBe InitializeFingerprint.of(
      initializeParams(processId = 2)
    )
  }

  it should "ignore a work-done token, which is per request" in {
    val withToken = initializeParams().mapObject(_.add("workDoneToken", "t-1".asJson))

    InitializeFingerprint.of(withToken) shouldBe InitializeFingerprint.of(initializeParams())
  }

  it should "change when the capabilities, the root or the options change" in {
    val base = InitializeFingerprint.of(initializeParams(capabilities = Json.obj("a" -> 1.asJson)))

    InitializeFingerprint.of(initializeParams(capabilities = Json.obj("a" -> 2.asJson))) should not be base
    InitializeFingerprint.of(
      initializeParams(capabilities = Json.obj("a" -> 1.asJson)).mapObject(_.add("rootUri", "file:///other".asJson))
    ) should not be base
    InitializeFingerprint.of(
      initializeParams(capabilities = Json.obj("a" -> 1.asJson))
        .mapObject(_.add("initializationOptions", Json.obj("x" -> true.asJson)))
    ) should not be base
  }

  it should "not depend on the order of object keys, however deeply nested" in {
    val one = json("""{"capabilities":{"a":{"x":1,"y":2},"b":[{"p":1,"q":2}]},"rootUri":"file:///w"}""")
    val two = json("""{"rootUri":"file:///w","capabilities":{"b":[{"q":2,"p":1}],"a":{"y":2,"x":1}}}""")

    InitializeFingerprint.of(one) shouldBe InitializeFingerprint.of(two)
  }

  it should "keep the order of array elements, which is meaningful" in {
    InitializeFingerprint.of(json("""{"folders":["a","b"]}""")) should not be InitializeFingerprint.of(
      json("""{"folders":["b","a"]}""")
    )
  }

  it should "be 64 lowercase hex characters" in {
    InitializeFingerprint.of(initializeParams()) should fullyMatch regex "[0-9a-f]{64}"
  }
