package com.serenity.session

import com.serenity.config.AppConfig
import com.serenity.testkit.ConfigGenerators
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Session state used to carry a hand-written copy of what an `AppConfig` is, and sixteen settings were missing from it
  * -- pane headers and the viewport margins among them. A restored session replaces the running config wholesale, so
  * those settings went back to their defaults on every restore however the config file was set.
  *
  * The round trip is what makes that impossible rather than merely fixed: a setting the encoder does not write comes
  * back as its default, so the property fails. Its reach comes from `ConfigGenerators`, which a property of its own
  * holds to varying every field.
  */
class SessionConfigCodecSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  "SessionConfigCodec" should "return the config it was given" in
    forAll(ConfigGenerators.genAppConfig) { config =>
      SessionConfigCodec.decode(SessionConfigCodec.encode(config).hcursor) shouldBe config
    }

  it should "return the default config unchanged" in {
    SessionConfigCodec.decode(SessionConfigCodec.encode(AppConfig.default).hcursor) shouldBe AppConfig.default
  }
