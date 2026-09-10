package com.serenity.session

import scala.jdk.CollectionConverters.*

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import com.serenity.config.AppConfig
import com.serenity.testkit.ConfigGenerators
import io.circe.Json
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.slf4j.LoggerFactory

/** Session state used to carry a hand-written copy of what an `AppConfig` is, and sixteen settings were missing from it
  * -- pane headers and the viewport margins among them. A restored session replaces the running config wholesale, so
  * those settings went back to their defaults on every restore however the config file was set.
  *
  * The round trip is what makes that impossible rather than merely fixed: a setting the encoder does not write comes
  * back as its default, so the property fails. Its reach comes from `ConfigGenerators`, which a property of its own
  * holds to varying every field.
  */
class SessionConfigCodecSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with BeforeAndAfterEach:

  private val logger   = LoggerFactory.getLogger("com.serenity.session.SessionConfigCodec")
  private val appender = new ListAppender[ILoggingEvent]()

  override def beforeEach(): Unit =
    appender.list.clear()
    appender.start()
    logger.asInstanceOf[ch.qos.logback.classic.Logger].addAppender(appender)

  override def afterEach(): Unit =
    logger.asInstanceOf[ch.qos.logback.classic.Logger].detachAppender(appender)
    appender.stop()

  "SessionConfigCodec" should "return the config it was given" in
    forAll(ConfigGenerators.genAppConfig) { config =>
      SessionConfigCodec.decode(SessionConfigCodec.encode(config).hcursor) shouldBe config
    }

  it should "return the default config unchanged" in {
    SessionConfigCodec.decode(SessionConfigCodec.encode(AppConfig.default).hcursor) shouldBe AppConfig.default
  }

  // #1423: a composite field that fails to decode used to fall back to the existing value with nothing to show for it
  // -- indistinguishable from a session file that never set the field at all. It must still fall back (a hand-edited
  // typo cannot be allowed to fail the whole restore), but the fallback now has to be visible in the logs.
  it should "log a warning naming the field and value when a composite field fails to decode, and still fall back" in {
    val encoded = SessionConfigCodec.encode(AppConfig.default)
    val malformed = Json.fromJsonObject(
      encoded.asObject.getOrElse(fail("SessionConfigCodec.encode did not produce a JSON object")).add(
        "motionPreset",
        Json.fromString("not-a-real-motion-preset")
      )
    )

    val decoded = SessionConfigCodec.decode(malformed.hcursor)

    decoded shouldBe AppConfig.default

    val messages = appender.list.asScala.toList.filter(_.getLevel == Level.WARN).map(_.getFormattedMessage)
    messages should not be empty
    messages.exists(_.contains("motionPreset")) shouldBe true
    messages.exists(_.contains("not-a-real-motion-preset")) shouldBe true
  }

  it should "leave the config field untouched, not just logged, when a composite field is missing entirely" in {
    val encoded = SessionConfigCodec.encode(AppConfig.default)
    val withoutField = Json.fromJsonObject(
      encoded.asObject.getOrElse(fail("SessionConfigCodec.encode did not produce a JSON object")).remove(
        "motionPreset"
      )
    )

    SessionConfigCodec.decode(withoutField.hcursor) shouldBe AppConfig.default
    appender.list.asScala.toList.filter(_.getLevel == Level.WARN) shouldBe empty
  }
