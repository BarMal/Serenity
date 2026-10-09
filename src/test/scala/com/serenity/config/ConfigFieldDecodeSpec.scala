package com.serenity.config

import scala.jdk.CollectionConverters.*

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import com.serenity.testkit.LogbackLoggers
import io.circe.Json
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1432: [[SessionConfigCodec.SessionField.decode]] was given a warning log when a field fails to decode (#1423), but
  * its sibling [[ConfigField.decode]] -- which covers the larger share of registered [[AppConfig]] fields -- stayed
  * silent, discarding the `DecodingFailure` in its `_` branch with nothing to show for it. Restoring a single malformed
  * session file therefore warned for some fields and not others.
  *
  * These tests match the pattern `SessionConfigCodecSpec` uses for the #1423 fix: a `ListAppender` captures what the
  * field's logger emits, a malformed value must produce a WARN naming the field and the value, and a field the cursor
  * simply does not carry must stay silent -- the same "absent vs. present-but-malformed" distinction, since an `Option`
  * field would otherwise decode a missing key as `None` and every miss would look like a failure.
  */
class ConfigFieldDecodeSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach:

  private val logger   = LogbackLoggers.named("com.serenity.config.ConfigField")
  private val appender = new ListAppender[ILoggingEvent]()

  private val densityField: ConfigField[InterfaceDensity] =
    ConfigRegistry.fields
      .collectFirst {
        case field: ConfigField[InterfaceDensity @unchecked] if field.sessionKey == "interfaceDensity" => field
      }
      .getOrElse(fail("ConfigRegistry has no field with sessionKey 'interfaceDensity'"))

  override def beforeEach(): Unit =
    appender.list.clear()
    appender.start()
    logger.addAppender(appender)

  override def afterEach(): Unit =
    logger.detachAppender(appender)
    appender.stop()

  private def warnMessages: List[String] =
    appender.list.asScala.toList.filter(_.getLevel == Level.WARN).map(_.getFormattedMessage)

  "ConfigField.decode" should "log a warning naming the field and value when it fails to decode, and still fall back" in {
    val cursor = Json.obj(densityField.sessionKey -> Json.fromString("not-a-real-density")).hcursor

    val decoded = densityField.decode(cursor, AppConfig.default)

    decoded shouldBe AppConfig.default

    val messages = warnMessages
    messages should not be empty
    messages.exists(_.contains("interfaceDensity")) shouldBe true
    messages.exists(_.contains("not-a-real-density")) shouldBe true
  }

  it should "leave the config field untouched, not just logged, when the field is missing entirely" in {
    val cursor = Json.obj().hcursor

    val decoded = densityField.decode(cursor, AppConfig.default)

    decoded shouldBe AppConfig.default
    warnMessages shouldBe empty
  }

  it should "fall back to the given config rather than the field's default when decoding fails" in {
    val customized = densityField.set(AppConfig.default, InterfaceDensity.Compact)
    val cursor     = Json.obj(densityField.sessionKey -> Json.fromString("not-a-real-density")).hcursor

    val decoded = densityField.decode(cursor, customized)

    densityField.get(decoded) shouldBe InterfaceDensity.Compact
    warnMessages.exists(_.contains("interfaceDensity")) shouldBe true
  }
