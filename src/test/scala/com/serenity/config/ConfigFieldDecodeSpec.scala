package com.serenity.config

import scala.jdk.CollectionConverters.*

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import io.circe.Json
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

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

  private val logger   = LoggerFactory.getLogger("com.serenity.config.ConfigField")
  private val appender = new ListAppender[ILoggingEvent]()

  private val materialPresetField: ConfigField[MaterialPreset] =
    ConfigRegistry.fields
      .collectFirst {
        case field: ConfigField[MaterialPreset @unchecked] if field.sessionKey == "materialPreset" => field
      }
      .getOrElse(fail("ConfigRegistry has no field with sessionKey 'materialPreset'"))

  override def beforeEach(): Unit =
    appender.list.clear()
    appender.start()
    logger.asInstanceOf[ch.qos.logback.classic.Logger].addAppender(appender)

  override def afterEach(): Unit =
    logger.asInstanceOf[ch.qos.logback.classic.Logger].detachAppender(appender)
    appender.stop()

  private def warnMessages: List[String] =
    appender.list.asScala.toList.filter(_.getLevel == Level.WARN).map(_.getFormattedMessage)

  "ConfigField.decode" should "log a warning naming the field and value when it fails to decode, and still fall back" in {
    val cursor = Json.obj(materialPresetField.sessionKey -> Json.fromString("not-a-real-material-preset")).hcursor

    val decoded = materialPresetField.decode(cursor, AppConfig.default)

    decoded shouldBe AppConfig.default

    val messages = warnMessages
    messages should not be empty
    messages.exists(_.contains("materialPreset")) shouldBe true
    messages.exists(_.contains("not-a-real-material-preset")) shouldBe true
  }

  it should "leave the config field untouched, not just logged, when the field is missing entirely" in {
    val cursor = Json.obj().hcursor

    val decoded = materialPresetField.decode(cursor, AppConfig.default)

    decoded shouldBe AppConfig.default
    warnMessages shouldBe empty
  }

  it should "fall back to the given config rather than the field's default when decoding fails" in {
    val customized = materialPresetField.set(AppConfig.default, MaterialPreset.Crystal)
    val cursor     = Json.obj(materialPresetField.sessionKey -> Json.fromString("not-a-real-material-preset")).hcursor

    val decoded = materialPresetField.decode(cursor, customized)

    materialPresetField.get(decoded) shouldBe MaterialPreset.Crystal
    warnMessages.exists(_.contains("materialPreset")) shouldBe true
  }
