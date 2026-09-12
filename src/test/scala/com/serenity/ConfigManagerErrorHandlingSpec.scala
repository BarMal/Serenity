package com.serenity

import java.nio.file.Files

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import com.serenity.config.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

/** Load/save error handling: the effectful API, structured migration reports, unparseable/missing files, and the
  * logging behaviour of the synchronous load and save paths on failure.
  */
class ConfigManagerErrorHandlingSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "load configuration through the effectful blocking-safe API" in {
    val configFile = Files.createTempFile("serenity-config-io", ".conf")
    Files.writeString(
      configFile,
      """syntax.highlighting = true
        |font.code.size = 18.0
        |""".stripMargin
    )

    val config = ConfigManager.loadConfigIO(Some(configFile.toString)).unsafeRunSync()

    config.languageToolsConfig.syntaxHighlightingEnabled shouldBe true
    config.editorConfig.fontConfig.fontSize shouldBe 18.0f
  }

  it should "load configuration with a structured migration report" in {
    val configFile = Files.createTempFile("serenity-config-result", ".conf")
    Files.writeString(
      configFile,
      """font_code_size = 18.0
        |unknown.setting = yes
        |syntax.highlighting = maybe
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.config.editorConfig.fontConfig.codeFontSize shouldBe 18.0f
    result.report.deprecatedEntries.map(_.key) should contain("font_code_size")
    result.report.deprecatedEntries.map(_.replacement) should contain("font.code.size")
    result.report.unknownKeys should contain("unknown.setting")
    result.report.invalidEntries.map(_.key) should contain("syntax.highlighting")
    result.report.hasWarnings shouldBe true
  }

  it should "fall back to defaults without throwing when a config file cannot be parsed" in {
    val configFile = Files.createTempFile("serenity-unparseable-sync-config", ".conf")
    Files.writeString(configFile, "this is not = valid = hocon {\n")

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.config shouldBe AppConfig.default
    result.report shouldBe ConfigMigrationReport.empty
  }

  it should "return default config result with an empty report when the config file is missing" in {
    val missingConfig = Files.createTempDirectory("serenity-missing-config-result").resolve("missing.conf")

    val result = ConfigManager.loadConfigResult(Some(missingConfig.toString))

    result.config shouldBe AppConfig.default
    result.report.hasWarnings shouldBe false
  }

  it should "return defaults through the effectful API when the config file is missing" in {
    val missingConfig = Files.createTempDirectory("serenity-missing-config").resolve("missing.conf")

    ConfigManager.loadConfigIO(Some(missingConfig.toString)).unsafeRunSync() shouldBe AppConfig.default
  }

  it should "return structured errors at the effectful configuration boundary" in {
    val invalidFile = Files.createTempFile("serenity-invalid-hocon", ".conf")
    Files.writeString(invalidFile, "font.code.size = [not-a-number]\n")

    ConfigManager.loadConfigResultIO(Some(invalidFile.toString)).unsafeRunSync() match
      case Left(error)  => error.message should include("font.code.size")
      case Right(value) => fail(s"expected a structured load error, received $value")

    val directoryPath = Files.createTempDirectory("serenity-save-error")
    ConfigManager.saveConfigIO(AppConfig.default, directoryPath).unsafeRunSync() match
      case Left(error) => error.path shouldBe directoryPath
      case Right(_)    => fail("expected a structured save error")
  }

  it should "log the real cause instead of silently discarding it when the synchronous save fails" in {
    // Prior to this test, ConfigManager.saveConfig's `catch case _: Exception => false` swallowed the underlying
    // exception entirely -- the caller got `false` and nothing else was ever recorded anywhere.
    val logger   = LoggerFactory.getLogger("com.serenity.config.ConfigManager")
    val appender = new ListAppender[ILoggingEvent]()
    appender.start()
    logger.asInstanceOf[ch.qos.logback.classic.Logger].addAppender(appender)
    try
      val directoryPath = Files.createTempDirectory("serenity-sync-save-error")

      ConfigManager.saveConfig(AppConfig.default, directoryPath) shouldBe false

      val errorEvents = appender.list.asScala.toList.filter(_.getLevel == Level.ERROR)
      errorEvents should not be empty
      errorEvents.exists(_.getFormattedMessage.contains(directoryPath.toString)) shouldBe true
      errorEvents.exists(event => Option(event.getThrowableProxy).isDefined) shouldBe true
    finally
      logger.asInstanceOf[ch.qos.logback.classic.Logger].detachAppender(appender)
      appender.stop()
  }

  it should "narrow the synchronous load's catch to non-fatal failures, matching the IO-based load path" in {
    val configFile = Files.createTempFile("serenity-sync-load-error", ".conf")
    // Not "key = value" shaped, so the legacy-format reader (which never throws -- see `parseLegacyConfig`) declines
    // it, and it falls through to a real HOCON parse of unparseable syntax, which does throw.
    Files.writeString(configFile, "this is not valid hocon at all {{{\n")

    val logger   = LoggerFactory.getLogger("com.serenity.config.ConfigManager")
    val appender = new ListAppender[ILoggingEvent]()
    appender.start()
    logger.asInstanceOf[ch.qos.logback.classic.Logger].addAppender(appender)
    try
      val result = ConfigManager.loadConfigResult(Some(configFile.toString))

      result.config shouldBe AppConfig.default

      val errorEvents = appender.list.asScala.toList.filter(_.getLevel == Level.ERROR)
      errorEvents should not be empty
      errorEvents.exists(event => Option(event.getThrowableProxy).isDefined) shouldBe true
    finally
      logger.asInstanceOf[ch.qos.logback.classic.Logger].detachAppender(appender)
      appender.stop()
  }

  it should "validate hotkey, keymap, and LSP entries through the structured load API" in {
    val invalidFile = Files.createTempFile("serenity-invalid-bindings", ".conf")
    Files.writeString(
      invalidFile,
      """hotkey.save = [not-a-real-trigger]
        |keymap.command_runner.submit = not-a-real-trigger
        |lsp.python.enabled = maybe
        |""".stripMargin
    )

    ConfigManager.loadConfigResultIO(Some(invalidFile.toString)).unsafeRunSync() match
      case Left(error) =>
        error.message should include("hotkey.save")
        error.message should include("keymap.command_runner.submit")
        error.message should include("lsp.python.enabled")
      case Right(value) => fail(s"expected binding validation errors, received $value")
  }
