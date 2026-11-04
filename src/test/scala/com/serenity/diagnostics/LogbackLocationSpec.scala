package com.serenity.diagnostics

import java.nio.file.Path

import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.FileAppender
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogbackLocationSpec extends AnyFlatSpec with Matchers:

  private def appFileOf(context: LoggerContext): Option[String] =
    context.getLogger("ROOT").getAppender("APP_FILE") match
      case file: FileAppender[?] => Some(file.getFile)
      case _                     => None

  "The packaged logback.xml" should "write the app log under the platform log directory" in {
    val chosen   = TestTemp.directory("serenity-logback")
    val previous = Option(System.getProperty(LogLocation.OverrideProperty))
    System.setProperty(LogLocation.OverrideProperty, chosen.toString)
    try
      val context      = LoggerContext()
      val configurator = JoranConfigurator()
      configurator.setContext(context)
      configurator.doConfigure(getClass.getClassLoader.getResource("logback.xml"))

      appFileOf(context).map(Path.of(_)) shouldBe Some(chosen.resolve("serenity.log"))
    finally
      previous.fold(System.clearProperty(LogLocation.OverrideProperty))(
        System.setProperty(LogLocation.OverrideProperty, _)
      )
      ()
  }

  // Same suite as the location case above: both set JVM-wide system properties, which suites running in parallel
  // would trample.
  private val LevelProperty = "SERENITY_LOG_LEVEL"

  private def rootLevelWith(level: Option[String]): Level =
    val logDirectory     = TestTemp.directory("serenity-logback-level")
    val previousLevel    = Option(System.getProperty(LevelProperty))
    val previousLocation = Option(System.getProperty(LogLocation.OverrideProperty))
    System.setProperty(LogLocation.OverrideProperty, logDirectory.toString)
    level.fold(System.clearProperty(LevelProperty))(System.setProperty(LevelProperty, _))
    try
      val context      = LoggerContext()
      val configurator = JoranConfigurator()
      configurator.setContext(context)
      configurator.doConfigure(getClass.getClassLoader.getResource("logback.xml"))
      val resolved = context.getLogger("ROOT").getLevel
      context.stop()
      resolved
    finally
      previousLevel.fold(System.clearProperty(LevelProperty))(System.setProperty(LevelProperty, _))
      previousLocation.fold(System.clearProperty(LogLocation.OverrideProperty))(
        System.setProperty(LogLocation.OverrideProperty, _)
      )
      ()

  it should "log at INFO when SERENITY_LOG_LEVEL is not set" in {
    rootLevelWith(None) shouldBe Level.INFO
  }

  it should "log at DEBUG when SERENITY_LOG_LEVEL asks for it, so the state-change chatter can be turned on" in {
    rootLevelWith(Some("DEBUG")) shouldBe Level.DEBUG
  }

  it should "quieten to WARN when SERENITY_LOG_LEVEL asks for that" in {
    rootLevelWith(Some("WARN")) shouldBe Level.WARN
  }
