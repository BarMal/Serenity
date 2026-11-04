package com.serenity.diagnostics

import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.classic.joran.JoranConfigurator
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Console and file logging stays at INFO for user-relevant events; `SERENITY_LOG_LEVEL` lets anyone turn the DEBUG
  * chatter (per-command, per-modal state changes) on without a rebuild (#2570).
  */
class LogbackLevelSpec extends AnyFlatSpec with Matchers:

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

  "The packaged logback.xml" should "log at INFO when SERENITY_LOG_LEVEL is not set" in {
    rootLevelWith(None) shouldBe Level.INFO
  }

  it should "log at DEBUG when SERENITY_LOG_LEVEL asks for it" in {
    rootLevelWith(Some("DEBUG")) shouldBe Level.DEBUG
  }

  it should "quieten to WARN when SERENITY_LOG_LEVEL asks for that" in {
    rootLevelWith(Some("WARN")) shouldBe Level.WARN
  }
