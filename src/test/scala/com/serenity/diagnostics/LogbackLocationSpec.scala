package com.serenity.diagnostics

import java.nio.file.Path

import ch.qos.logback.classic.LoggerContext
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
