package com.serenity.ui.theme.config

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import com.serenity.testkit.LogbackLoggers
import com.serenity.ui.theme.DefaultThemes
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The built-in "transparent" theme is gone, but a session saved while it was selected still names it. */
class RemovedThemeFallbackSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach:

  private val logger   = LogbackLoggers.named(classOf[AppThemeManager].getName)
  private val appender = new ListAppender[ILoggingEvent]()

  override def beforeEach(): Unit =
    appender.list.clear()
    appender.start()
    logger.addAppender(appender)

  override def afterEach(): Unit =
    logger.detachAppender(appender)
    appender.stop()

  private def warnings: List[String] =
    appender.list.asScala.toList.filter(_.getLevel == Level.WARN).map(_.getFormattedMessage)

  "The transparent theme" should "no longer be a built-in theme" in {
    DefaultThemes.allInternal.keySet should not contain "transparent"
  }

  "Starting with the removed transparent theme selected" should "fall back to the default theme with one warning" in {
    val manager = AppThemeManager.create
    val theme   = manager.initializeWithTheme("transparent").unsafeRunSync()

    theme shouldBe DefaultThemes.default
    manager.getCurrentTheme.unsafeRunSync() shouldBe Some(DefaultThemes.default)
    warnings should have size 1
    warnings.headOption.getOrElse("") should (include("transparent") and include("removed"))
  }

  "Starting with an available theme" should "not warn" in {
    AppThemeManager.create.initializeWithTheme("default-light").unsafeRunSync() shouldBe DefaultThemes.defaultLight
    warnings shouldBe Nil
  }
