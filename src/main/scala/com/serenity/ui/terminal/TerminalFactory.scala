package com.serenity.ui.terminal

import cats.effect.IO
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.typelevel.log4cats.Logger

object TerminalFactory:

  /** Create terminal with optional font configuration for GUI environments */
  def createTerminal(fontConfig: FontConfig)(using logger: Logger[IO]): IO[Terminal] =
    for
      _ <- logger.debug("Creating terminal with font configuration")
      isHeadless = java.awt.GraphicsEnvironment.isHeadless
      terminal <- if isHeadless then createHeadlessTerminal() else createTerminalWithFontSupport(fontConfig)
      _        <- logger.info(s"Created ${if isHeadless then "headless" else "GUI"} terminal")
    yield terminal

  /** Create headless terminal (standard console) */
  private def createHeadlessTerminal()(using Logger[IO]): IO[Terminal] =
    IO.blocking {
      new DefaultTerminalFactory().createTerminal()
    }

  /** Create terminal with font support where possible */
  private def createTerminalWithFontSupport(fontConfig: FontConfig)(using logger: Logger[IO]): IO[Terminal] =
    for
      fonts <- FontLoader.loadMonaspaceNeon(fontConfig)
      _     <- logger.debug(s"Loaded ${fonts.length} fonts for terminal")
      factory = new DefaultTerminalFactory()
      _       = factory.setForceTextTerminal(false) // Allow GUI terminal
      terminal <- IO.blocking {
        // For now, use the default factory
        // In the future, we can add SwingWindow customization here
        // when we understand the exact API for the Lanterna version we're using
        val terminal = factory.createTerminal()

        terminal
      }
    yield terminal

  /** Check if we're running in a GUI environment */
  def isGuiEnvironment: Boolean =
    !java.awt.GraphicsEnvironment.isHeadless

  /** Force headless mode (useful for testing) */
  def forceHeadless(): Unit =
    System.setProperty("java.awt.headless", "true")
