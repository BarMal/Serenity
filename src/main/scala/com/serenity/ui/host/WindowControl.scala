package com.serenity.ui.host

import scala.annotation.unused

import cats.effect.IO
import com.serenity.config.PreferredWindowSize
import com.serenity.input.FileDropHandler
import com.serenity.state.models.PointerShape
import com.serenity.ui.accessibility.AccessibilitySnapshot
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.theme.Theme
import org.typelevel.log4cats.Logger

/** What the application asks of a window as a window: its lifecycle, its size, the platform's view of the display and
  * the cursor. Nothing here is specific to one window system.
  */
trait WindowControl:

  /** Completes once the user has asked to close the window. */
  def awaitClose: IO[Unit]

  def setOnResize(cb: () => Unit): Unit

  /** Registers a callback fired with `true` when the window gains keyboard focus and `false` when it loses it. */
  def setOnFocusChange(cb: Boolean => Unit): Unit

  /** The size the window was last resized to, once, until it changes again. */
  def doResizeIfNecessary(): Option[ViewportSize]

  /** Takes the window off the screen without waiting for it to be released; calling it again does nothing. */
  def hide(): Unit

  /** Raises the window for a later launch that handed its files over to this one. */
  def bringToFront(): Unit

  def currentPreferredWindowSize: PreferredWindowSize
  def resizeToPreferred(size: PreferredWindowSize): Unit

  def updateMetrics(codeMetrics: CellMetrics, uiMetrics: CellMetrics): Unit
  def updateChromeTheme(theme: Theme): Unit
  def updatePointerShape(shape: PointerShape): Unit
  def updateAccessibility(snapshot: AccessibilitySnapshot): Unit

  /** Ends the input method's composition from the platform's side. */
  def endInputMethodComposition(): Unit

  /** The scale the display asks text to be drawn at. */
  def detectedDeviceTextScale: Double

  /** Logs what the first frames did on screen; a window that keeps no such record logs nothing. */
  def reportFirstPaints(@unused logger: Logger[IO]): IO[Unit] = IO.unit

  /** Makes the window take file drops; a window that cannot be dropped on ignores the handler. */
  def installFileDrop(@unused handler: FileDropHandler): Unit = ()
