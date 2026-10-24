package com.serenity.ui.host

import cats.effect.IO
import com.serenity.config.PreferredWindowSize
import com.serenity.state.models.PointerShape
import com.serenity.ui.accessibility.AccessibilitySnapshot
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.theme.Theme

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
