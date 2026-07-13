package com.serenity.ui.terminal

import java.awt.{Color, Window}

import scala.util.control.NonFatal

import com.sun.jna.platform.win32.WinDef
import com.sun.jna.ptr.IntByReference
import com.sun.jna.{Native, NativeLibrary}

/** Applies optional Windows DWM caption colours without replacing the OS-owned window frame. */
object WindowsNativeChrome:

  private val UseImmersiveDarkMode = 20
  private val BorderColor          = 34
  private val CaptionColor         = 35
  private val TextColor            = 36

  private lazy val setWindowAttribute =
    NativeLibrary.getInstance("dwmapi").getFunction("DwmSetWindowAttribute")

  /** Whether the current platform can receive native themed-chrome attributes. */
  def isSupported(osName: String = System.getProperty("os.name", "")): Boolean =
    osName.toLowerCase(java.util.Locale.ROOT).contains("windows")

  /** Apply a theme to the native Windows title bar, returning false when the platform or DWM call is unavailable. */
  def apply(window: Window, palette: SwingWindow.ChromePalette): Boolean =
    if !isSupported() then false
    else
      try
        val hwnd = new WinDef.HWND(Native.getComponentPointer(window))
        val dark = luminance(palette.titleBackground) < 0.5
        setAttribute(hwnd, UseImmersiveDarkMode, if dark then 1 else 0) &&
        setAttribute(hwnd, BorderColor, colorRef(palette.border)) &&
        setAttribute(hwnd, CaptionColor, colorRef(palette.titleBackground)) &&
        setAttribute(hwnd, TextColor, colorRef(palette.titleForeground))
      catch case NonFatal(_) => false

  private def setAttribute(hwnd: WinDef.HWND, attribute: Int, value: Int): Boolean =
    val reference = new IntByReference(value)
    setWindowAttribute.invokeInt(
      Array(hwnd, Int.box(attribute), reference.getPointer, Int.box(Integer.BYTES))
    ) == 0

  private def colorRef(color: Color): Int =
    color.getRed | (color.getGreen << 8) | (color.getBlue << 16)

  private def luminance(color: Color): Double =
    (0.2126 * color.getRed + 0.7152 * color.getGreen + 0.0722 * color.getBlue) / 255.0
