package com.serenity

import com.serenity.config.WindowChromeMode
import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which chrome and translucency the window picks from the desktop session it was launched in. */
class SwingWindowChromeEnvironmentSpec extends AnyFlatSpec with Matchers:

  "SwingWindow.isTilingCompositor" should "detect the tiling compositors from their own sockets" in {
    SwingWindow.isTilingCompositor(Map("HYPRLAND_INSTANCE_SIGNATURE" -> "abc_123")) shouldBe true
    SwingWindow.isTilingCompositor(Map("SWAYSOCK" -> "/run/user/1000/sway-ipc.sock")) shouldBe true
    SwingWindow.isTilingCompositor(Map("I3SOCK" -> "/run/user/1000/i3/ipc-socket")) shouldBe true
    SwingWindow.isTilingCompositor(Map("NIRI_SOCKET" -> "/run/user/1000/niri.sock")) shouldBe true
  }

  it should "detect a tiling desktop named in XDG_CURRENT_DESKTOP, case-insensitively and in a list" in {
    SwingWindow.isTilingCompositor(Map("XDG_CURRENT_DESKTOP" -> "Hyprland")) shouldBe true
    SwingWindow.isTilingCompositor(Map("XDG_CURRENT_DESKTOP" -> "sway")) shouldBe true
    SwingWindow.isTilingCompositor(Map("XDG_CURRENT_DESKTOP" -> "niri")) shouldBe true
    SwingWindow.isTilingCompositor(Map("XDG_CURRENT_DESKTOP" -> "wlroots:river")) shouldBe true
  }

  it should "leave floating desktops and blank variables alone" in {
    SwingWindow.isTilingCompositor(Map.empty) shouldBe false
    SwingWindow.isTilingCompositor(Map("XDG_CURRENT_DESKTOP" -> "GNOME")) shouldBe false
    SwingWindow.isTilingCompositor(Map("XDG_CURRENT_DESKTOP" -> "ubuntu:GNOME")) shouldBe false
    SwingWindow.isTilingCompositor(Map("XDG_CURRENT_DESKTOP" -> "KDE")) shouldBe false
    SwingWindow.isTilingCompositor(Map("SWAYSOCK" -> "  ")) shouldBe false
  }

  "SwingWindow.isWaylandSession" should "follow WAYLAND_DISPLAY" in {
    SwingWindow.isWaylandSession(Map("WAYLAND_DISPLAY" -> "wayland-0")) shouldBe true
    SwingWindow.isWaylandSession(Map("WAYLAND_DISPLAY" -> "")) shouldBe false
    SwingWindow.isWaylandSession(Map("DISPLAY" -> ":0")) shouldBe false
  }

  "SwingWindow.shouldUseCustomChrome" should "hand Auto chrome to a tiling compositor on Linux" in {
    val hyprland = Map("HYPRLAND_INSTANCE_SIGNATURE" -> "abc")
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Auto, "Linux", hyprland) shouldBe false
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Auto, "Linux", Map("SWAYSOCK" -> "/tmp/s")) shouldBe false
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Auto, "Linux", Map("WAYLAND_DISPLAY" -> "w")) shouldBe true
  }

  it should "still honour an explicit chrome choice under a tiling compositor" in {
    val hyprland = Map("HYPRLAND_INSTANCE_SIGNATURE" -> "abc")
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Custom, "Linux", hyprland) shouldBe true
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Native, "Linux", hyprland) shouldBe false
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.NativeThemed, "Linux", hyprland) shouldBe false
  }

  it should "hand Auto chrome to the compositor under the native Wayland toolkit, which cannot move its own window" in {
    val gnome = Map("WAYLAND_DISPLAY" -> "wayland-0", "XDG_CURRENT_DESKTOP" -> "GNOME")
    SwingWindow.shouldUseCustomChrome(WindowChromeMode.Auto, "Linux", gnome, nativeWaylandToolkit = true) shouldBe false
    SwingWindow.shouldUseCustomChrome(
      WindowChromeMode.Custom,
      "Linux",
      gnome,
      nativeWaylandToolkit = true
    ) shouldBe true
  }

  "SwingWindow.isNativeWaylandToolkit" should "recognise the JetBrains Runtime Wayland toolkit by class" in {
    SwingWindow.isNativeWaylandToolkit("sun.awt.wl.WLToolkit") shouldBe true
    SwingWindow.isNativeWaylandToolkit("sun.awt.X11.XToolkit") shouldBe false
  }

  "SwingWindow.usesTranslucentWindow" should "default to opaque on Wayland and translucent elsewhere" in {
    val wayland = Map("WAYLAND_DISPLAY" -> "wayland-1")
    val x11     = Map("DISPLAY" -> ":0")
    SwingWindow.usesTranslucentWindow(None, wayland, perPixelTranslucencySupported = true) shouldBe false
    SwingWindow.usesTranslucentWindow(None, x11, perPixelTranslucencySupported = true) shouldBe true
  }

  it should "let the setting override the session default either way" in {
    val wayland = Map("WAYLAND_DISPLAY" -> "wayland-1")
    val x11     = Map("DISPLAY" -> ":0")
    SwingWindow.usesTranslucentWindow(Some(true), wayland, perPixelTranslucencySupported = true) shouldBe true
    SwingWindow.usesTranslucentWindow(Some(false), x11, perPixelTranslucencySupported = true) shouldBe false
  }

  it should "never be translucent without platform per-pixel translucency" in {
    SwingWindow.usesTranslucentWindow(Some(true), Map.empty, perPixelTranslucencySupported = false) shouldBe false
    SwingWindow.usesTranslucentWindow(None, Map.empty, perPixelTranslucencySupported = false) shouldBe false
  }

  "an opaque window" should "neither mask rounded corners nor paint transparent content" in {
    val translucent = SwingWindow.usesTranslucentWindow(None, Map("WAYLAND_DISPLAY" -> "w"), true)
    SwingWindow.shouldUsePerPixelRoundedCorners(
      usesCustomChrome = true,
      maximized = false,
      perPixelTranslucencySupported = translucent
    ) shouldBe false
    SwingWindow.shouldPaintTransparentContent(
      usesCustomChrome = true,
      perPixelTranslucencySupported = translucent,
      backgroundAlpha = 0
    ) shouldBe false
  }
end SwingWindowChromeEnvironmentSpec
