package com.serenity

import com.serenity.config.WindowChromeMode
import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which chrome the window picks from the desktop session it was launched in. */
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
end SwingWindowChromeEnvironmentSpec
