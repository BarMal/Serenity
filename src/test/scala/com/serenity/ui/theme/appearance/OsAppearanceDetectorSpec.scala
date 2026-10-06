package com.serenity.ui.theme.appearance

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OsAppearanceDetectorSpec extends AnyFlatSpec with Matchers:

  private def runnerReturning(outputs: Map[List[String], ProcessOutput]): ProcessRunner =
    command => IO.pure(outputs.get(command))

  private val macStyle    = List("defaults", "read", "-g", "AppleInterfaceStyle")
  private val macContrast = List("defaults", "read", "com.apple.universalaccess", "increaseContrast")

  private val winTheme = List(
    "reg",
    "query",
    "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
    "/v",
    "AppsUseLightTheme"
  )

  private val winContrast = List("reg", "query", "HKCU\\Control Panel\\Accessibility\\HighContrast", "/v", "Flags")
  private val gsettingsScheme =
    List("gsettings", "get", "org.gnome.desktop.interface", "color-scheme")
  private val gsettingsGtk = List("gsettings", "get", "org.gnome.desktop.interface", "gtk-theme")

  "the detector" should "ask macOS for the interface style and the increase-contrast setting" in {
    val runner = runnerReturning(Map(macStyle -> ProcessOutput(0, "Dark\n"), macContrast -> ProcessOutput(0, "0\n")))

    OsAppearanceDetector.forOs("Mac OS X", runner).detect.unsafeRunSync() shouldBe OsAppearance.Dark
  }

  it should "ask Windows for the personalisation and high-contrast registry values" in {
    val runner = runnerReturning(
      Map(
        winTheme    -> ProcessOutput(0, "    AppsUseLightTheme    REG_DWORD    0x1\r\n"),
        winContrast -> ProcessOutput(0, "    Flags    REG_SZ    127\r\n")
      )
    )

    OsAppearanceDetector.forOs("Windows 11", runner).detect.unsafeRunSync() shouldBe OsAppearance.HighContrast
  }

  it should "ask gsettings on Linux" in {
    val runner = runnerReturning(
      Map(gsettingsScheme -> ProcessOutput(0, "'prefer-dark'\n"), gsettingsGtk -> ProcessOutput(0, "'Adwaita'\n"))
    )

    OsAppearanceDetector.forOs("Linux", runner).detect.unsafeRunSync() shouldBe OsAppearance.Dark
  }

  it should "be Unknown when a command cannot be run" in {
    OsAppearanceDetector.forOs("Linux", _ => IO.pure(None)).detect.unsafeRunSync() shouldBe OsAppearance.Unknown
  }

  it should "be Unknown rather than fail when the runner raises" in {
    val runner: ProcessRunner = _ => IO.raiseError(new java.io.IOException("no such file"))

    OsAppearanceDetector.forOs("Mac OS X", runner).detect.unsafeRunSync() shouldBe OsAppearance.Unknown
  }

  it should "be Unknown on an operating system it has no query for" in {
    OsAppearanceDetector
      .forOs("Plan 9", runnerReturning(Map.empty))
      .detect
      .unsafeRunSync() shouldBe OsAppearance.Unknown
  }

  "the system process runner" should "report a program that does not exist as not run" in {
    ProcessRunner.system(List("serenity-no-such-program-xyz")).unsafeRunSync() shouldBe None
  }
