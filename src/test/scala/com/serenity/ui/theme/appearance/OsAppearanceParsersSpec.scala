package com.serenity.ui.theme.appearance

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Each OS parser is fed the strings the real command prints, so a change in what we accept is a change in a fixture.
  */
class OsAppearanceParsersSpec extends AnyFlatSpec with Matchers:

  private def ok(stdout: String): Option[ProcessOutput] = Some(ProcessOutput(0, stdout))
  private def failed: Option[ProcessOutput]             = Some(ProcessOutput(1, ""))
  private val notRun: Option[ProcessOutput]             = None

  private val windowsLightRegistry =
    "\r\nHKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize\r\n" +
      "    AppsUseLightTheme    REG_DWORD    0x1\r\n\r\n"

  private val windowsDarkRegistry = windowsLightRegistry.replace("0x1", "0x0")

  private def windowsFlags(value: String) =
    "\r\nHKEY_CURRENT_USER\\Control Panel\\Accessibility\\HighContrast\r\n" +
      s"    Flags    REG_SZ    $value\r\n\r\n"

  "the macOS parser" should "read Dark from the interface style" in {
    OsAppearanceParsers.macOs(ok("Dark\n"), ok("0\n")) shouldBe OsAppearance.Dark
  }

  it should "read Light when the interface style key is absent, as it is in light mode" in {
    OsAppearanceParsers.macOs(failed, ok("0\n")) shouldBe OsAppearance.Light
  }

  it should "let increase-contrast win over the interface style" in {
    OsAppearanceParsers.macOs(ok("Dark\n"), ok("1\n")) shouldBe OsAppearance.HighContrast
    OsAppearanceParsers.macOs(failed, ok("1\n")) shouldBe OsAppearance.HighContrast
  }

  it should "ignore an unreadable increase-contrast setting" in {
    OsAppearanceParsers.macOs(ok("Dark\n"), failed) shouldBe OsAppearance.Dark
    OsAppearanceParsers.macOs(ok("Dark\n"), notRun) shouldBe OsAppearance.Dark
  }

  it should "be Unknown when defaults could not run at all" in {
    OsAppearanceParsers.macOs(notRun, notRun) shouldBe OsAppearance.Unknown
  }

  "the Windows parser" should "read the AppsUseLightTheme DWORD" in {
    OsAppearanceParsers.windows(ok(windowsLightRegistry), ok(windowsFlags("126"))) shouldBe OsAppearance.Light
    OsAppearanceParsers.windows(ok(windowsDarkRegistry), ok(windowsFlags("126"))) shouldBe OsAppearance.Dark
  }

  it should "report high contrast when the HCF_HIGHCONTRASTON bit is set" in {
    OsAppearanceParsers.windows(ok(windowsLightRegistry), ok(windowsFlags("127"))) shouldBe OsAppearance.HighContrast
    OsAppearanceParsers.windows(ok(windowsDarkRegistry), ok(windowsFlags("1"))) shouldBe OsAppearance.HighContrast
  }

  it should "treat a missing AppsUseLightTheme value, as on older Windows, as light" in {
    OsAppearanceParsers.windows(failed, ok(windowsFlags("126"))) shouldBe OsAppearance.Light
  }

  it should "not mistake an unreadable flags value for high contrast" in {
    OsAppearanceParsers.windows(ok(windowsDarkRegistry), failed) shouldBe OsAppearance.Dark
    OsAppearanceParsers.windows(ok(windowsDarkRegistry), ok("garbage")) shouldBe OsAppearance.Dark
  }

  it should "be Unknown when reg could not run, or printed something it cannot read" in {
    OsAppearanceParsers.windows(notRun, notRun) shouldBe OsAppearance.Unknown
    OsAppearanceParsers.windows(ok("garbage"), ok(windowsFlags("126"))) shouldBe OsAppearance.Unknown
  }

  "the Linux parser" should "read the colour-scheme key" in {
    OsAppearanceParsers.linux(ok("'prefer-dark'\n"), ok("'Adwaita'\n")) shouldBe OsAppearance.Dark
    OsAppearanceParsers.linux(ok("'prefer-light'\n"), ok("'Adwaita-dark'\n")) shouldBe OsAppearance.Light
  }

  it should "fall back to the GTK theme name when the colour scheme is the default" in {
    OsAppearanceParsers.linux(ok("'default'\n"), ok("'Adwaita-dark'\n")) shouldBe OsAppearance.Dark
    OsAppearanceParsers.linux(ok("'default'\n"), ok("'Adwaita'\n")) shouldBe OsAppearance.Light
  }

  it should "fall back to the GTK theme name when the colour-scheme key does not exist" in {
    OsAppearanceParsers.linux(failed, ok("'Yaru-dark'\n")) shouldBe OsAppearance.Dark
    OsAppearanceParsers.linux(notRun, ok("'Yaru'\n")) shouldBe OsAppearance.Light
  }

  it should "report high contrast for the HighContrast GTK themes whatever the colour scheme says" in {
    OsAppearanceParsers.linux(ok("'default'\n"), ok("'HighContrast'\n")) shouldBe OsAppearance.HighContrast
    OsAppearanceParsers.linux(ok("'prefer-dark'\n"), ok("'HighContrastInverse'\n")) shouldBe OsAppearance.HighContrast
  }

  it should "be Unknown when gsettings is unavailable" in {
    OsAppearanceParsers.linux(notRun, notRun) shouldBe OsAppearance.Unknown
    OsAppearanceParsers.linux(failed, failed) shouldBe OsAppearance.Unknown
  }
