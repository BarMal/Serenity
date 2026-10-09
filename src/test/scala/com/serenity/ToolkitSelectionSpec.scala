package com.serenity

import com.serenity.app.ToolkitSelection
import com.serenity.app.ToolkitSelection.{Choice, Environment}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which AWT toolkit a launch asks for, decided before anything loads AWT. */
class ToolkitSelectionSpec extends AnyFlatSpec with Matchers:

  private val jbrProperties = Map("java.vendor" -> "JetBrains s.r.o.", "java.vm.vendor" -> "JetBrains s.r.o.")
  private val temurin       = Map("java.vendor" -> "Eclipse Adoptium", "java.vm.vendor" -> "Eclipse Adoptium")
  private val wayland       = Map("WAYLAND_DISPLAY" -> "wayland-1", "DISPLAY" -> ":0")
  private val x11Only       = Map("DISPLAY" -> ":0")

  private def jbr(env: Map[String, String], properties: Map[String, String] = jbrProperties): Environment =
    Environment("Linux", properties, env, waylandToolkitAvailable = true)

  private def stock(env: Map[String, String]): Environment =
    Environment("Linux", temurin, env, waylandToolkitAvailable = false)

  "ToolkitSelection.choose" should "pick the native Wayland toolkit on a JetBrains Runtime in a Wayland session" in {
    ToolkitSelection.choose(jbr(wayland)).choice shouldBe Choice.Wayland
  }

  it should "stay on the platform default outside a Wayland session" in {
    ToolkitSelection.choose(jbr(x11Only)).choice shouldBe Choice.PlatformDefault
    ToolkitSelection.choose(jbr(Map("WAYLAND_DISPLAY" -> " "))).choice shouldBe Choice.PlatformDefault
  }

  it should "stay on the platform default when the runtime has no Wayland toolkit" in {
    ToolkitSelection.choose(stock(wayland)).choice shouldBe Choice.PlatformDefault
  }

  it should "trust the toolkit class over the vendor name" in {
    ToolkitSelection.choose(jbr(wayland, properties = temurin)).choice shouldBe Choice.Wayland
  }

  it should "leave every other platform alone, even when asked for Wayland" in {
    val mac = Environment("Mac OS X", jbrProperties, wayland + ("SERENITY_TOOLKIT" -> "wayland"), true)
    ToolkitSelection.choose(mac).choice shouldBe Choice.PlatformDefault
    ToolkitSelection.choose(mac.copy(osName = "Windows 11")).choice shouldBe Choice.PlatformDefault
  }

  it should "never override a toolkit chosen on the command line" in {
    val explicit = jbr(wayland, jbrProperties + ("awt.toolkit.name" -> "XToolkit"))
    ToolkitSelection.choose(explicit).choice shouldBe Choice.PlatformDefault
    ToolkitSelection.choose(explicit).reason should include("awt.toolkit.name")
  }

  "SERENITY_TOOLKIT" should "force X11 through XWayland" in {
    ToolkitSelection.choose(jbr(wayland + ("SERENITY_TOOLKIT" -> "x11"))).choice shouldBe Choice.X11
    ToolkitSelection.choose(jbr(wayland + ("SERENITY_TOOLKIT" -> " X11 "))).choice shouldBe Choice.X11
  }

  it should "force Wayland only where the runtime can provide it" in {
    ToolkitSelection.choose(jbr(x11Only + ("SERENITY_TOOLKIT" -> "wayland"))).choice shouldBe Choice.Wayland
    val unavailable = ToolkitSelection.choose(stock(wayland + ("SERENITY_TOOLKIT" -> "wayland")))
    unavailable.choice shouldBe Choice.PlatformDefault
    unavailable.reason should include("JetBrains Runtime")
  }

  it should "fall back to detection for auto, blank and unrecognised values" in {
    ToolkitSelection.choose(jbr(wayland + ("SERENITY_TOOLKIT" -> "auto"))).choice shouldBe Choice.Wayland
    ToolkitSelection.choose(jbr(wayland + ("SERENITY_TOOLKIT" -> ""))).choice shouldBe Choice.Wayland
    val unknown = ToolkitSelection.choose(jbr(wayland + ("SERENITY_TOOLKIT" -> "mir")))
    unknown.choice shouldBe Choice.Wayland
    unknown.reason should include("mir")
  }

  "Choice.toolkitProperty" should "name the JetBrains Runtime toolkit classes, or nothing for the default" in {
    Choice.Wayland.toolkitProperty shouldBe Some("WLToolkit")
    Choice.X11.toolkitProperty shouldBe Some("XToolkit")
    Choice.PlatformDefault.toolkitProperty shouldBe None
  }
