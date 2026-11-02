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
    Environment("Linux", properties, env, waylandToolkitAvailable = true, waylandSocketProblem = None)

  private def stock(env: Map[String, String]): Environment =
    Environment("Linux", temurin, env, waylandToolkitAvailable = false, waylandSocketProblem = None)

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
    val mac = Environment("Mac OS X", jbrProperties, wayland + ("SERENITY_TOOLKIT" -> "wayland"), true, None)
    ToolkitSelection.choose(mac).choice shouldBe Choice.PlatformDefault
    ToolkitSelection.choose(mac.copy(osName = "Windows 11")).choice shouldBe Choice.PlatformDefault
  }

  it should "never override a toolkit chosen on the command line" in {
    val explicit = jbr(wayland, jbrProperties + ("awt.toolkit.name" -> "XToolkit"))
    ToolkitSelection.choose(explicit).choice shouldBe Choice.PlatformDefault
    ToolkitSelection.choose(explicit).reason should include("awt.toolkit.name")
  }

  "ToolkitSelection.userToolkit" should "see nothing when the JVM was started without the flag" in {
    ToolkitSelection.userToolkit(Nil) shouldBe None
    ToolkitSelection.userToolkit(List("-Xmx2g", "-Dfoo=bar")) shouldBe None
  }

  it should "read the toolkit named by -Dawt.toolkit.name" in {
    ToolkitSelection.userToolkit(List("-Dawt.toolkit.name=WLToolkit")) shouldBe Some("WLToolkit")
  }

  it should "take the last of several flags, as the JVM does" in {
    val flags = List("-Dawt.toolkit.name=XToolkit", "-Dawt.toolkit.name=WLToolkit")
    ToolkitSelection.userToolkit(flags) shouldBe Some("WLToolkit")
  }

  it should "ignore an empty value and a property that only starts with the same name" in {
    ToolkitSelection.userToolkit(List("-Dawt.toolkit.name=")) shouldBe None
    ToolkitSelection.userToolkit(List("-Dawt.toolkit.name.other=WLToolkit")) shouldBe None
  }

  "ToolkitSelection.userProperties" should "drop a toolkit the runtime set itself" in {
    val preset = jbrProperties + ("awt.toolkit.name" -> "XToolkit")
    ToolkitSelection.userProperties(preset, Nil) shouldBe jbrProperties
  }

  it should "keep the toolkit the user passed, with the user's value" in {
    val preset = jbrProperties + ("awt.toolkit.name" -> "XToolkit")
    ToolkitSelection.userProperties(preset, List("-Dawt.toolkit.name=WLToolkit")) shouldBe
      jbrProperties + ("awt.toolkit.name" -> "WLToolkit")
  }

  "A JetBrains Runtime that presets awt.toolkit.name=XToolkit" should "obey SERENITY_TOOLKIT=wayland" in {
    val preset      = jbrProperties + ("awt.toolkit.name" -> "XToolkit")
    val environment = jbr(wayland + ("SERENITY_TOOLKIT" -> "wayland"), ToolkitSelection.userProperties(preset, Nil))
    ToolkitSelection.choose(environment).choice shouldBe Choice.Wayland
  }

  it should "pick the native Wayland toolkit in a Wayland session with nothing set" in {
    val preset = jbrProperties + ("awt.toolkit.name" -> "XToolkit")
    ToolkitSelection.choose(jbr(wayland, ToolkitSelection.userProperties(preset, Nil))).choice shouldBe Choice.Wayland
  }

  it should "still lose to an explicit -Dawt.toolkit.name=XToolkit" in {
    val preset = jbrProperties + ("awt.toolkit.name" -> "XToolkit")
    val flags  = List("-Dawt.toolkit.name=XToolkit")
    val chosen = ToolkitSelection.choose(
      jbr(wayland + ("SERENITY_TOOLKIT" -> "wayland"), ToolkitSelection.userProperties(preset, flags))
    )
    chosen.choice shouldBe Choice.PlatformDefault
    chosen.reason should include("awt.toolkit.name")
  }

  private val staleSocket = Some("/run/user/1000/wayland-1: Connection refused")

  it should "fall back to the platform default when the Wayland socket does not connect" in {
    val stale = ToolkitSelection.choose(jbr(wayland).copy(waylandSocketProblem = staleSocket))
    stale.choice shouldBe Choice.PlatformDefault
    stale.reason should include("WAYLAND_DISPLAY")
    stale.reason should include("/run/user/1000/wayland-1: Connection refused")
  }

  it should "still pick the native Wayland toolkit when the socket connects" in {
    ToolkitSelection.choose(jbr(wayland).copy(waylandSocketProblem = None)).choice shouldBe Choice.Wayland
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

  it should "not force Wayland when the socket does not connect, and say why" in {
    val forced   = jbr(wayland + ("SERENITY_TOOLKIT" -> "wayland")).copy(waylandSocketProblem = staleSocket)
    val decision = ToolkitSelection.choose(forced)
    decision.choice shouldBe Choice.PlatformDefault
    decision.reason should include("SERENITY_TOOLKIT=wayland")
    decision.reason should include("Connection refused")
  }

  it should "leave the X11 request alone when the socket does not connect" in {
    val x11 = jbr(wayland + ("SERENITY_TOOLKIT" -> "x11")).copy(waylandSocketProblem = staleSocket)
    ToolkitSelection.choose(x11).choice shouldBe Choice.X11
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

  "ToolkitSelection.choose with xwaylandForSkia" should "pick X11 on a JetBrains Runtime in a Wayland session" in {
    val decision = ToolkitSelection.choose(jbr(wayland).copy(xwaylandForSkia = true))
    decision.choice shouldBe Choice.X11
    decision.reason should include("xwayland")
  }

  it should "change nothing outside a Wayland session" in {
    ToolkitSelection.choose(jbr(x11Only).copy(xwaylandForSkia = true)).choice shouldBe Choice.PlatformDefault
  }

  it should "lose to an explicit SERENITY_TOOLKIT=wayland" in {
    val explicit = jbr(wayland + ("SERENITY_TOOLKIT" -> "wayland")).copy(xwaylandForSkia = true)
    ToolkitSelection.choose(explicit).choice shouldBe Choice.Wayland
  }

  it should "lose to a toolkit named on the command line" in {
    val explicit = jbr(wayland, jbrProperties + ("awt.toolkit.name" -> "WLToolkit")).copy(xwaylandForSkia = true)
    ToolkitSelection.choose(explicit).choice shouldBe Choice.PlatformDefault
  }
