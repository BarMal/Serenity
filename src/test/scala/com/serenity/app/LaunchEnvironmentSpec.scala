package com.serenity.app

import com.serenity.config.RenderBackendSetting
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** An environment variable that Serenity reads and cannot make sense of stops the launch instead of being ignored. */
class LaunchEnvironmentSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  "LaunchEnvironment.problems" should "accept an environment with none of the variables set" in {
    LaunchEnvironment.problems(Map("PATH" -> "/usr/bin")) shouldBe Nil
  }

  it should "accept every valid value, in any case and with surrounding space" in {
    val valid = List(
      "SERENITY_RENDER_BACKEND" -> List("auto", "skia", "java2d", " Skia ", "JAVA2D"),
      "SERENITY_WAYLAND_RENDER" -> List("java2d", "skia-raster", "xwayland", " XWayland "),
      "SERENITY_TOOLKIT"        -> List("auto", "x11", "wayland", " X11 ", "Wayland")
    )
    for
      (name, values) <- valid
      value          <- values
    do LaunchEnvironment.problems(Map(name -> value)) shouldBe Nil
  }

  it should "treat a blank value as unset" in {
    for name <- List("SERENITY_RENDER_BACKEND", "SERENITY_WAYLAND_RENDER", "SERENITY_TOOLKIT") do
      LaunchEnvironment.problems(Map(name -> "  ")) shouldBe Nil
  }

  it should "name the variable, the value and the choices for an invalid SERENITY_RENDER_BACKEND" in {
    val problems = LaunchEnvironment.problems(Map("SERENITY_RENDER_BACKEND" -> "vulkan"))
    problems should have size 1
    problems.head should (include("SERENITY_RENDER_BACKEND") and include("vulkan") and include(
      "auto, skia or java2d"
    ))
  }

  it should "name the variable, the value and the choices for an invalid SERENITY_WAYLAND_RENDER" in {
    val problems = LaunchEnvironment.problems(Map("SERENITY_WAYLAND_RENDER" -> "wl"))
    problems should have size 1
    problems.head should (include("SERENITY_WAYLAND_RENDER") and include("wl") and include(
      "java2d, skia-raster or xwayland"
    ))
  }

  it should "name the variable, the value and the choices for an invalid SERENITY_TOOLKIT" in {
    val problems = LaunchEnvironment.problems(Map("SERENITY_TOOLKIT" -> "mir"))
    problems should have size 1
    problems.head should (include("SERENITY_TOOLKIT") and include("mir") and include("auto, x11 or wayland"))
  }

  it should "report every invalid variable, not just the first" in {
    val problems = LaunchEnvironment.problems(
      Map("SERENITY_RENDER_BACKEND" -> "x", "SERENITY_WAYLAND_RENDER" -> "y", "SERENITY_TOOLKIT" -> "z")
    )
    problems should have size 3
  }

  it should "reject exactly the values the variable's own parser would not understand" in {
    val values = Gen.oneOf(
      Gen.alphaNumStr,
      Gen.oneOf("auto", "skia", "java2d", "x11", "wayland", "xwayland", "skia-raster", "AUTO", "vulkan")
    )
    forAll(values) { value =>
      val accepted = value.trim.isEmpty || RenderBackendSetting.fromConfigKey(value).isDefined
      LaunchEnvironment.problems(Map("SERENITY_RENDER_BACKEND" -> value)).isEmpty shouldBe accepted
    }
  }

  "ToolkitSelection.Accepted" should "list the values SERENITY_TOOLKIT understands" in {
    ToolkitSelection.Accepted shouldBe List("auto", "x11", "wayland")
  }
