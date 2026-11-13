package com.serenity

import com.serenity.app.SwingPipelineDefaults
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingPipelineDefaultsSpec extends AnyFlatSpec with Matchers:

  "SwingPipelineDefaults.missingDefaults" should "disable unsafe Java2D pipelines by default on Windows" in {
    SwingPipelineDefaults.missingDefaults("Windows 11", _ => None) shouldBe Map(
      "sun.java2d.d3d"    -> "false",
      "sun.java2d.opengl" -> "false"
    )
  }

  it should "preserve explicit JVM pipeline properties" in {
    val existing = Map("sun.java2d.d3d" -> "true")

    SwingPipelineDefaults.missingDefaults("Windows 11", existing.get) shouldBe Map(
      "sun.java2d.opengl" -> "false"
    )
  }

  it should "leave non-Windows platforms untouched" in {
    SwingPipelineDefaults.missingDefaults("Linux", _ => None) shouldBe Map.empty
  }
