package com.serenity

import com.serenity.app.Java2DPipeline
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Java2DPipelineSpec extends AnyFlatSpec with Matchers:

  "Java2DPipeline.missingDefaults" should "disable unsafe Java2D pipelines by default on Windows" in {
    Java2DPipeline.missingDefaults("Windows 11", _ => None) shouldBe Map(
      "sun.java2d.d3d"    -> "false",
      "sun.java2d.opengl" -> "false"
    )
  }

  it should "preserve explicit JVM pipeline properties" in {
    val existing = Map("sun.java2d.d3d" -> "true")

    Java2DPipeline.missingDefaults("Windows 11", existing.get) shouldBe Map(
      "sun.java2d.opengl" -> "false"
    )
  }

  it should "leave non-Windows platforms untouched" in {
    Java2DPipeline.missingDefaults("Linux", _ => None) shouldBe Map.empty
  }
