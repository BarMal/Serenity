package com.serenity.ui.layout

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Skia measurer is handed to the state manager at the one place the owner approved (step D0, #2660 item 2), and
  * since step D2 layout reads it, through the `LayoutMemos` its wrap cache carries. Since D3 the renderer hands it to
  * the text it aligns and fits, from the three places that build a frame's `RenderContext`. A new file that names it,
  * or a file other than those that reads a measurer outside layout, fails here, so that spreading it is a decision and
  * not a side effect.
  */
class SkiaTextMeasurerUnwiredSpec extends AnyFlatSpec with Matchers:

  private val root = Paths.get("src", "main")

  private def mainSourcesContaining(text: String): List[String] =
    Using.resource(Files.walk(root)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .filter(path => Files.readString(path).contains(text))
        .map(path => root.relativize(path).toString)
        .toList
        .sorted
    }

  "Only SkiaTextMeasurer itself and the GUI launch" should "name SkiaTextMeasurer" in {
    mainSourcesContaining("SkiaTextMeasurer") shouldBe List(
      "scala/Main.scala",
      "scala/com/serenity/ui/skia/SkiaTextMeasurer.scala"
    )
  }

  "Only layout and the three frame entry points" should "read a measurer" in {
    mainSourcesContaining(".measurer").filterNot(_.startsWith("scala/com/serenity/ui/layout/")) shouldBe List(
      "scala/com/serenity/ui/renderer/RenderContext.scala",
      "scala/com/serenity/ui/renderer/RendererEntryPoints.scala",
      "scala/com/serenity/ui/renderer/RendererFramePlanner.scala"
    )
  }
