package com.serenity.ui.layout

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Skia measurer is handed to the state manager at the one place the owner approved (step D0, #2660 item 2), and
  * nothing reads it yet: layout still measures through AWT until the later steps of D move each part onto it. A new
  * file that names it, or reads `RenderCaches.measurer`, fails here, so that spreading it is a decision and not a side
  * effect.
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

  "Nothing but RenderCaches" should "read the measurer a RenderCaches holds" in {
    mainSourcesContaining(".measurer") shouldBe Nil
  }
