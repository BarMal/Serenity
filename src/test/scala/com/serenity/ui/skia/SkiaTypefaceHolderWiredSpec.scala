package com.serenity.ui.skia

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A process resolves a logical family such as `SansSerif` once, through the holder of its shared slots, so that the
  * grid, layout and drawing all see the same face. A font cache or holder made anywhere else in production code would
  * resolve on its own.
  */
class SkiaTypefaceHolderWiredSpec extends AnyFlatSpec with Matchers:

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

  "Only the Skia text caches and the display metrics over a holder they are given" should "make a font cache" in {
    mainSourcesContaining("new SkiaFontCache(") shouldBe List(
      "scala/com/serenity/app/RuntimeDisplayState.scala",
      "scala/com/serenity/ui/skia/SkiaTextCaches.scala"
    )
  }

  "A typeface holder" should "be made only by the Skia text caches and the defaults of specs' convenience" in {
    mainSourcesContaining("new SharedTypefaces(") shouldBe List(
      "scala/com/serenity/app/RuntimeDisplayState.scala",
      "scala/com/serenity/ui/skia/SkiaFontCache.scala",
      "scala/com/serenity/ui/skia/SkiaTextCaches.scala"
    )
  }
