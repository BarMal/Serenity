package com.serenity.ui.skia

import java.nio.file.{Files, Paths}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Skia measurer and the face cache take a `FontSpec` and name no AWT: what AWT still gives Skia (the advance that
  * picks a weight, a strikethrough rule's geometry) comes through `AwtFontReconciliation`, the one place to delete.
  */
final class SkiaMeasurementPathNamesNoAwtSpec extends AnyFlatSpec with Matchers:

  private val Directory = Paths.get("src", "main", "scala", "com", "serenity", "ui", "skia")

  private def source(name: String): String = Files.readString(Directory.resolve(name))

  "The Skia measurement path" should "name no java.awt" in
    List("SkiaTextMeasurer.scala", "SkiaFontCache.scala").foreach { name =>
      withClue(name) {
        source(name) should not include "java.awt"
      }
    }

  "AwtFontReconciliation" should "be the only Skia file that asks AWT for a font's advance or strikethrough" in {
    val files = Files.list(Directory)
    val naming =
      try
        import scala.jdk.CollectionConverters.*
        files.iterator.asScala.toList
          .filter(path => Files.readString(path).contains("getStringBounds"))
          .map(_.getFileName.toString)
      finally files.close()
    naming shouldBe List("AwtFontReconciliation.scala")
  }
