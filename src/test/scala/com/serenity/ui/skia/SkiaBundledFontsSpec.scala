package com.serenity.ui.skia

import java.awt.Font
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The fonts the application bundles reach Skia as they reach AWT: a font added to the resources is one a fallback
  * chain can name, and Skia must find it by family rather than draw a system default in its place.
  */
class SkiaBundledFontsSpec extends AnyFlatSpec with Matchers:

  private val fontFolder: Path = Paths.get("src", "main", "resources", "fonts")

  private def bundledFiles: List[Path] =
    Using.resource(Files.list(fontFolder)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => path.toString.endsWith(".ttf") || path.toString.endsWith(".otf"))
        .toList
        .sortBy(_.toString)
    }

  private def familyOf(file: Path): String =
    Font.createFont(Font.TRUETYPE_FONT, file.toFile).getFamily

  "Every bundled font" should "be found by Skia under its own family" in {
    val unresolved = bundledFiles.filterNot(file => SkiaTypefaces.hasBundled(familyOf(file)))

    unresolved.map(_.getFileName.toString) shouldBe Nil
  }
