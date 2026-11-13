package com.serenity.ui.renderer

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Nothing paints through Java2D any more: the window's frames are Skia's, on the GPU layer or the CPU raster. A name
  * from the rendering family that comes back in the main sources (code or a comment describing code that is gone) fails
  * here, so a stray reference cannot return. Swing's own use of Java2D for its window, and AWT fonts, are not that
  * family.
  */
class Java2DRenderingRemovedSpec extends AnyFlatSpec with Matchers:

  private val mainRoot = Paths.get("src", "main")

  private val Retired = List(
    "Java2DRenderSurface",
    "Java2DRoundedOutlines",
    "Java2DKeptOff",
    "FrameSurfaceFactory.Java2D",
    "RasterThenJava2D",
    "LayerSurfaces"
  )

  /** Where the word legitimately stays: the migration that retires the `java2d` config values, and flying-saucer's own
    * `Java2DRenderer`, which the markdown preview still renders through (epic #2660 item 3).
    */
  private val MayNameJava2D =
    Set("ConfigMigration.scala", "RenderBackendMigration.scala", "MarkdownDocumentPreview.scala")

  private def mainSources: List[Path] =
    Using.resource(Files.walk(mainRoot)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .toList
    }

  "The main sources" should "name nothing from the retired Java2D rendering family" in {
    val offenders = for
      path           <- mainSources
      (line, number) <- Files.readAllLines(path).asScala.zipWithIndex
      name           <- Retired.filter(line.contains)
    yield s"${mainRoot.relativize(path)}:${number + 1} $name"

    offenders shouldBe Nil
  }

  it should "not call anything Java2D outside the migration and the markdown preview, in code or in comments" in {
    val offenders = for
      path           <- mainSources.filterNot(path => MayNameJava2D.contains(path.getFileName.toString))
      (line, number) <- Files.readAllLines(path).asScala.zipWithIndex
      if line.contains("Java2D")
    yield s"${mainRoot.relativize(path)}:${number + 1} ${line.trim}"

    offenders shouldBe Nil
  }

  it should "hold no file of the retired classes" in {
    val files = mainSources.map(_.getFileName.toString).filter(name => Retired.exists(name.startsWith))

    files shouldBe Nil
  }

end Java2DRenderingRemovedSpec
