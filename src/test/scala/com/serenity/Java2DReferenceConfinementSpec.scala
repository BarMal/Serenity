package com.serenity

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pixel specs take their reference renderer from [[ReferenceFrames]], [[PersistedFrameFixture]] and the cold-frame
  * harness, which paint with Skia. A test file that names `Java2DRenderSurface` or `Java2DFrames` has to be on a list
  * that says why: a new file cannot join one, and a file that stops naming them has to leave its list.
  */
class Java2DReferenceConfinementSpec extends AnyFlatSpec with Matchers:

  private val testRoot = Paths.get("src", "test", "scala", "com", "serenity")
  private val selfName = "Java2DReferenceConfinementSpec.scala"

  /** They test `Java2DRenderSurface`, its image types, layer buffers, font render context, shaped-text cache and
    * activity counters, `Graphics2D` state, its use as the fallback, how Java2D caches a presented image, or a fixture
    * only AWT can see (a font registered with `GraphicsEnvironment`); they go with the class. Skia has its own specs
    * for the same behaviours (`SkiaRenderSurfaceSpec`).
    */
  private val java2dOnly: Set[String] = Set(
    "CaretFollowsCursorAcrossFramesSpec.scala",
    "CommandRunnerEditorPaneAccumulationSpec.scala",
    "CursorPixelAlignmentSpec.scala",
    "FrameBackend.scala",
    "Java2DFrameImageTypeSpec.scala",
    "Java2DLayerSurfaceSpec.scala",
    "Java2DRenderSurfaceSpec.scala",
    "RenderSurfaceDrawRunPxSpec.scala",
    "SingleUseFrameSurface.scala",
    "perf/CellGridBenchmarks.scala",
    "perf/FontFallbackColdFrameSpec.scala",
    "perf/FontFallbackSelectionSpec.scala",
    "perf/GuiCellGridClusterSelectionSpec.scala",
    "perf/GuiCellGridColdFrameSpec.scala",
    "perf/GuiCellGridSelectionSpec.scala",
    "perf/PageDownRichDocumentFramesSpec.scala",
    "perf/PresentedFrames.scala",
    "perf/PresentedFramesSpec.scala",
    "state/manager/RenderCachesRendererOwnershipSpec.scala",
    "ui/renderer/FontFallbackPaintSpec.scala",
    "ui/renderer/FontSpecSpec.scala",
    "ui/renderer/GuiCellGridFallbackPaintSpec.scala",
    "ui/renderer/Java2DClearViewportExceptSpec.scala",
    "ui/renderer/LabelSpritesSpec.scala",
    "ui/renderer/LigatureFontShapingSpec.scala",
    "ui/renderer/PixelLiftingContractSpec.scala",
    "ui/renderer/RenderImageSpec.scala",
    "ui/renderer/ShapedTextPaintSpec.scala",
    "ui/renderer/StyledFontCacheSpec.scala",
    "ui/renderer/SurfaceActivitySpec.scala",
    "ui/skia/SkiaLayerSurfaceFactorySpec.scala"
  )

  /** Still painting through `new Java2DRenderSurface`; the list only shrinks. */
  private val notYetMoved: Set[String] = Set(
    "perf/ChapterGhostRowReuseSpec.scala",
    "perf/ColumnPaneRowReuseSpec.scala",
    "perf/FrameCountersRenderSpec.scala",
    "perf/LaptopFrameBenchmarks.scala",
    "perf/LaptopFramesMatchColdSpec.scala",
    "perf/OracleImages.scala",
    "perf/PanelLayerGeometrySpec.scala",
    "perf/RenderBenchmarks.scala",
    "perf/TypingFramePresentSpec.scala"
  )

  /** Run the same assertions on both renderers, so they name `Java2DFrames` on purpose. */
  private val bothRenderers: Set[String] = Set(
    "FrameBackend.scala",
    "perf/SkiaFrameBenchmarks.scala",
    "ui/renderer/Java2DRunPaintSpec.scala",
    "ui/renderer/PixelLiftingContractSpec.scala",
    "ui/renderer/SurfaceTextContractSpec.scala"
  )

  /** Suites that stay on Java2D because Skia still leaves their incremental frames different from the cold ones
    * (#2380); each flips by deleting its `referenceFrames` override. The line number gutter and heading spacing suites
    * are pinned on Windows only, and the focused rich-text and virtual drop cap suites on macOS only, where Skia leaves
    * a few pixels of one row different; the raised letter and column paging suites are pinned everywhere.
    */
  private val pinnedBySkiaGap: Set[String] = Set(
    "perf/ColumnPagingFramesSpec.scala",
    "perf/FocusedRichTextFrameSpec.scala",
    "perf/HeadingSpacingFramesSpec.scala",
    "perf/LineNumberGutterSpillSpec.scala",
    "perf/RaisedLetterIncrementalFrameSpec.scala",
    "perf/VirtualDropCapIncrementalFrameSpec.scala"
  )

  /** Window specs that still use Java2D as the reference for what Skia paints; the list only shrinks. */
  private val windowSpecsNotYetMoved: Set[String] = Set(
    "ScrollReusePixelsSpec.scala",
    "ui/skia/SkiaLayerWindowSpec.scala",
    "ui/skia/SkiaRasterWindowSpec.scala"
  )

  private def testSources: List[Path] =
    Using.resource(Files.walk(testRoot)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .toList
    }

  private def relative(path: Path): String = testRoot.relativize(path).toString.replace('\\', '/')

  private def naming(word: String): Set[String] =
    testSources
      .filterNot(path => relative(path) == selfName)
      .filter(path => Files.readAllLines(path).asScala.exists(_.contains(word)))
      .map(relative)
      .toSet

  private val namingSurface: Set[String] = naming("Java2DRenderSurface")
  private val namingFrames: Set[String]  = naming("Java2DFrames")

  private val surfaceLists = java2dOnly ++ notYetMoved
  private val framesLists  = bothRenderers ++ pinnedBySkiaGap ++ windowSpecsNotYetMoved

  "Test files naming Java2DRenderSurface" should "be on a list" in {
    (namingSurface -- surfaceLists) shouldBe Set.empty
  }

  it should "leave the lists once they stop naming it" in {
    (surfaceLists -- namingSurface) shouldBe Set.empty
  }

  it should "not sit on both lists" in {
    (java2dOnly intersect notYetMoved) shouldBe Set.empty
  }

  "Test files naming Java2DFrames" should "be on a list" in {
    (namingFrames -- framesLists) shouldBe Set.empty
  }

  it should "leave the lists once they stop naming it" in {
    (framesLists -- namingFrames) shouldBe Set.empty
  }
