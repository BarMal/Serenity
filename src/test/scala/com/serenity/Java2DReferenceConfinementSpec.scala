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

  /** They test `Java2DRenderSurface`, its image types and layer buffers, `Graphics2D` state, or a fixture only AWT can
    * see (a font registered with `GraphicsEnvironment`); they go with the class.
    */
  private val java2dOnly: Set[String] = Set(
    "CaretFollowsCursorAcrossFramesSpec.scala",
    "CommandRunnerEditorPaneAccumulationSpec.scala",
    "FrameBackend.scala",
    "Java2DFrameImageTypeSpec.scala",
    "Java2DLayerSurfaceSpec.scala",
    "Java2DRenderSurfaceSpec.scala",
    "perf/PageDownRichDocumentFramesSpec.scala",
    "ui/renderer/FontFallbackPaintSpec.scala",
    "ui/renderer/FontSpecSpec.scala",
    "ui/renderer/GuiCellGridFallbackPaintSpec.scala",
    "ui/renderer/Java2DClearViewportExceptSpec.scala",
    "ui/renderer/LabelSpritesSpec.scala",
    "ui/renderer/PixelLiftingContractSpec.scala",
    "ui/renderer/RenderImageSpec.scala",
    "ui/renderer/StyledFontCacheSpec.scala"
  )

  /** Still painting through `new Java2DRenderSurface`; the list only shrinks. */
  private val notYetMoved: Set[String] = Set(
    "CursorPixelAlignmentSpec.scala",
    "ModalLayerCompositingSpec.scala",
    "RenderSurfaceDrawRunPxSpec.scala",
    "SingleUseFrameSurface.scala",
    "perf/CellGridBenchmarks.scala",
    "perf/ChapterGhostRowReuseSpec.scala",
    "perf/ColumnPaneRowReuseSpec.scala",
    "perf/FontFallbackColdFrameSpec.scala",
    "perf/FontFallbackSelectionSpec.scala",
    "perf/FrameCountersRenderSpec.scala",
    "perf/GuiCellGridClusterSelectionSpec.scala",
    "perf/GuiCellGridColdFrameSpec.scala",
    "perf/GuiCellGridSelectionSpec.scala",
    "perf/LaptopFrameBenchmarks.scala",
    "perf/LaptopFramesMatchColdSpec.scala",
    "perf/OracleImages.scala",
    "perf/PanelLayerGeometrySpec.scala",
    "perf/PresentedFrames.scala",
    "perf/PresentedFramesSpec.scala",
    "perf/RenderBenchmarks.scala",
    "perf/TypingFramePresentSpec.scala",
    "state/manager/RenderCachesRendererOwnershipSpec.scala",
    "ui/color/RenderColorSpec.scala",
    "ui/layout/RaisedLetterGeometrySpec.scala",
    "ui/renderer/ImePreEditScreenshotSpec.scala",
    "ui/renderer/LigatureFontShapingSpec.scala",
    "ui/renderer/ShapedTextPaintSpec.scala",
    "ui/renderer/SurfaceActivitySpec.scala",
    "ui/skia/SkiaLayerSurfaceFactorySpec.scala"
  )

  /** Run the same assertions on both renderers, so they name `Java2DFrames` on purpose. */
  private val bothRenderers: Set[String] = Set(
    "FrameBackend.scala",
    "perf/SkiaFrameBenchmarks.scala",
    "ui/renderer/Java2DRunPaintSpec.scala",
    "ui/renderer/PixelLiftingContractSpec.scala",
    "ui/renderer/SurfaceTextContractSpec.scala"
  )

  /** On Skia an incrementally drawn rich-text frame is the cold frame shifted one device pixel to the left, so these
    * suites stay on Java2D until that is fixed (#2380); each flips by deleting its `referenceFrames` override. The line
    * number gutter suite is pinned on Windows only, where Skia leaves a few pixels of one row different.
    */
  private val pinnedBySkiaGap: Set[String] = Set(
    "perf/LineNumberGutterSpillSpec.scala",
    "perf/ColumnPagingFramesSpec.scala",
    "perf/DropCapIncrementalFrameSpec.scala",
    "perf/HeadingSpacingFramesSpec.scala",
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
