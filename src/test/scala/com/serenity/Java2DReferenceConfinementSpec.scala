package com.serenity

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pixel specs take their reference renderer from [[ReferenceFrames]] and [[PersistedFrameFixture]], which paint
  * with Skia. A test file that names `Java2DRenderSurface` has to be on one of two lists: the specs that exist to pin
  * Java2D itself, and the ones not yet moved. A new file cannot join either, and a file that stops naming it has to
  * leave its list.
  */
class Java2DReferenceConfinementSpec extends AnyFlatSpec with Matchers:

  private val testRoot = Paths.get("src", "test", "scala", "com", "serenity")
  private val selfName = "Java2DReferenceConfinementSpec.scala"

  /** They test `Java2DRenderSurface`, its image types and layer buffers, `Graphics2D` state, or a fixture only AWT can
    * see (a font registered with `GraphicsEnvironment`); they go with the class.
    */
  private val java2dOnly: Set[String] = Set(
    "FrameBackend.scala",
    "Java2DFrameImageTypeSpec.scala",
    "Java2DLayerSurfaceSpec.scala",
    "Java2DRenderSurfaceSpec.scala",
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
    "CaretFollowsCursorAcrossFramesSpec.scala",
    "CommandRunnerEditorPaneAccumulationSpec.scala",
    "CursorPixelAlignmentSpec.scala",
    "GuiPopupAnchorSpec.scala",
    "MockRenderSurface.scala",
    "ModalLayerCompositingSpec.scala",
    "RenderSurfaceDrawRunPxSpec.scala",
    "RendererRepaintRectsSpec.scala",
    "SingleUseFrameSurface.scala",
    "SpellCheckToggleUnderCommandRunnerSpec.scala",
    "SummonedChromeFrameSpec.scala",
    "UiScenarioDriver.scala",
    "perf/CellGridBenchmarks.scala",
    "perf/ChapterGhostRowReuseSpec.scala",
    "perf/ColdFrameHarness.scala",
    "perf/ColumnPaneRowReuseSpec.scala",
    "perf/FontFallbackColdFrameSpec.scala",
    "perf/FontFallbackSelectionSpec.scala",
    "perf/FrameCountersRenderSpec.scala",
    "perf/GuiCellGridClusterSelectionSpec.scala",
    "perf/GuiCellGridColdFrameSpec.scala",
    "perf/GuiCellGridSelectionSpec.scala",
    "perf/HeadingSpacingFramesSpec.scala",
    "perf/LaptopFrameBenchmarks.scala",
    "perf/LaptopFramesMatchColdSpec.scala",
    "perf/PageDownRichDocumentFramesSpec.scala",
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
    "ui/renderer/RenderSurfaceCapabilitiesSpec.scala",
    "ui/renderer/ShapedTextPaintSpec.scala",
    "ui/renderer/SurfaceActivitySpec.scala",
    "ui/theme/TextStyleFontSpec.scala"
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

  private val namingJava2D: Set[String] =
    testSources
      .filterNot(path => relative(path) == selfName)
      .filter(path => Files.readAllLines(path).asScala.exists(_.contains("Java2DRenderSurface")))
      .map(relative)
      .toSet

  "Test files naming Java2DRenderSurface" should "be on a list" in {
    (namingJava2D -- java2dOnly -- notYetMoved) shouldBe Set.empty
  }

  it should "leave the lists once they stop naming it" in {
    ((java2dOnly ++ notYetMoved) -- namingJava2D) shouldBe Set.empty
  }

  it should "not sit on both lists" in {
    (java2dOnly intersect notYetMoved) shouldBe Set.empty
  }
