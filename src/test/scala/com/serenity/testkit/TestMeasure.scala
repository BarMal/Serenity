package com.serenity.testkit

import java.awt.Font
import java.util.concurrent.Executor

import com.serenity.state.manager.{RenderCaches, ScrollCountExecutor, ScrollCounting}
import com.serenity.ui.layout.{AnchorWrapCache, CellMetrics, LayoutMemos, TextMeasurer, WrappedLineCache}
import com.serenity.ui.renderer.{BackendCaches, CacheSlots, FontSpec, StyledFontCache}
import com.serenity.ui.skia.{SkiaTextMeasurer, TestBundledTypefaces}

/** What a spec measures a font with: the Skia measurer layout measures with in the app (#2660 item 2, step D2), over
  * faces that resolve the bundled fonts, the test-only Inter subset included, the same way on every host. Layout specs
  * ask here, and not the measuring code directly, for the numbers they lay text out in.
  */
object TestMeasure:

  /** One measurer for every spec that does not draw: it holds no state a layout could leave behind for another. */
  /** The faces the measurer lays out with, which every Skia oracle of a spec takes its fonts from too: which physical
    * font a logical family such as `SansSerif` resolves to can differ from one font cache to another on a machine, and
    * an oracle in a different font than the layout measures proves nothing.
    */
  lazy val faces: com.serenity.ui.skia.SkiaFontCache = TestBundledTypefaces.cacheWithInter()

  lazy val measurer: TextMeasurer = new SkiaTextMeasurer(faces)

  def cellMetrics(font: Font): CellMetrics = measurer.cellMetrics(FontSpec.fromAwt(font))

  /** The AWT context the specs that keep an AWT measurement as an oracle measure with: unscaled, anti-aliased,
    * fractional metrics.
    */
  val awtFontRenderContext: java.awt.font.FontRenderContext =
    java.awt.font.FontRenderContext(java.awt.geom.AffineTransform(), true, true)

  /** The ink Skia itself gives `text` in `font`, independent of the measurer under test: the oracle for where a glyph
    * stands, in place of AWT's glyph vector.
    */
  def skiaInk(font: Font, text: String): org.jetbrains.skia.Rect =
    faces.faceFor(FontSpec.fromAwt(font)).font.measureText(text, null)

  /** The ascent (negative above the baseline), descent and leading Skia itself gives `font`. */
  def skiaMetrics(font: Font): (Float, Float, Float) = com.serenity.ui.skia.SkiaTestMetrics.of(font)

  /** How far the font really advances one character, which a run laid out by the measurer is spaced by, not the whole
    * pixels of its grid cell.
    */
  def glyphAdvancePx(font: Font): Float = measurer.glyphWidthPx(FontSpec.fromAwt(font), "M")

  /** Memos that measure the way a GUI owner's do. */
  def layoutMemos(certificateBuilder: Executor = ScrollCountExecutor.shared): LayoutMemos =
    LayoutMemos(certificateBuilder, () => Some(measurer))

  /** A wrap cache over [[layoutMemos]], for specs that lay text out as a GUI owner does. */
  def wrapCache(): WrappedLineCache.Bounded = WrappedLineCache.bounded(layoutMemos = layoutMemos())

  /** Measures every wrap afresh with the GUI owner's measurer and remembers none, the reference a remembering cache is
    * compared with: what [[WrappedLineCache.Uncached]], which has no measurer, would be for a cell grid.
    */
  def measuringWrap(): WrappedLineCache.Measuring = WrappedLineCache.measuring(layoutMemos())

  /** The anchor wraps a GUI owner's state carries: measured, as the renderer lays the same rows out. */
  def anchorWraps(): AnchorWrapCache = AnchorWrapCache()(() => Some(measurer))

  /** What `Main` hands a GUI `StateManager` to measure with: the measurer of the faces and stretches its owner's
    * surfaces draw with.
    */
  val layoutMeasurerFor: BackendCaches => Option[TextMeasurer] = owner => Some(SkiaTextMeasurer.of(owner))

  /** What `Main` shares between its owners, made once for the test JVM, so the fonts one spec opens are the fonts the
    * next one finds.
    */
  val sharedBackendCaches: CacheSlots = new CacheSlots

  /** A GUI owner's caches: layout measures with the measurer of the faces and stretches its surfaces draw with, as
    * `Main` wires it. The measurer is made the first time something measures, so a spec may still install a font of its
    * own into the owner's backend caches before then.
    */
  def renderCaches(
    rendererFrameStateCacheCapacity: Int = 64,
    scrollCounting: ScrollCounting = ScrollCounting.Inline,
    styledFonts: StyledFontCache = StyledFontCache()
  ): RenderCaches =
    RenderCaches.create(
      rendererFrameStateCacheCapacity,
      scrollCounting,
      styledFonts,
      measurerFor = layoutMeasurerFor,
      sharedBackendCaches = sharedBackendCaches
    )
