package com.serenity.perf

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.atomic.AtomicInteger

import cats.effect.unsafe.IORuntime
import com.serenity.config.AppMode
import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Buffer, BufferId, CursorPosition, EditingState, PaneId}
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{
  ColdFontProbes,
  LayoutEngine,
  LayoutMemos,
  ProseColumn,
  ScreenPosition,
  TextLayoutSnapshot,
  WrappedLineCache
}
import com.serenity.ui.renderer.ShapedTextCache

/** Scenarios that make a move of a process-wide cache into state a runtime owns measurable: what such a move costs when
  * the cache is cold, and what two runtimes in one process stop sharing.
  *
  *   - `layout.anchor_wrap.*`: locating a caret through `LayoutEngine.laidOutPanes`, whose wrap cache was process-wide
  *     and is now the state's own (`Runtime.anchorWraps`).
  *   - `layout.cold_font.*`: the first use of a font. Every call is given a font no cache has seen, so each row times
  *     the probe that fills one cache: `GlyphAdvances`, `ShapingBarriers`, `FontLineExtents`, `ProseColumn`, and
  *     `TextLayoutSnapshot`'s measured-layout memo. A cache a runtime owns is cold for every new runtime, not only for
  *     the first in the process.
  *   - `laptop.input.state_manager.two_runtimes.*`: two `StateManager`s in one process, the second typing the text the
  *     first just typed (`same_text`) or text of its own (`different_text`). What `same_text` saves over
  *     `different_text` is the work one runtime's caches do for the other, which owned caches no longer share.
  */
private[perf] object CacheOwnershipBenchmarks:

  given Balance = Balance.default

  private val Warmups    = 3
  private val Iterations = 20

  def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    anchorWrapBenchmarks ++ coldFontBenchmarks ++ twoRuntimeBenchmarks

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def caretCell(state: AppState): Option[ScreenPosition] =
    for
      viewport <- state.runtime.viewportSize
      cursor   <- state.persisted.buffers.get(bufferId).flatMap(_.editing.cursorPositions.headOption)
      layout = LayoutEngine.calculateLayout(state, viewport)
      cell <- LayoutEngine.laidOutPanes(state, layout).screenPosition(paneId, cursor)
    yield cell

  /** `proseState` parks its cursor far down the document, below the first screenful its viewport shows. */
  private def cursorOnScreen(state: AppState): AppState =
    val moved = state.persisted.buffers.get(bufferId).map(_.copy(editing = EditingState(List(CursorPosition(2, 40)))))
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers ++ moved.map(bufferId -> _)))

  private def typed(letter: Char, state: AppState): AppState =
    EditorEventReducer.reduce(InsertChar(letter), paneId, state).state

  /** `warm` asks for the caret of one unchanged state, which the cache answers after the first call. `after_edit` types
    * a letter first, so the edited line's wrap misses; `after_edit.reduce_only` is that without the lookup, to
    * subtract.
    */
  private def anchorWrapBenchmarks: List[BenchmarkRunner.Benchmark] =
    val unchanged = cursorOnScreen(LaptopFrameBenchmarks.proseState)
    val expected  = caretCell(unchanged)
    def editing(): java.util.concurrent.atomic.AtomicReference[AppState] =
      java.util.concurrent.atomic.AtomicReference(unchanged)
    val withLookup = editing()
    val reduceOnly = editing()
    val letters    = BenchmarkFixtures.randomLetters(seed = 7L)
    List(
      BenchmarkRunner.Benchmark(
        "layout.anchor_wrap.warm_caret_lookup",
        Warmups,
        BenchmarkIterationCounts.LayoutVisibleViewport,
        () => assert(expected.nonEmpty && caretCell(unchanged) == expected, "caret lookup found no cell"),
        () => caretCell(unchanged),
        settleJit = true
      ),
      BenchmarkRunner.Benchmark(
        "layout.anchor_wrap.after_edit.caret_lookup",
        Warmups,
        Iterations,
        () => assert(caretCell(withLookup.updateAndGet(typed(letters.next(), _))).nonEmpty, "edited caret has no cell"),
        () => caretCell(withLookup.updateAndGet(typed(letters.next(), _))),
        fixedSampleCount = true
      ),
      BenchmarkRunner.Benchmark(
        "layout.anchor_wrap.after_edit.reduce_only",
        Warmups,
        Iterations,
        () => assert(reduceOnly.updateAndGet(typed(letters.next(), _)) ne unchanged, "typing changed nothing"),
        () => reduceOnly.updateAndGet(typed(letters.next(), _)),
        fixedSampleCount = true
      )
    )

  /** Gives every call a font whose size no earlier call used, so no cache keyed by font can answer. */
  final private class FreshFonts(base: Font):
    private val issued = AtomicInteger(0)
    def next(): Font   = base.deriveFont(base.getSize2D + issued.incrementAndGet() * 0.001f)

  private val proseBase = Font(Font.SERIF, Font.PLAIN, 14)
  private val codeBase  = Font(Font.MONOSPACED, Font.PLAIN, 12)

  /** `verify` compares the cold answer with the cached one for the same font, so a row that stops reaching its probe
    * through a cache fails here rather than timing something else.
    */
  private def coldProbe[A](name: String, base: Font)(probe: (Font, FontRenderContext) => A)(
    sane: A => Boolean
  ): BenchmarkRunner.Benchmark =
    val fonts = FreshFonts(base)
    val frc   = TextLayoutSnapshot.defaultFontRenderContext()
    BenchmarkRunner.Benchmark(
      name,
      Warmups,
      Iterations,
      () =>
        val font = fonts.next()
        val cold = probe(font, frc)
        assert(sane(cold) && probe(font, frc) == cold, s"$name: cold and cached answers differ or are empty")
      ,
      () => probe(fonts.next(), frc),
      fixedSampleCount = true
    )

  private def proseColumnState(textFontSize: Float): AppState =
    val config = LaptopFrameBenchmarks.proseState.persisted.config
      .withFontConfig(FontConfig(textFontSize = textFontSize))
      .withAppMode(AppMode.Prose)
      .withProseMeasure(Some(66))
    val state = LaptopFrameBenchmarks.proseState
    state.copy(persisted = state.persisted.copy(config = config))

  private def coldFontBenchmarks: List[BenchmarkRunner.Benchmark] =
    val proseSizes    = AtomicInteger(0)
    val snapshotFonts = FreshFonts(proseBase)
    val document      = Buffer.fromString(bufferId, BenchmarkFixtures.loremIpsumProse(paragraphs = 3))
    List(
      coldProbe("layout.cold_font.glyph_advances", proseBase)(ColdFontProbes.glyphAdvances)(_._4 > 0),
      coldProbe("layout.cold_font.shaping_barriers", proseBase)(ColdFontProbes.shapingBarriers)(_ >= 1),
      coldProbe("layout.cold_font.font_line_extents", proseBase)(ColdFontProbes.lineExtents)(_.heightPx > 0),
      coldProbe("layout.cold_font.measured_layout_memo.proportional", proseBase)(ColdFontProbes.measuredLayoutDecision)(
        identity
      ),
      coldProbe("layout.cold_font.measured_layout_memo.monospaced", codeBase)(ColdFontProbes.measuredLayoutDecision)(
        _ => true
      ),
      BenchmarkRunner.Benchmark(
        "layout.cold_font.prose_column",
        Warmups,
        Iterations,
        () =>
          val state = proseColumnState(14f + proseSizes.incrementAndGet() * 0.001f)
          assert(!state.runtime.capabilities.isCellGrid, "a cell grid never measures the prose font")
          assert(ProseColumn.widthCells(state).nonEmpty, "prose column measure was not applied")
        ,
        () => ProseColumn.widthCells(proseColumnState(14f + proseSizes.incrementAndGet() * 0.001f)),
        fixedSampleCount = true
      ),
      BenchmarkRunner.Benchmark(
        "layout.cold_font.text_layout_snapshot",
        Warmups,
        Iterations,
        () => assert(snapshotOf(document, snapshotFonts.next()).visualLines.nonEmpty, "snapshot has no rows"),
        () => snapshotOf(document, snapshotFonts.next()),
        fixedSampleCount = true
      )
    )

  private def snapshotOf(buffer: Buffer, font: Font): TextLayoutSnapshot =
    TextLayoutSnapshot.fromBuffer(
      buffer,
      panelWidthPx = 900,
      font = font,
      wrapCache = WrappedLineCache.measuring(ShapedTextCache(), LayoutMemos())
    )

  /** Both runtimes start from the same document. The second types the first's letter (`same_text`) or the same letter
    * upper-cased (`different_text`), so its edited line is one the first has just wrapped, or one nobody has.
    */
  private def twoRuntimeBenchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    List(
      twoRuntimes("laptop.input.state_manager.two_runtimes.same_text", identity),
      twoRuntimes("laptop.input.state_manager.two_runtimes.different_text", distinctFrom)
    )

  private def distinctFrom(letter: Char): Char = if letter.isLetter then letter.toUpper else '-'

  private def twoRuntimes(name: String, secondRuntimeLetter: Char => Char)(using
    IORuntime
  ): BenchmarkRunner.Benchmark =
    val (first, firstBuffer)   = LaptopFrameBenchmarks.proseStateManager()
    val (second, secondBuffer) = LaptopFrameBenchmarks.proseStateManager()
    def weight(manager: StateManager, id: BufferId): Option[Int] =
      manager.getCurrentState.unsafeRunSync().persisted.buffers.get(id).map(_.document.content.weight)
    val letters = BenchmarkFixtures.randomLetters(seed = 42L)
    def keystroke(): Unit =
      val letter = letters.next()
      first.applyEvent(InsertChar(letter)).unsafeRunSync()
      second.applyEvent(InsertChar(secondRuntimeLetter(letter))).unsafeRunSync()
    BenchmarkRunner.Benchmark(
      name,
      Warmups,
      BenchmarkIterationCounts.RandomTyping,
      () =>
        val (before1, before2) = (weight(first, firstBuffer), weight(second, secondBuffer))
        keystroke()
        assert(
          weight(first, firstBuffer) == before1.map(_ + 1) && weight(second, secondBuffer) == before2.map(_ + 1),
          s"both runtimes should have grown by one character: $before1/$before2"
        )
      ,
      () => keystroke(),
      fixedSampleCount = true
    )

end CacheOwnershipBenchmarks
