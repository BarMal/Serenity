package com.serenity.state.manager

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.frontend.FrontendCapabilities
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.LineRange
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Pins what `Viewport.topLine` and `Viewport.visibleLines` mean on screen, against the rows `TextLayoutSnapshot`
  * actually lays out from them, in wrapped and unwrapped modes: the lines a language server is told are visible.
  */
class VisibleBufferLinesSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val font        = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
  private val charWidthPx = CellMetrics.fromFont(font).charWidth

  private def bufferOf(text: String, viewport: Viewport): Buffer =
    Buffer.fromString(BufferId(1), text).copy(viewport = viewport)

  private def viewport(topLine: Int, visibleLines: Int, topVisualLine: Int = 0): Viewport =
    Viewport(
      topLine = topLine,
      leftColumn = 0,
      visibleLines = visibleLines,
      visibleColumns = 10,
      topVisualLine = topVisualLine
    )

  private def shownLines(buffer: Buffer, wordWrapEnabled: Boolean): Vector[Int] =
    TextLayoutSnapshot
      .fromBuffer(buffer, panelWidthPx = charWidthPx * 10, font, wordWrapEnabled = wordWrapEnabled)
      .visualLines
      .map(_.bufferLine)
      .distinct

  private val shortLines = (0 until 100).map(n => s"line $n").mkString("\n")
  private val wrapsInTwo = (0 until 100).map(_ => "x" * 20).mkString("\n")

  private def withSurface(update: com.serenity.config.SurfaceConfig => com.serenity.config.SurfaceConfig): AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(config =
        initial.persisted.config.copy(surfaceConfig = update(initial.persisted.config.surfaceConfig))
      )
    )

  "VisibleBufferLines.range" should "be exactly the lines on screen when lines do not wrap" in {
    val buffer = bufferOf(shortLines, viewport(topLine = 10, visibleLines = 8))

    val shown = shownLines(buffer, wordWrapEnabled = false)

    shown shouldBe (10 to 17).toVector
    VisibleBufferLines.range(10, 8, 100) shouldBe LineRange(shown.min, shown.max)
  }

  it should "start at the first line on screen and not stop short of the last when lines wrap" in {
    val buffer = bufferOf(wrapsInTwo, viewport(topLine = 10, visibleLines = 8))

    val shown = shownLines(buffer, wordWrapEnabled = true)
    val range = VisibleBufferLines.range(10, 8, 100)

    shown shouldBe (10 to 13).toVector
    range.first shouldBe shown.min
    range.last should be >= shown.max
  }

  it should "start at the line a wrapped viewport is scrolled partway into" in {
    val buffer = bufferOf(wrapsInTwo, viewport(topLine = 10, visibleLines = 8, topVisualLine = 1))

    val shown = shownLines(buffer, wordWrapEnabled = true)

    shown.min shouldBe 10
    VisibleBufferLines.range(10, 8, 100).first shouldBe shown.min
    VisibleBufferLines.range(10, 8, 100).last should be >= shown.max
  }

  it should "stop at the last line of the document" in {
    val buffer = bufferOf(shortLines, viewport(topLine = 95, visibleLines = 8))

    VisibleBufferLines.range(95, 8, 100) shouldBe LineRange(95, 99)
    shownLines(buffer, wordWrapEnabled = false) shouldBe (95 to 99).toVector
  }

  it should "stay inside a document that is shorter than the viewport, or empty" in {
    VisibleBufferLines.range(0, 50, 3) shouldBe LineRange(0, 2)
    VisibleBufferLines.range(40, 10, 3) shouldBe LineRange(2, 2)
    VisibleBufferLines.range(0, 10, 1) shouldBe LineRange(0, 0)
  }

  "VisibleBufferLines.of" should "count the rows the viewport says on a cell grid" in {
    val state =
      AppState.initial.copy(runtime = AppState.initial.runtime.copy(capabilities = FrontendCapabilities.tui()))
    val buffer = bufferOf(shortLines, viewport(topLine = 10, visibleLines = 8))

    VisibleBufferLines.of(buffer, state) shouldBe LineRange(10, 17)
  }

  it should "count the rows the viewport says for a buffer drawn in the code font" in {
    val plain  = bufferOf(shortLines, viewport(topLine = 10, visibleLines = 8))
    val buffer = plain.copy(document = plain.document.copy(language = Some(LanguageId.Scala)))

    VisibleBufferLines.of(buffer, AppState.initial) shouldBe LineRange(10, 17)
  }

  it should "reach the end of the document in column mode, where several screens sit side by side" in {
    val state  = withSurface(_.copy(wordWrapEnabled = true, columnModeEnabled = true))
    val buffer = bufferOf(shortLines, viewport(topLine = 10, visibleLines = 8))

    VisibleBufferLines.of(buffer, state) shouldBe LineRange(10, 99)
  }
