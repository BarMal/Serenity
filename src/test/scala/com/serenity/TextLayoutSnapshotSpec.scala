package com.serenity

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.richtext.{ParagraphAlignment, RichTextDocument, RichTextParagraph}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class TextLayoutSnapshotSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  final case class CountingLineCountRope(delegate: Rope, lineCountCount: AtomicInteger) extends Rope:
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override def newlineCount: Int =
      delegate.newlineCount

    override def lastLineLength: Int =
      delegate.lastLineLength

    override def endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def lineCount: Int =
      lineCountCount.incrementAndGet()
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      delegate.getLine(lineIndex)

    override def collect(): String =
      delegate.collect()

  private def proportionalDeltas(snapshot: TextLayoutSnapshot): Vector[Float] =
    snapshot.visualLines.headOption.toVector.flatMap { line =>
      line.caretStops.sliding(2).collect { case Vector(a, b) => b.xPx - a.xPx }.toVector
    }

  "TextLayoutSnapshot" should "derive visible wrapped lines for the current buffer viewport" in {
    val buffer = Buffer
      .fromString(BufferId(1), "abcdefghij")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 5, visibleLines = 4))
    val font = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 24, font)

    snapshot.visualLines.map(_.text) shouldBe Vector("abc", "def", "ghi", "j")
    snapshot.visualLines.map(line => line.startColumn -> line.endColumn) shouldBe
      Vector(0 -> 3, 3 -> 6, 6 -> 9, 9 -> 10)
  }

  it should "prefer word boundaries when wrapping text" in {
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)
    val buffer = Buffer
      .fromString(BufferId(8), "hello world again")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 8, visibleLines = 4))

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 8, font)

    snapshot.visualLines.map(_.text) shouldBe Vector("hello ", "world ", "again")
  }

  it should "keep logical lines unwrapped when word wrap is disabled" in {
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)
    val buffer = Buffer
      .fromString(BufferId(12), "hello world again")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 4))

    val snapshot =
      TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 8, font, wordWrapEnabled = false)

    snapshot.visualLines.map(_.text) shouldBe Vector("hello world again")
    TextLayoutSnapshot.visualLineIndexForCursor(
      "hello world again",
      cursorColumn = 12,
      panelWidthPx = metrics.charWidth * 8,
      font,
      wordWrapEnabled = false
    ) shouldBe 0
  }

  it should "read the buffer line count once while collecting visible lines" in {
    val lineCountCount = AtomicInteger(0)
    val content        = CountingLineCountRope(Rope((1 to 100).map(i => s"line-$i").mkString("\n")), lineCountCount)
    val buffer = Buffer
      .fromString(BufferId(13), "")
      .copy(
        content = content,
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 8)
      )
    val font = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()

    TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)

    lineCountCount.get() shouldBe 1
  }

  it should "limit unwrapped visual lines to the visible viewport" in {
    val font = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val buffer = Buffer
      .fromString(BufferId(14), "abcdefghijklmnopqrstuvwxyz")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 2, visibleColumns = 5, visibleLines = 4))

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font, wordWrapEnabled = false)

    snapshot.visualLines.map(_.text) shouldBe Vector("cdefghi")
    snapshot.visualLines.map(line => line.startColumn -> line.endColumn) shouldBe Vector(2 -> 9)
  }

  it should "skip wrapped visual rows from the top of a long logical line" in {
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)
    val buffer = Buffer
      .fromString(BufferId(9), "alpha beta gamma")
      .copy(
        viewport = Viewport(
          topLine = 0,
          leftColumn = 0,
          visibleColumns = 8,
          visibleLines = 2,
          topVisualLine = 1
        )
      )

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 8, font)

    snapshot.visualLines.map(_.text) shouldBe Vector("beta ", "gamma")
  }

  it should "ignore horizontal scroll when word wrap is enabled" in {
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)
    val buffer = Buffer
      .fromString(BufferId(16), "alpha beta gamma")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 6, visibleColumns = 8, visibleLines = 3))

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 8, font)

    snapshot.visualLines.map(_.text) shouldBe Vector("alpha ", "beta ", "gamma")
    snapshot.visualLines.map(line => line.startColumn -> line.endColumn) shouldBe Vector(0 -> 6, 6 -> 11, 11 -> 16)
  }

  it should "bound wrapped line shaping to the requested visible rows" in {
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)

    val lines = TextLayoutSnapshot.boundedVisualLinesForText(
      text = "abcdefghijkl",
      bufferLine = 0,
      panelWidthPx = metrics.charWidth * 3,
      font = font,
      maxVisualLines = 2
    )

    lines.map(_.text) shouldBe Vector("abc", "def")
  }

  it should "shape only visible wrapped rows from a large logical line" in {
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)
    val buffer = Buffer
      .fromString(BufferId(15), "abcdefghijklmnopqrstuvwxyz" * 100)
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 3, visibleLines = 2))

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 3, font)

    snapshot.visualLines.map(_.text) shouldBe Vector("abc", "def")
  }

  it should "track caret stops for every logical column in a visual segment" in {
    val buffer = Buffer
      .fromString(BufferId(2), "office")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2))
    val font = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val line     = snapshot.visualLines.head

    line.caretStops.map(_.column) shouldBe Vector(0, 1, 2, 3, 4, 5, 6)
    line.caretStops.map(_.xPx).sliding(2).forall {
      case Vector(a, b) => b >= a
      case _            => true
    } shouldBe true
    line.xForColumn(0) shouldBe Some(0.0f)
    line.xForColumn(6) shouldBe Some(line.widthPx)
  }

  it should "expose caret stops only at grapheme boundaries" in {
    val buffer = Buffer
      .fromString(BufferId(17), "a\uD83D\uDE42b cafe\u0301!")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2))
    val font = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font, wordWrapEnabled = false)
    val line     = snapshot.visualLines.head

    line.caretStops.map(_.column) shouldBe Vector(0, 1, 3, 4, 5, 6, 7, 8, 10, 11)
    line.xForColumn(2) shouldBe line.xForColumn(3)
    line.xForColumn(9) shouldBe line.xForColumn(10)
    snapshot.cursorForVisualRowAndXPx(0, line.xForColumn(2).getOrElse(fail("missing caret x"))) shouldBe Some(
      CursorPosition(0, 3)
    )
  }

  it should "offset center-aligned rich text caret stops within the panel width" in {
    val document = RichTextDocument(
      List(RichTextParagraph.plain("abcd", alignment = ParagraphAlignment.Center))
    )
    val buffer = Buffer
      .fromString(BufferId(10), document.plainText)
      .copy(
        richTextDocument = Some(document),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2)
      )
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)

    val snapshot       = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 10, font)
    val line           = snapshot.visualLines.head
    val expectedOffset = ((metrics.charWidth * 10).toFloat - line.widthPx) / 2.0f

    line.xOffsetPx shouldBe expectedOffset
    line.xForColumn(0) shouldBe Some(expectedOffset)
    line.xForColumn(4) shouldBe Some(expectedOffset + line.widthPx)
    snapshot.cursorForVisualRowAndXPx(0, expectedOffset) shouldBe Some(CursorPosition(0, 0))
  }

  it should "skip rich text alignment when the metadata shape no longer matches the buffer" in {
    val document = RichTextDocument(
      List(RichTextParagraph.plain("abcd", alignment = ParagraphAlignment.Center))
    )
    val buffer = Buffer
      .fromString(BufferId(16), "abcd\nefgh")
      .copy(
        richTextDocument = Some(document),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2)
      )
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 10, font)

    snapshot.richTextDocument shouldBe None
    snapshot.visualLines.head.xOffsetPx shouldBe 0.0f
  }

  it should "offset right-aligned rich text caret stops within the panel width" in {
    val document = RichTextDocument(
      List(RichTextParagraph.plain("abcd", alignment = ParagraphAlignment.Right))
    )
    val buffer = Buffer
      .fromString(BufferId(11), document.plainText)
      .copy(
        richTextDocument = Some(document),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2)
      )
    val font    = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val metrics = CellMetrics.fromFont(font)

    val snapshot       = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 10, font)
    val line           = snapshot.visualLines.head
    val expectedOffset = (metrics.charWidth * 10).toFloat - line.widthPx

    line.xOffsetPx shouldBe expectedOffset
    line.xForColumn(0) shouldBe Some(expectedOffset)
    line.xForColumn(4) shouldBe Some(expectedOffset + line.widthPx)
  }

  it should "capture non-uniform caret advances for proportional text fonts" in {
    val buffer = Buffer
      .fromString(BufferId(3), "WiWi")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2))
    val font = FontLoader
      .loadTextFont(
        FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
      )
      .unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val deltas   = proportionalDeltas(snapshot)

    deltas.distinct.size should be > 1
  }

  it should "map a visual row and measured x position back to the nearest logical cursor column" in {
    val buffer = Buffer
      .fromString(BufferId(4), "iW")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2))
    val font = FontLoader
      .loadTextFont(
        FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
      )
      .unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val line     = snapshot.visualLines.head
    val xNearW   = line.xForColumn(1).getOrElse(fail("missing caret stop")) + 0.1f

    snapshot.cursorForVisualRowAndXPx(0, xNearW) shouldBe Some(CursorPosition(0, 1))
    snapshot.cursorForVisualRowAndXPx(0, line.widthPx) shouldBe Some(CursorPosition(0, 2))
  }

  it should "hit-test RTL caret stops without assuming x order" in {
    val caretStops = Vector(
      com.serenity.ui.layout.TextCaretStop(0, 26.5f),
      com.serenity.ui.layout.TextCaretStop(1, 15.8f),
      com.serenity.ui.layout.TextCaretStop(2, 6.6f),
      com.serenity.ui.layout.TextCaretStop(3, 26.5f)
    )
    val line = com.serenity.ui.layout.TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = 3,
      text = "אבג",
      widthPx = 26.5f,
      caretStops = caretStops,
      xSortedCaretStops = caretStops.sortBy(_.xPx)
    )

    line.nearestColumnForXPx(15.8f) shouldBe 1
    line.nearestColumnForXPx(6.6f) shouldBe 2
  }

  it should "hit-test a long line through its x-sorted caret index" in {
    val caretStops = Vector.tabulate(20_001)(column => com.serenity.ui.layout.TextCaretStop(column, column * 0.5f))
    val line = com.serenity.ui.layout.TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = 20_000,
      text = "x" * 20_000,
      widthPx = 10_000.0f,
      caretStops = caretStops,
      xSortedCaretStops = caretStops
    )

    line.nearestColumnForXPx(6_789.6f) shouldBe 13_579
  }

  it should "retain grapheme-boundary caret positions for a long measured line" in {
    val text = "Wi" * 1_000
    val font = FontLoader
      .loadTextFont(
        FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
      )
      .unsafeRunSync()

    val line = TextLayoutSnapshot.visualLineForText(text, bufferLine = 0, font)

    line.caretStops.map(_.column) shouldBe (0 to text.length).toVector
    line.caretStops.sliding(2).forall {
      case Vector(first, second) => second.xPx >= first.xPx
      case _                     => true
    } shouldBe true
  }

  it should "move vertically using measured caret x rather than raw logical columns" in {
    val buffer = Buffer
      .fromString(BufferId(5), "WWWW\niiii")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 4))
    val font = FontLoader
      .loadTextFont(
        FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
      )
      .unsafeRunSync()

    val snapshot      = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val secondLineEnd = CursorPosition(1, 4)
    val preferredXPx  = snapshot.xPxForCursor(secondLineEnd).getOrElse(fail("missing caret x"))
    val firstLine     = snapshot.visualLines.head
    val expectedCol   = firstLine.caretStops.minBy(stop => math.abs(stop.xPx - preferredXPx)).column

    snapshot.moveVertical(secondLineEnd, direction = -1, preferredXPx = preferredXPx) shouldBe
      Some(CursorPosition(0, expectedCol))
  }

  it should "respect the viewport left column when deriving visible text" in {
    val buffer = Buffer
      .fromString(BufferId(6), "abcdef")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 2, visibleColumns = 20, visibleLines = 2))
    val font = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font, wordWrapEnabled = false)
    val line     = snapshot.visualLines.head

    line.text shouldBe "cdef"
    line.startColumn shouldBe 2
    line.endColumn shouldBe 6
  }

  it should "derive a measured left column for cursor visibility in proportional text" in {
    val font = FontLoader
      .loadTextFont(
        FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
      )
      .unsafeRunSync()

    val leftColumn = TextLayoutSnapshot.leftColumnForCursorVisibility(
      lineText = "iiiiiiiiWW",
      cursorColumn = 10,
      visibleWidthPx = CellMetrics.fromFont(font).charWidth * 4,
      font = font
    )

    leftColumn should be <= 7
    leftColumn should be >= 0
  }

  it should "redistribute collapsed caret stops inside ligature clusters so repeated punctuation remains navigable" in {
    val buffer = Buffer
      .fromString(BufferId(7), "...")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2))
    val font = FontLoader
      .loadCodeFont(
        FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, fontSize = 12.0f, enableLigatures = true)
      )
      .unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val xs       = snapshot.visualLines.head.caretStops.map(_.xPx)

    xs should have size 4
    xs.sliding(2).forall {
      case Vector(a, b) => b > a
      case _            => true
    } shouldBe true
  }
