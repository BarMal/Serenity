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

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
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

    leftColumn should be < 7
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
