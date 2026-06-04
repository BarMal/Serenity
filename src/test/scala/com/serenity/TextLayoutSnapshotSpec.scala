package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, CursorPosition, Viewport}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class TextLayoutSnapshotSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

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

  it should "capture non-uniform caret advances for proportional text fonts" in {
    val buffer = Buffer
      .fromString(BufferId(3), "WiWi")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2))
    val font = FontLoader.loadTextFont(
      FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
    ).unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val deltas   = proportionalDeltas(snapshot)

    deltas.distinct.size should be > 1
  }

  it should "map a visual row and measured x position back to the nearest logical cursor column" in {
    val buffer = Buffer
      .fromString(BufferId(4), "iW")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 2))
    val font = FontLoader.loadTextFont(
      FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
    ).unsafeRunSync()

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
    val font = FontLoader.loadTextFont(
      FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
    ).unsafeRunSync()

    val snapshot      = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val secondLineEnd = CursorPosition(1, 4)
    val preferredXPx  = snapshot.xPxForCursor(secondLineEnd).getOrElse(fail("missing caret x"))
    val firstLine     = snapshot.visualLines.head
    val expectedCol = firstLine.caretStops.minBy(stop => math.abs(stop.xPx - preferredXPx)).column

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
    val font = FontLoader.loadTextFont(
      FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
    ).unsafeRunSync()

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
    val font = FontLoader.loadCodeFont(
      FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, fontSize = 12.0f, enableLigatures = true)
    ).unsafeRunSync()

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 400, font)
    val xs       = snapshot.visualLines.head.caretStops.map(_.xPx)

    xs should have size 4
    xs.sliding(2).forall {
      case Vector(a, b) => b > a
      case _            => true
    } shouldBe true
  }
