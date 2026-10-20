package com.serenity.state.manager

import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.keystroke.events.{MouseClick, MouseInputEvent}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A pointer in the space around a heading lands on the heading's nearest row -- the space is blank, so it holds no
  * caret position of its own -- and a pointer on the text lands where it always has.
  */
class HeadingSpacingMouseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(0)
  private val viewportSize = ViewportSize(70, 50)

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("Opening words of the book."),
      RichTextParagraph.plain("Chapter one", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("The first paragraph under the chapter."),
      RichTextParagraph.plain("Section", role = ParagraphRole.Heading(2)),
      RichTextParagraph.plain("The last paragraph.")
    )
  )

  private val spacing = AppConfig.default.withoutStatusLine
    .withLineNumbers(false)
    .withWordWrap(true)
    .withHeadingsConfig(
      HeadingsConfig()
        .withLevel(1)(_.copy(spaceBefore = Some(1.5f), spaceAfter = Some(1.0f)))
        .withLevel(2)(_.copy(spaceBefore = Some(1.0f), spaceAfter = Some(0.5f)))
    )

  private def stateWith(config: AppConfig): AppState =
    val base = Buffer.fromString(bufferId, document.plainText)
    val buffer =
      base.copy(
        richText = RichTextState(richTextDocument = Some(document)),
        editing = EditingState(List(CursorPosition(0, 0)))
      )
    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      ),
      runtime = initial.runtime.copy(viewportSize = Some(viewportSize))
    )

  private def cacheOf(state: AppState): MouseTargetCache =
    MouseTargetCache.fromState(state, viewportSize, AuthoritativeUiScene())

  /** Where the text of each visual row starts and ends, stacked from the pane's top by its own measured heights and
    * space -- worked out here from the rows, not from the metrics under test.
    */
  private def textSpans(state: AppState): Vector[(TextVisualLine, Int, Int)] =
    val cache       = cacheOf(state)
    val contentRect = cache.scene.paneLayouts(paneId).contentRect
    val snapshot    = cache.scene.textSnapshot(paneId).getOrElse(fail("no text snapshot"))
    val metrics     = MouseHitTestGeometry.floatingCellMetrics(state)
    val tops = snapshot.visualLines.scanLeft(metrics.toPixelY(contentRect.y)) { (top, line) =>
      top + line.spaceBeforePx + (if line.heightPx > 0 then line.heightPx
                                  else snapshot.lineHeightPx) + line.spaceAfterPx
    }
    snapshot.visualLines.zipWithIndex.map { (line, index) =>
      val textTop = tops(index) + line.spaceBeforePx
      (line, textTop, textTop + (if line.heightPx > 0 then line.heightPx else snapshot.lineHeightPx))
    }

  private def spanOf(state: AppState, bufferLine: Int): (TextVisualLine, Int, Int) =
    textSpans(state).find(_._1.bufferLine == bufferLine).getOrElse(fail(s"no row for line $bufferLine"))

  private def lastSpanOf(state: AppState, bufferLine: Int): (TextVisualLine, Int, Int) =
    textSpans(state).findLast(_._1.bufferLine == bufferLine).getOrElse(fail(s"no row for line $bufferLine"))

  private def clickAtPixelY(state: AppState, y: Int, x: Int = 200): MouseClick =
    val metrics = MouseHitTestGeometry.floatingCellMetrics(state)
    MouseClick(metrics.toCol(x), metrics.toRow(y), pixelX = Some(x), pixelY = Some(y))

  private def resolve(state: AppState, event: MouseInputEvent): Option[CursorPosition] =
    EditorMouseTargeting
      .targetAt(event, state, cacheOf(state))
      .fold(missing => fail(s"missing snapshot ${missing.paneId}"), identity)
      .map(_._3)

  private val state = stateWith(spacing)

  "The fixture" should "put real space around the headings and none around body text" in {
    val (chapter, _, _) = spanOf(state, 1)
    val (section, _, _) = spanOf(state, 3)
    val (opening, _, _) = spanOf(state, 0)
    val (body, _, _)    = spanOf(state, 2)

    chapter.spaceBeforePx should be > 0
    chapter.spaceAfterPx should be > 0
    section.spaceBeforePx should be > 0
    opening.spaceBeforePx + opening.spaceAfterPx + body.spaceBeforePx + body.spaceAfterPx shouldBe 0
  }

  "A click in the space above a heading" should "place the caret on that heading" in {
    val (chapter, top, _) = spanOf(state, 1)

    List(top - 1, top - chapter.spaceBeforePx / 2, top - chapter.spaceBeforePx).foreach { y =>
      withClue(s"y=$y: ")(resolve(state, clickAtPixelY(state, y)).map(_.line) shouldBe Some(1))
    }
  }

  "A click in the space below a heading" should "place the caret on that heading, not the paragraph under it" in {
    val (chapter, _, bottom) = lastSpanOf(state, 1)

    List(bottom, bottom + chapter.spaceAfterPx / 2, bottom + chapter.spaceAfterPx - 1).foreach { y =>
      withClue(s"y=$y: ")(resolve(state, clickAtPixelY(state, y)).map(_.line) shouldBe Some(1))
    }
  }

  it should "end where the next row's own space and text begin" in {
    val (section, sectionTop, _) = spanOf(state, 3)

    resolve(state, clickAtPixelY(state, sectionTop - section.spaceBeforePx)).map(_.line) shouldBe Some(3)
    resolve(state, clickAtPixelY(state, sectionTop - section.spaceBeforePx - 1)).map(_.line) shouldBe Some(2)
  }

  "A click on a heading's text" should "land on the heading" in {
    val (_, top, bottom) = spanOf(state, 1)

    List(top, (top + bottom) / 2, bottom - 1).foreach { y =>
      withClue(s"y=$y: ")(resolve(state, clickAtPixelY(state, y)).map(_.line) shouldBe Some(1))
    }
  }

  "A click on body text" should "land on it, below and above spaced headings alike" in {
    val (under, underTop, underBottom) = spanOf(state, 2)
    val (last, lastTop, lastBottom)    = spanOf(state, 4)
    val (opening, openTop, openBottom) = spanOf(state, 0)

    under.bufferLine shouldBe 2
    last.bufferLine shouldBe 4
    opening.bufferLine shouldBe 0
    resolve(state, clickAtPixelY(state, (underTop + underBottom) / 2)).map(_.line) shouldBe Some(2)
    resolve(state, clickAtPixelY(state, (lastTop + lastBottom) / 2)).map(_.line) shouldBe Some(4)
    resolve(state, clickAtPixelY(state, (openTop + openBottom) / 2)).map(_.line) shouldBe Some(0)
  }

  "Without any heading space a click" should "resolve rows exactly as before" in {
    val plain = stateWith(spacing.withHeadingsConfig(HeadingsConfig()))
    val rows  = textSpans(plain)

    rows.foreach { (line, top, bottom) =>
      withClue(s"line ${line.bufferLine}: ")(
        resolve(plain, clickAtPixelY(plain, (top + bottom) / 2)).map(_.line) shouldBe Some(line.bufferLine)
      )
    }
  }

  "A click in a terminal's blank rows" should "land on the heading nearest to it" in {
    val tui      = AppConfig.default.withoutStatusLine.withLineNumbers(false).withWordWrap(true)
    val terminal = stateWith(tui.withHeadingsConfig(spacing.headingsConfig))
    val cells =
      terminal.copy(runtime = terminal.runtime.copy(capabilities = com.serenity.frontend.FrontendCapabilities.tui()))
    val cache       = cacheOf(cells)
    val snapshot    = cache.scene.textSnapshot(paneId).getOrElse(fail("no text snapshot"))
    val contentRect = cache.scene.paneLayouts(paneId).contentRect
    val screenRows =
      snapshot.visualLines.scanLeft(contentRect.y)((row, line) => row + line.spaceBeforePx + 1 + line.spaceAfterPx)

    def textRow(index: Int): Int = screenRows(index) + snapshot.visualLines(index).spaceBeforePx
    def lineAt(row: Int): Option[Int] =
      EditorMouseTargeting
        .targetAt(MouseClick(contentRect.x + 2, row), cells, cache)
        .toOption
        .flatten
        .map(_._3.line)

    val chapter = snapshot.visualLines.indexWhere(_.bufferLine == 1)
    val section = snapshot.visualLines.indexWhere(_.bufferLine == 3)
    snapshot.visualLines(chapter).spaceBeforePx should be > 0
    snapshot.visualLines(chapter).spaceAfterPx should be > 0
    lineAt(textRow(chapter)) shouldBe Some(1)
    (1 to snapshot.visualLines(chapter).spaceBeforePx).foreach(gap =>
      withClue(s"$gap rows above the chapter: ")(lineAt(textRow(chapter) - gap) shouldBe Some(1))
    )
    (1 to snapshot.visualLines(chapter).spaceAfterPx).foreach(gap =>
      withClue(s"$gap rows below the chapter: ")(lineAt(textRow(chapter) + gap) shouldBe Some(1))
    )
    lineAt(textRow(chapter) + snapshot.visualLines(chapter).spaceAfterPx + 1) shouldBe Some(2)
    lineAt(textRow(section) - snapshot.visualLines(section).spaceBeforePx) shouldBe Some(3)
    lineAt(textRow(section) - snapshot.visualLines(section).spaceBeforePx - 1) shouldBe Some(2)
    lineAt(textRow(0)) shouldBe Some(0)
  }
