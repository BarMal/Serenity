package com.serenity.ui.tui

import java.awt.Font
import java.io.StringWriter

import com.serenity.TestWorkspaceTrees
import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererCursorOverlay, RendererEntryPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The terminal has no fractions of a row, so a heading's space becomes blank rows: its em rounded to whole rows, half
  * a row or more being one. They hold no text and no caret, line numbers skip them, and a click in one lands on the
  * heading.
  */
class HeadingSpacingTuiSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val viewport = ViewportSize(60, 24)
  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def paragraph(text: String, role: ParagraphRole = ParagraphRole.Body): RichTextParagraph =
    RichTextParagraph.plain(text, role = role)

  private val document = RichTextDocument(
    List(
      paragraph("Opening line."),
      paragraph("Headline", ParagraphRole.Heading(1)),
      paragraph("Under the headline."),
      paragraph("Subhead", ParagraphRole.Heading(2)),
      paragraph("Final line.")
    )
  )

  private def spacing(h1: (Float, Float), h2: (Float, Float)): AppConfig =
    AppConfig.conventional
      .withLineNumbers(false)
      .withHeadingsConfig(
        HeadingsConfig()
          .withLevel(1)(_.copy(spaceBefore = Some(h1._1), spaceAfter = Some(h1._2)))
          .withLevel(2)(_.copy(spaceBefore = Some(h2._1), spaceAfter = Some(h2._2)))
      )

  private def stateOf(
    config: AppConfig,
    doc: RichTextDocument = document,
    cursor: CursorPosition = CursorPosition(0, 0)
  ) =
    val base = Buffer.fromString(bufferId, doc.plainText)
    val buffer = base.copy(
      richText = RichTextState(richTextDocument = Some(doc)),
      editing = EditingState(List(cursor)),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = viewport.width, visibleLines = viewport.height)
    )
    val initial = AppState.initial(AppConfig.conventional)
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      ),
      runtime = initial.runtime.copy(capabilities = FrontendCapabilities.tui(), viewportSize = Some(viewport))
    )

  private def render(state: AppState, cursorVisible: Boolean = false): TerminalEmulator =
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(viewport.width, viewport.height, writer, CellMetrics.cellUnit)
    val caches  = RenderCaches.create()
    RendererEntryPoints.render(
      state,
      cursorVisible,
      surface,
      viewport,
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      CellMetrics.cellUnit,
      None,
      caches
    )
    surface.flush()
    if cursorVisible then
      val _ = RendererCursorOverlay.renderCursorOnly(
        state,
        cursorVisible,
        surface,
        viewport,
        FontSpec.fromAwt(font),
        FontSpec.fromAwt(font),
        FontSpec.fromAwt(font),
        CellMetrics.cellUnit,
        CellMetrics.cellUnit,
        None,
        caches
      )
    TerminalEmulator.blank(viewport.width, viewport.height).consume(writer.toString)

  private def rowOf(screen: TerminalEmulator, text: String): Int =
    screen.find(text).map(_._2).getOrElse(fail(s"no '$text' on screen:\n${screen.render}"))

  private val texts = List("Opening line.", "Headline", "Under the headline.", "Subhead", "Final line.")

  private def rowsOf(screen: TerminalEmulator): List[Int] = texts.map(rowOf(screen, _))

  private def gaps(rows: List[Int]): List[Int] = rows.zip(rows.tail).map((a, b) => b - a)

  "A terminal heading" should "sit on consecutive rows while its space is automatic" in {
    gaps(rowsOf(render(stateOf(AppConfig.conventional.withLineNumbers(false))))) shouldBe List(1, 1, 1, 1)
  }

  it should "get one blank row for half a row of space or more, rounding to whole rows" in {
    val screen = render(stateOf(spacing(h1 = (0.5f, 1.4f), h2 = (2.0f, 0.49f))))

    // before H1 = 1 row, after H1 = 1 row, before H2 = 2 rows, after H2 = 0 rows
    gaps(rowsOf(screen)) shouldBe List(2, 2, 3, 1)
  }

  it should "leave the blank rows empty" in {
    val screen = render(stateOf(spacing(h1 = (1.0f, 1.0f), h2 = (2.0f, 1.0f))))
    val rows   = rowsOf(screen).toSet

    (rows.min to rows.max).filterNot(rows.contains).foreach { row =>
      withClue(s"row $row: ")(screen.rowText(row).trim shouldBe empty)
    }
  }

  it should "take no space above a heading that is the first line of the document" in {
    val doc    = RichTextDocument(List(paragraph("Headline", ParagraphRole.Heading(1)), paragraph("Body line.")))
    val screen = render(stateOf(spacing(h1 = (3.0f, 2.0f), h2 = (0.0f, 0.0f)), doc))
    val plain  = render(stateOf(AppConfig.conventional.withLineNumbers(false), doc))

    rowOf(screen, "Headline") shouldBe rowOf(plain, "Headline")
    rowOf(screen, "Body line.") - rowOf(screen, "Headline") shouldBe 3
  }

  it should "put the caret on the heading's own row, never in the blank rows" in {
    val config    = spacing(h1 = (1.0f, 1.0f), h2 = (2.0f, 1.0f))
    val atHeading = render(stateOf(config, cursor = CursorPosition(1, 3)), cursorVisible = true)
    val atSubhead = render(stateOf(config, cursor = CursorPosition(3, 2)), cursorVisible = true)

    atHeading.cursor.row shouldBe rowOf(atHeading, "Headline")
    atSubhead.cursor.row shouldBe rowOf(atSubhead, "Subhead")
    atSubhead.cursor.col should be > 0
  }

  it should "align a line number with its text, leaving the blank rows without one" in {
    val screen = render(stateOf(spacing(h1 = (1.0f, 1.0f), h2 = (1.0f, 1.0f)).withLineNumbers(true)))
    val rows   = rowsOf(screen)
    val digits = List("1", "2", "3", "4", "5")

    rows.zip(digits).foreach((row, digit) => withClue(s"row $row: ")(screen.rowText(row).trim should startWith(digit)))
    (rows.head to rows.last).filterNot(rows.contains).foreach { row =>
      withClue(s"blank row $row: ")(screen.rowText(row).trim shouldBe empty)
    }
  }
