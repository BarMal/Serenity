package com.serenity.ui.tui

import java.awt.{Color, Font}
import java.io.StringWriter

import com.serenity.TestWorkspaceTrees
import com.serenity.config.{AppConfig, HeadingLevelConfig, HeadingsConfig}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererCursorOverlay, RendererEntryPoints}
import com.serenity.ui.theme.{HeadingColour, SyntaxElement, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** In a terminal the six heading levels are told apart by a ladder of weight, slant, underline and colour, since there
  * is no size to tell them by. Rich text and Markdown source both follow it, every level can be set under
  * `headings.hN.*`, and a rule under a heading is drawn in the row its `space_after` gives it.
  */
class HeadingStyleTuiSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val viewport = ViewportSize(60, 24)
  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val orange = RenderColor.fromRgba(0xee, 0x88, 0x22)

  private val levelTexts = List("Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot")

  private val richDocument = RichTextDocument(
    levelTexts.zipWithIndex.map((text, index) =>
      RichTextParagraph.plain(text, role = ParagraphRole.Heading(index + 1))
    ) :+ RichTextParagraph.plain("Plain body")
  )

  private val markdownText =
    levelTexts.zipWithIndex.map((text, index) => s"${"#" * (index + 1)} $text").mkString("\n") + "\nplain text"

  private def render(state: AppState, caches: RenderCaches = RenderCaches.create(), cursor: Boolean = false) =
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(viewport.width, viewport.height, writer, CellMetrics.cellUnit)
    val spec    = FontSpec.fromAwt(font)
    RendererEntryPoints.render(
      state,
      cursor,
      surface,
      viewport,
      spec,
      spec,
      CellMetrics.cellUnit,
      None,
      caches
    )
    surface.flush()
    if cursor then
      val _ = RendererCursorOverlay.renderCursorOnly(
        state,
        cursor,
        surface,
        viewport,
        spec,
        spec,
        spec,
        CellMetrics.cellUnit,
        CellMetrics.cellUnit,
        None,
        caches
      )
    TerminalEmulator.blank(viewport.width, viewport.height).consume(writer.toString)

  private def stateOf(
    buffer: Buffer,
    config: AppConfig,
    theme: Theme,
    capabilities: FrontendCapabilities,
    lineNumbers: Boolean = false
  ): AppState =
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
        theme = theme,
        config = config.withLineNumbers(lineNumbers)
      ),
      runtime = initial.runtime.copy(capabilities = capabilities, viewportSize = Some(viewport))
    )

  private def prose(
    config: AppConfig = AppConfig.conventional,
    theme: Theme = Theme.dark,
    capabilities: FrontendCapabilities = FrontendCapabilities.tui(),
    document: RichTextDocument = richDocument,
    cursor: CursorPosition = CursorPosition(0, 0),
    lineNumbers: Boolean = false
  ): AppState =
    val base = Buffer.fromString(bufferId, document.plainText)
    val buffer = base.copy(
      richText = RichTextState(richTextDocument = Some(document)),
      editing = EditingState(List(cursor)),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = viewport.width, visibleLines = viewport.height)
    )
    stateOf(buffer, config, theme, capabilities, lineNumbers)

  private def markdown(
    config: AppConfig = AppConfig.conventional,
    theme: Theme = Theme.dark,
    text: String = markdownText
  ): AppState =
    val base   = Buffer.fromString(bufferId, text)
    val buffer = base.copy(document = base.document.copy(language = Some(LanguageId.Markdown)))
    stateOf(buffer, config.withSyntaxHighlighting(true), theme, FrontendCapabilities.tui())

  private def withHeadings(headings: HeadingsConfig): AppConfig =
    AppConfig.conventional.withHeadingsConfig(headings)

  private def level(headings: HeadingsConfig, number: Int)(update: HeadingLevelConfig => HeadingLevelConfig) =
    headings.withLevel(number)(update)

  private def cellsOf(screen: TerminalEmulator, text: String) =
    val (col, row) = screen.find(text).getOrElse(fail(s"no '$text' on screen:\n${screen.render}"))
    (col until col + text.length).map(screen.cellAt(_, row))

  /** (bold, italic, underlined), which every cell of the text must agree on. */
  private def lookOf(screen: TerminalEmulator, text: String): (Boolean, Boolean, Boolean) =
    val looks = cellsOf(screen, text).map(cell => (cell.style.isBold, cell.style.isItalic, cell.style.isUnderlined))
    withClue(s"'$text' cells: ")(looks.distinct.size shouldBe 1)
    looks.head

  private def foregroundOf(screen: TerminalEmulator, text: String): Int =
    val colours = cellsOf(screen, text).map(cell => rgb(cell.fg))
    withClue(s"'$text' cells: ")(colours.distinct.size shouldBe 1)
    colours.head

  private def rgb(color: Color): Int       = color.getRGB & 0xffffff
  private def rgb(color: RenderColor): Int = rgb(color.toAwt)

  private def headingColour(theme: Theme): Int = rgb(theme.colorFor(SyntaxElement.Heading).foreground)

  private val plain = (false, false, false)

  "A rich-text heading in the terminal" should "climb a ladder of weight, slant and underline by default" in {
    val screen = render(prose())

    lookOf(screen, "Alpha") shouldBe (true, false, true)
    lookOf(screen, "Bravo") shouldBe (true, false, false)
    lookOf(screen, "Charlie") shouldBe (true, true, false)
    lookOf(screen, "Delta") shouldBe (true, false, false)
    lookOf(screen, "Echo") shouldBe (true, false, false)
    lookOf(screen, "Foxtrot") shouldBe (true, false, false)
    lookOf(screen, "Plain body") shouldBe plain
  }

  it should "underline the heading's text and not the cells beside it" in {
    val screen     = render(prose())
    val (col, row) = screen.find("Alpha").getOrElse(fail(screen.render))

    screen.cellAt(col + "Alpha".length, row).style.isUnderlined shouldBe false
    if col > 0 then screen.cellAt(col - 1, row).style.isUnderlined shouldBe false
  }

  it should "tell levels 4 to 6 apart by theme colour, leaving levels 1 to 3 in the text colour" in {
    List(Theme.dark, Theme.light).foreach { theme =>
      withClue(s"${theme.name}: ") {
        val screen = render(prose(theme = theme))

        List("Alpha", "Bravo", "Charlie", "Plain body").foreach(text =>
          foregroundOf(screen, text) shouldBe rgb(theme.foreground)
        )
        foregroundOf(screen, "Delta") shouldBe rgb(theme.accent)
        foregroundOf(screen, "Echo") shouldBe headingColour(theme)
        foregroundOf(screen, "Foxtrot") shouldBe rgb(theme.muted)
        levelTexts.foreach(text => cellsOf(screen, text).map(cell => rgb(cell.bg)).distinct shouldBe List(rgb(theme.background)))
      }
    }
  }

  it should "keep the level colours distinct and readable against the background in light and dark themes" in {
    List(Theme.dark, Theme.light).foreach { theme =>
      val screen  = render(prose(theme = theme))
      val colours = List("Delta", "Echo", "Foxtrot").map(foregroundOf(screen, _))

      withClue(s"${theme.name}: ")(colours.distinct.size shouldBe 3)
      colours.foreach { colour =>
        withClue(s"${theme.name} #${Integer.toHexString(colour)}: ")(
          Theme.contrastRatio(RenderColor.fromRgba((colour >> 16) & 0xff, (colour >> 8) & 0xff, colour & 0xff), theme.background) should be >= 3.0
        )
      }
    }
  }

  it should "let each level's own settings win over the ladder" in {
    val headings = HeadingsConfig()
      .withLevel(1)(_.copy(underline = Some(false)))
      .withLevel(2)(_.copy(underline = Some(true), bold = Some(false)))
      .withLevel(3)(_.copy(italic = Some(false)))
      .withLevel(4)(_.copy(italic = Some(true), colour = Some(HeadingColour.Fixed(orange))))
    val screen = render(prose(withHeadings(headings)))

    lookOf(screen, "Alpha") shouldBe (true, false, false)
    lookOf(screen, "Bravo") shouldBe (false, false, true)
    lookOf(screen, "Charlie") shouldBe (true, false, false)
    lookOf(screen, "Delta") shouldBe (true, true, false)
    foregroundOf(screen, "Delta") shouldBe rgb(orange)
    lookOf(screen, "Echo") shouldBe (true, false, false)
  }

  it should "let the global heading colour win over the ladder's colours, and a level's own over both" in {
    val theme    = Theme.dark
    val headings = HeadingsConfig(colour = Some(HeadingColour.Heading)).withLevel(5)(_.copy(colour = Some(HeadingColour.Text)))
    val screen   = render(prose(withHeadings(headings), theme))

    foregroundOf(screen, "Alpha") shouldBe headingColour(theme)
    foregroundOf(screen, "Delta") shouldBe headingColour(theme)
    foregroundOf(screen, "Echo") shouldBe rgb(theme.foreground)
    foregroundOf(screen, "Foxtrot") shouldBe headingColour(theme)
  }

  it should "take colours by name from the config" in {
    val theme  = Theme.dark
    val config = com.serenity.config.ConfigRegistry
      .read(AppConfig.conventional, "headings.h1.colour", "accent")
      .flatMap(com.serenity.config.ConfigRegistry.read(_, "headings.h2.colour", "muted"))
      .getOrElse(fail("accent and muted were refused"))
    val screen = render(prose(config, theme))

    foregroundOf(screen, "Alpha") shouldBe rgb(theme.accent)
    foregroundOf(screen, "Bravo") shouldBe rgb(theme.muted)
  }

  it should "look as it always has when the session is a graphical one" in {
    val screen = render(prose(capabilities = FrontendCapabilities.gui))

    levelTexts.foreach(text => lookOf(screen, text) shouldBe (true, false, false))
    levelTexts.foreach(text => foregroundOf(screen, text) shouldBe rgb(Theme.dark.foreground))
  }

  it should "show a setting changed between frames that share their render caches" in {
    val caches = RenderCaches.create()
    val off    = withHeadings(HeadingsConfig().withLevel(1)(_.copy(underline = Some(false))))

    lookOf(render(prose(), caches), "Alpha")._3 shouldBe true
    lookOf(render(prose(off), caches), "Alpha")._3 shouldBe false
    lookOf(render(prose(), caches), "Alpha")._3 shouldBe true
  }

  "A Markdown heading in the terminal" should "honour the level's bold, italic and underline, climbing the same ladder" in {
    val screen = render(markdown())

    lookOf(screen, "Alpha") shouldBe (true, false, true)
    lookOf(screen, "Bravo") shouldBe (true, false, false)
    lookOf(screen, "Charlie") shouldBe (true, true, false)
    lookOf(screen, "Delta") shouldBe (true, false, false)
    lookOf(screen, "plain text") shouldBe plain
  }

  it should "stop being bold when its level's bold is off, and take italic and underline when they are on" in {
    val headings = HeadingsConfig()
      .withLevel(1)(_.copy(bold = Some(false), underline = Some(false)))
      .withLevel(2)(_.copy(italic = Some(true), underline = Some(true)))
      .withLevel(3)(_.copy(bold = Some(false), italic = Some(false)))
    val screen = render(markdown(withHeadings(headings)))

    lookOf(screen, "Alpha") shouldBe plain
    lookOf(screen, "Bravo") shouldBe (true, true, true)
    lookOf(screen, "Charlie") shouldBe plain
  }

  it should "leave the # marker bold in the marker colour, unstyled by the ladder" in {
    val theme  = Theme.dark
    val screen = render(markdown(theme = theme))
    val marker = screen.cellAt(0, 0)

    marker.text shouldBe "#"
    marker.style.isBold shouldBe true
    marker.style.isUnderlined shouldBe false
    marker.style.isItalic shouldBe false
    rgb(marker.fg) shouldBe rgb(theme.colorFor(SyntaxElement.HeadingMarker).foreground)
  }

  it should "tell levels 4 to 6 apart by colour, with levels 1 to 3 in the heading colour as before" in {
    List(Theme.dark, Theme.light).foreach { theme =>
      withClue(s"${theme.name}: ") {
        val screen = render(markdown(theme = theme))

        List("Alpha", "Bravo", "Charlie").foreach(text => foregroundOf(screen, text) shouldBe headingColour(theme))
        foregroundOf(screen, "Delta") shouldBe rgb(theme.accent)
        foregroundOf(screen, "Echo") shouldBe headingColour(theme)
        foregroundOf(screen, "Foxtrot") shouldBe rgb(theme.muted)
        foregroundOf(screen, "plain text") shouldBe rgb(theme.foreground)
      }
    }
  }

  it should "follow a level's own colour over the ladder" in {
    val headings = HeadingsConfig().withLevel(4)(_.copy(colour = Some(HeadingColour.Fixed(orange))))

    foregroundOf(render(markdown(withHeadings(headings))), "Delta") shouldBe rgb(orange)
  }

  it should "show a bold changed between frames that share their render caches" in {
    val caches = RenderCaches.create()
    val off    = withHeadings(HeadingsConfig().withLevel(2)(_.copy(bold = Some(false))))

    lookOf(render(markdown(), caches), "Bravo")._1 shouldBe true
    lookOf(render(markdown(off), caches), "Bravo")._1 shouldBe false
    lookOf(render(markdown(), caches), "Bravo")._1 shouldBe true
  }

  private val ruled = HeadingsConfig()
    .withLevel(1)(_.copy(rule = Some(true)))
    .withLevel(2)(_.copy(rule = Some(true)))

  private val ruledDocument = RichTextDocument(
    List(
      RichTextParagraph.plain("Opening line."),
      RichTextParagraph.plain("Headline", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("Under the headline."),
      RichTextParagraph.plain("Subhead", role = ParagraphRole.Heading(2)),
      RichTextParagraph.plain("Final line.")
    )
  )

  private val ruledTexts = List("Opening line.", "Headline", "Under the headline.", "Subhead", "Final line.")

  private def ruledState(
    headings: HeadingsConfig,
    cursor: CursorPosition = CursorPosition(0, 0),
    lineNumbers: Boolean = false
  ): AppState =
    prose(withHeadings(headings), document = ruledDocument, cursor = cursor, lineNumbers = lineNumbers)

  private def rowOf(screen: TerminalEmulator, text: String): Int =
    screen.find(text).map(_._2).getOrElse(fail(s"no '$text' on screen:\n${screen.render}"))

  private def gaps(screen: TerminalEmulator): List[Int] =
    val rows = ruledTexts.map(rowOf(screen, _))
    rows.zip(rows.tail).map((a, b) => b - a)

  private def isRule(screen: TerminalEmulator, row: Int, glyph: Char): Boolean =
    val text = screen.rowText(row).trim
    text.nonEmpty && text.forall(_ == glyph)

  "A rule under a heading" should "be off by default, leaving the heading's blank row empty" in {
    val spaced = HeadingsConfig()
      .withLevel(1)(_.copy(spaceAfter = Some(1.0f)))
      .withLevel(2)(_.copy(spaceAfter = Some(1.0f)))
    val screen = render(ruledState(spaced))

    gaps(screen) shouldBe List(1, 2, 1, 2)
    screen.rowText(rowOf(screen, "Headline") + 1).trim shouldBe empty
    screen.rowText(rowOf(screen, "Subhead") + 1).trim shouldBe empty
  }

  it should "take the row under the heading, drawn in a heavy line for level 1 and a light one for level 2" in {
    val screen = render(ruledState(ruled))

    gaps(screen) shouldBe List(1, 2, 1, 2)
    isRule(screen, rowOf(screen, "Headline") + 1, '━') shouldBe true
    isRule(screen, rowOf(screen, "Subhead") + 1, '─') shouldBe true
  }

  it should "span the width of the text area, in the heading's colour" in {
    val theme  = Theme.dark
    val screen = render(ruledState(ruled.withLevel(1)(_.copy(colour = Some(HeadingColour.Fixed(orange))))))
    val row    = rowOf(screen, "Headline") + 1
    val glyphs = (0 until viewport.width).filter(col => screen.cellAt(col, row).text == "━")

    glyphs.head shouldBe 0
    glyphs.size should be >= viewport.width - 2
    glyphs.foreach(col => rgb(screen.cellAt(col, row).fg) shouldBe rgb(orange))
    val subRow = rowOf(screen, "Subhead") + 1
    rgb(screen.cellAt(0, subRow).fg) shouldBe rgb(theme.foreground)
  }

  it should "not be drawn when the level's space_after is set to nothing" in {
    val screen = render(ruledState(ruled.withLevel(1)(_.copy(spaceAfter = Some(0.0f)))))

    gaps(screen) shouldBe List(1, 1, 1, 2)
    screen.rowText(rowOf(screen, "Headline") + 1) should include("Under the headline.")
    isRule(screen, rowOf(screen, "Subhead") + 1, '─') shouldBe true
  }

  it should "sit in the first of several blank rows" in {
    val screen = render(ruledState(ruled.withLevel(1)(_.copy(spaceAfter = Some(2.0f)))))

    gaps(screen) shouldBe List(1, 3, 1, 2)
    val row = rowOf(screen, "Headline")
    isRule(screen, row + 1, '━') shouldBe true
    screen.rowText(row + 2).trim shouldBe empty
  }

  it should "follow a heading that wraps onto several rows, below the last of them" in {
    val long = "A very long headline that must wrap onto a second row of the text area"
    val doc = RichTextDocument(
      List(RichTextParagraph.plain(long, role = ParagraphRole.Heading(1)), RichTextParagraph.plain("Body."))
    )
    val state  = prose(withHeadings(ruled).withWordWrap(true), document = doc)
    val screen = render(state)
    val last   = screen.rowsContaining("row of the text area").head
    val first  = screen.rowsContaining("A very long").head

    last should be > first
    isRule(screen, last + 1, '━') shouldBe true
    (first until last).foreach(row => isRule(screen, row, '━') shouldBe false)
    rowOf(screen, "Body.") shouldBe last + 2
  }

  it should "keep the caret on the heading and out of the rule's row" in {
    val atHeading = render(ruledState(ruled, CursorPosition(1, 3)), cursor = true)

    atHeading.cursor.row shouldBe rowOf(atHeading, "Headline")
  }

  it should "leave the line number column without a number on the rule's row" in {
    val screen = render(ruledState(ruled, lineNumbers = true))
    val row    = rowOf(screen, "Headline") + 1

    screen.rowText(row).exists(_.isDigit) shouldBe false
    screen.rowText(row).contains('━') shouldBe true
    screen.rowText(rowOf(screen, "Headline")).trim should startWith("2")
  }

  it should "come and go between frames that share their render caches" in {
    val caches = RenderCaches.create()
    val spaced = HeadingsConfig().withLevel(1)(_.copy(spaceAfter = Some(1.0f)))

    isRule(render(ruledState(ruled), caches), rowOf(render(ruledState(ruled), caches), "Headline") + 1, '━') shouldBe true
    val without = render(ruledState(spaced), caches)
    without.rowText(rowOf(without, "Headline") + 1).trim shouldBe empty
    val again = render(ruledState(ruled), caches)
    isRule(again, rowOf(again, "Headline") + 1, '━') shouldBe true
  }

  it should "not appear on a Markdown heading, which has no row of space in a terminal" in {
    val screen = render(markdown(withHeadings(ruled)))

    screen.rows.exists(_.contains('━')) shouldBe false
    screen.rows.exists(_.contains('─')) shouldBe false
  }
