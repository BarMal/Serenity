package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.config.MarkdownViewMode
import com.serenity.markdown.{MarkdownBlockSpans, MarkdownInlineView, MarkerMode}
import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.state.models.{Buffer, Cursor, IndentationSettings}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.openings.{ChapterOpenings, OpeningContext, OpeningSettings, ParagraphRoles, VirtualDropCaps}
import com.serenity.ui.theme.{HeadingSettings, RichTextStyling}

/** The inputs a logical line's wrap depends on besides its text: the rich-text paragraph styles that choose each run's
  * font and a drop cap's role, the prose zoom that scales them, whether drop caps are enabled at all, the chapter
  * openings that are set as drop caps, and how the line's inline Markdown is restyled. `tabSize` is the width a tab
  * takes on the cell grid, and `headings` the look its heading paragraphs resolve to.
  */
final case class RichTextContext(
    document: Option[RichTextDocument],
    proseScale: Float,
    dropCapsEnabled: Boolean,
    markdown: MarkdownInlineView = MarkdownInlineView.Off,
    tabSize: Int = IndentationSettings.DefaultTabSize,
    headings: HeadingSettings = HeadingSettings.default,
    virtualDropCaps: Option[VirtualDropCaps] = None,
    resolver: ChapterOpenings.Resolver = ChapterOpenings.standard
):

  val roles: ParagraphRoles = ParagraphRoles(OpeningContext(dropCapsEnabled, virtualDropCaps), resolver)

  /** The role `line`'s paragraph lays out with; body text where there is no rich document. */
  def layoutRole(line: Int): ParagraphRole =
    document.fold(ParagraphRole.Body)(roles.layoutRole(_, line))

  /** Hiding a marker means measuring glyph by glyph, which a font laid out on the cell grid is not. */
  def requiresMeasuredLayout: Boolean = markdown.isActive

object RichTextContext:

  /** A buffer with no rich-text styling: every line wraps in the one base font. */
  val plain: RichTextContext = RichTextContext(None, 1.0f, dropCapsEnabled = true)

  /** The context the renderer paints `buffer` with at `font`, so row counts can follow the painted wrap. A document
    * that no longer has the buffer's text shape is stale and ignored, exactly as the painted layout ignores it.
    */
  def forBuffer(
    buffer: Buffer,
    font: Font,
    dropCapsEnabled: Boolean,
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    cellGrid: Boolean = false,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    frc: FontRenderContext = TextLayoutSnapshot.defaultFontRenderContext(),
    tabSize: Int = IndentationSettings.DefaultTabSize,
    headings: HeadingSettings = HeadingSettings.default,
    openings: OpeningSettings = OpeningSettings.default
  ): RichTextContext =
    val content  = buffer.document.content
    val document = buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(content.lineCount, content.weight))
    RichTextContext(
      document,
      RichTextStyling.proseZoom(font.getSize2D),
      dropCapsEnabled,
      markdownViewFor(buffer, markdownViewMode, cellGrid, wrapCache, font, frc),
      tabSize,
      headings,
      document.flatMap(wrapCache.virtualDropCaps(_, openings))
    )

  /** How `buffer`'s inline Markdown is restyled under `viewMode`. A buffer carrying a rich-text document styles its own
    * runs, so it is never restyled twice.
    */
  def markdownViewFor(
    buffer: Buffer,
    viewMode: MarkdownViewMode,
    cellGrid: Boolean,
    wrapCache: WrappedLineCache,
    font: Font,
    frc: FontRenderContext = TextLayoutSnapshot.defaultFontRenderContext()
  ): MarkdownInlineView =
    val mode = MarkerMode.of(viewMode, buffer.document.language, cellGrid)
    if mode == MarkerMode.Off || buffer.richText.richTextDocument.isDefined then MarkdownInlineView.Off
    else
      val content = buffer.document.content
      val fences = wrapCache.fenceIndex(
        buffer.id,
        content,
        Some(ContentRevision(buffer.document.contentVersion, buffer.document.changesSince))
      )
      val mono = FontLoader.isMonospacedFont(font)
      val columns =
        wrapCache.tableColumns(buffer.id, content, font, frc)(MarkdownTableMetrics.columnsFor(content, font, frc, mono))
      MarkdownInlineView(
        mode,
        if mode == MarkerMode.Live then revealedLines(buffer) else Vector.empty,
        fences,
        mono,
        columns
      )

  /** The lines the caret or a selection touches. A caret on a line with nothing to reveal changes no layout, so it is
    * left out and moving between such lines leaves the view, and everything cached under it, as it was.
    */
  private[serenity] def revealedLines(buffer: Buffer): Vector[Range.Inclusive] =
    buffer.editing.cursors.toList.toVector.map(touchedLines).filter { lines =>
      lines.start != lines.end || buffer.document.content
        .getLine(lines.start)
        .exists(MarkdownBlockSpans.mayContainMarkup)
    }

  private def touchedLines(cursor: Cursor): Range.Inclusive =
    val anchorLine = cursor.selectionAnchor.getOrElse(cursor.position).line
    math.min(cursor.position.line, anchorLine) to math.max(cursor.position.line, anchorLine)
