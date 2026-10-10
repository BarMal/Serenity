package com.serenity.markdown

import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownBlockLens.FenceRangeIndex
import com.serenity.markdown.MarkdownInlineSpans.Run

/** Whether, and where, a buffer's Markdown emphasis and code markers are hidden. */
enum MarkerMode:
  /** Markers are ordinary text: source editing, or any buffer a live preview does not apply to. */
  case Off

  /** Markers are hidden except on the lines the caret or a selection touches. */
  case Live

  /** Markers are hidden on every line. */
  case Read

object MarkerMode:

  /** A live preview needs per-glyph measurement and a pixel surface, so a Markdown buffer on a cell grid keeps its
    * markers whatever the view mode says.
    */
  def of(viewMode: MarkdownViewMode, language: Option[LanguageId], cellGrid: Boolean): MarkerMode =
    if cellGrid || !language.contains(LanguageId.Markdown) then Off
    else
      viewMode match
        case MarkdownViewMode.LivePreview => Live
        case MarkdownViewMode.Read        => Read
        case _                            => Off

/** Everything layout and painting need to restyle one buffer's inline Markdown: how markers are hidden, the lines that
  * stay revealed, the fenced blocks whose text is code rather than prose, and whether the buffer's own font is already
  * monospaced, which decides if a code span needs a face of its own. It is a plain value, so two views that would lay a
  * line out alike compare equal and a cached layout keyed on one is reused.
  */
final case class MarkdownInlineView(
    mode: MarkerMode,
    revealed: Vector[Range.Inclusive],
    fences: FenceRangeIndex,
    baseIsMonospaced: Boolean = false
):

  def isActive: Boolean = mode != MarkerMode.Off

  /** The decorated stretches of `text`, the content of buffer line `line`. Code inside a fenced block is not prose. */
  def runsOn(line: Int, text: String): Vector[Run] =
    if !isActive || !MarkdownInlineSpans.mayContainMarkup(text) || fences.rangeAt(line).isDefined then Vector.empty
    else MarkdownInlineSpans.scan(text)

  def hidesMarkersOn(line: Int): Boolean =
    mode match
      case MarkerMode.Off  => false
      case MarkerMode.Read => true
      case MarkerMode.Live => !revealed.exists(range => range.start <= line && line <= range.end)

object MarkdownInlineView:

  val Off: MarkdownInlineView = MarkdownInlineView(MarkerMode.Off, Vector.empty, FenceRangeIndex(Vector.empty))
