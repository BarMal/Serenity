package com.serenity.ui.renderer

import com.serenity.state.models.Viewport
import com.serenity.ui.layout.TextLayoutSnapshot

/** Where the split Markdown preview rests so it follows the editor's top line without jumping (#1862). Both panes show
  * the same lines but wrap them at their own widths, so the editor's row inside a wrapped line is carried over as the
  * fraction of the line already scrolled past.
  */
object MarkdownPreviewScroll:

  def previewViewport(editor: Viewport): Viewport =
    editor.copy(topVisualLine = 0, leftColumn = 0)

  /** Rows the snapshot draws for `line` from its first row on. */
  def leadingRows(snapshot: TextLayoutSnapshot, line: Int): Int =
    snapshot.visualLines.takeWhile(_.bufferLine == line).length

  /** The rows the editor wraps its top line into: those scrolled past plus those it still draws. */
  def editorRows(editor: Viewport, snapshot: TextLayoutSnapshot): Int =
    editor.topVisualLine + leadingRows(snapshot, editor.topLine)

  /** The preview's row inside the same source line: the editor's `editorRow` of `editorRows` as the same fraction of
    * the `previewRows` the preview wraps the line into.
    */
  def previewRow(editorRow: Int, editorRows: Int, previewRows: Int): Int =
    if editorRows <= 1 || previewRows <= 1 then 0
    else (editorRow.toLong * previewRows / editorRows).toInt.min(previewRows - 1).max(0)
