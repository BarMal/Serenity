package com.serenity.ui.renderer

import com.serenity.state.models.Viewport
import com.serenity.ui.layout.DropCapSnapshots
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The split Markdown preview follows the editor's scroll position smoothly (#1862). */
class MarkdownPreviewScrollSpec extends AnyFlatSpec with Matchers:

  private def editor(topLine: Int, topVisualLine: Int = 0, leftColumn: Int = 0): Viewport =
    Viewport(
      topLine = topLine,
      leftColumn = leftColumn,
      visibleLines = 20,
      visibleColumns = 80,
      topVisualLine = topVisualLine
    )

  "The preview viewport" should "start at the source line the editor has scrolled to" in {
    MarkdownPreviewScroll.previewViewport(editor(topLine = 42)).topLine shouldBe 42
  }

  it should "not inherit the editor's row or horizontal scroll, which the preview sets from its own wrapping" in {
    val preview = MarkdownPreviewScroll.previewViewport(editor(topLine = 7, topVisualLine = 3, leftColumn = 30))

    (preview.topVisualLine, preview.leftColumn) shouldBe (0, 0)
  }

  "The editor's wrapped rows for its top line" should "count the rows already scrolled past plus those still drawn" in {
    val snapshot = DropCapSnapshots.snapshot(
      DropCapSnapshots.Paragraph(com.serenity.richtext.ParagraphRole.Body, rows = 2, firstRow = 3)
    )

    MarkdownPreviewScroll.editorRows(editor(topLine = 0, topVisualLine = 3), snapshot) shouldBe 5
  }

  "The preview's matching row" should "be the same fraction of the line when both panes wrap it alike (1x)" in {
    MarkdownPreviewScroll.previewRow(editorRow = 2, editorRows = 4, previewRows = 4) shouldBe 2
  }

  it should "double when the preview wraps the line into twice the rows (2x)" in {
    MarkdownPreviewScroll.previewRow(editorRow = 2, editorRows = 4, previewRows = 8) shouldBe 4
  }

  it should "halve when the preview wraps the line into half the rows" in {
    MarkdownPreviewScroll.previewRow(editorRow = 2, editorRows = 4, previewRows = 2) shouldBe 1
  }

  it should "stay on the line's last row when the editor is on its last row" in {
    MarkdownPreviewScroll.previewRow(editorRow = 3, editorRows = 4, previewRows = 9) shouldBe 6
  }

  it should "be the first row when the editor is at the start of the line or the line does not wrap" in {
    (
      MarkdownPreviewScroll.previewRow(0, 4, 8),
      MarkdownPreviewScroll.previewRow(0, 1, 5),
      MarkdownPreviewScroll.previewRow(0, 4, 1)
    ) shouldBe (0, 0, 0)
  }
