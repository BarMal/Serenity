package com.serenity.ui.renderer

import com.serenity.state.models.Viewport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The split Markdown preview follows the editor's scroll position (#1862). */
class MarkdownPreviewScrollSpec extends AnyFlatSpec with Matchers:

  "The preview viewport" should "start at the source line the editor has scrolled to" in {
    val editor = Viewport(topLine = 42, topVisualLine = 0, visibleLines = 20, visibleColumns = 80)

    RendererFloatingPanels.markdownPreviewViewport(editor).topLine shouldBe 42
  }

  it should "snap to the start of that source line when the editor is part-way down a wrapped line" in {
    val editor = Viewport(topLine = 7, topVisualLine = 3, visibleLines = 20, visibleColumns = 80)

    val preview = RendererFloatingPanels.markdownPreviewViewport(editor)

    (preview.topLine, preview.topVisualLine) shouldBe (7, 0)
  }

  it should "not inherit the editor's horizontal scroll" in {
    val editor = Viewport(topLine = 5, leftColumn = 30, visibleLines = 20, visibleColumns = 80)

    RendererFloatingPanels.markdownPreviewViewport(editor).leftColumn shouldBe 0
  }
