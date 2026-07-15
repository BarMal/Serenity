package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownLensUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Markdown Lens UI scenario" should "retain the heading preview when the caret moves into its paragraph" in {
    val source = scala.io.Source.fromResource("ui-scenarios/markdown-lens.md").mkString
    val driver = UiScenarioDriver.create("markdown-lens").unsafeRunSync()
    driver.updateState { state =>
      val id = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
      state.copy(
        buffers = state.buffers.updated(id, state.buffers(id).copy(content = Rope(source), language = Some(LanguageId.Markdown))),
        config = state.config.withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    }.unsafeRunSync()
    driver.stateManager.setCursorPosition(PaneId(0), 0, 0).unsafeRunSync()
    val heading = driver.renderFrame("heading").unsafeRunSync()
    driver.stateManager.setCursorPosition(PaneId(0), 2, 0).unsafeRunSync()
    val paragraph = driver.renderFrame("paragraph").unsafeRunSync()

    heading.evidence.sourcePreviewMappings.values.flatten.toSet should contain(0)
    paragraph.evidence.sourcePreviewMappings.values.flatten.toSet should contain(2)
    paragraph.evidence.visibleText should contain("Paragraph after the heading.")
    paragraph.evidence.layoutViolations shouldBe empty
  }
