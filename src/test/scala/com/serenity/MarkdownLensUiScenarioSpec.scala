package com.serenity

import java.nio.file.Paths

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.MarkdownViewMode
import com.serenity.keystroke.events.MoveDown
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownLensUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Markdown Lens UI scenario" should "retain rendered content when input moves from heading into its paragraph" in {
    val driver  = UiScenarioDriver.create("markdown-lens").unsafeRunSync()
    val fixture = Paths.get(getClass.getResource("/ui-scenarios/markdown-lens.md").toURI)
    driver.stateManager.openFile(fixture).unsafeRunSync()
    driver.stateManager
      .executeCommand(
        Command.typed(
          "inline-lens",
          "Inline Lens",
          CommandIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    val heading = driver.renderFrame("heading").unsafeRunSync()
    driver.dispatch(MoveDown).unsafeRunSync()
    driver.dispatch(MoveDown).unsafeRunSync()
    val paragraph = driver.renderFrame("paragraph").unsafeRunSync()

    heading.evidence.renderedContentRows should not be empty
    paragraph.evidence.renderedContentRows should not be empty
    paragraph.evidence.renderedContentRows.min should be < driver.environment.cellMetrics.lineHeight
    paragraph.evidence.layoutViolations shouldBe empty
  }
