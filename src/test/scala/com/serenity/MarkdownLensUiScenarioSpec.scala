package com.serenity

import java.nio.file.Paths

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.config.MarkdownViewMode
import com.serenity.keystroke.events.{MoveDown, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.PaneSplitDirection
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
          CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens)),
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
    paragraph.evidence.sourcePreviewMappings.values.flatten should contain(2)
    paragraph.evidence.previewPlacements.values.map(_.firstSourceLine) should contain(0)
    paragraph.evidence.visiblePreviewSourceLines.values.flatten should contain allOf (0, 15)
    paragraph.evidence.layoutViolations shouldBe empty

    driver.dispatch(ScrollDown(4)).unsafeRunSync()
    val scrolled = driver.renderFrame("scrolled").unsafeRunSync()

    scrolled.evidence.previewPlacements.values.map(_.firstPreviewRow) should contain(0)
    scrolled.evidence.visiblePreviewSourceLines.values.flatten should contain allOf (0, 15)
  }

  it should "traverse list, table, and code blocks without collapsing the preview" in {
    val driver = markdownDriver("markdown-structures")
    val expectedBlocks = List(
      4  -> Set(4),
      7  -> Set(7, 8, 9),
      12 -> Set(11, 12, 13)
    )

    expectedBlocks.foreach {
      case (line, expectedLines) =>
        driver.stateManager.setCursorPosition(PaneId(0), line, 1).unsafeRunSync()
        val evidence = driver.renderFrame(s"line-$line").unsafeRunSync().evidence
        val bufferId = driver.state.unsafeRunSync().focusedBufferId.getOrElse(fail("Expected Markdown buffer"))
        evidence.sourcePreviewMappings(bufferId) should contain allElementsOf expectedLines
        evidence.previewPlacements should contain key bufferId
        evidence.renderedContentRows should not be empty
    }
  }

  it should "associate preview placement with each Markdown pane and preserve multiple cursors" in {
    val driver = markdownDriver("markdown-multi-pane")
    driver
      .updateState { state =>
        val original =
          state.focusedBufferId.flatMap(state.persisted.buffers.get).getOrElse(fail("Expected Markdown buffer"))
        val firstId = BufferId(50)
        val firstBuffer = original.copy(
          id = firstId,
          editing = original.editing.copy(cursors = List(CursorPosition(0, 0), CursorPosition(4, 0)))
        )
        val secondId = BufferId(99)
        val secondBuffer = firstBuffer.copy(
          id = secondId,
          document = firstBuffer.document.copy(content = com.serenity.rope.Rope("# Other\n\nSecond pane paragraph.\n")),
          editing = firstBuffer.editing.copy(cursors = List(CursorPosition(2, 0)))
        )
        state.copy(
          persisted = state.persisted.copy(
            buffers = Map(firstId -> firstBuffer, secondId -> secondBuffer),
            bufferOrder = List(firstId, secondId),
            layout = state.persisted.layout.copy(
              editorPanes = Map(
                PaneId(0) -> EditorPane.withBuffer(PaneId(0), firstId),
                PaneId(1) -> EditorPane.withBuffer(PaneId(1), secondId)
              ),
              activeEditorPaneId = Some(PaneId(1)),
              paneOrder = List(PaneId(0), PaneId(1)),
              splitDirection = PaneSplitDirection.Vertical
            ),
            focus = Focus.EditorPane(PaneId(1))
          ),
          runtime = state.runtime.copy(
            nextBufferId = BufferId(100),
            nextPaneId = PaneId(2)
          )
        )
      }
      .unsafeRunSync()
    val evidence = driver.renderFrame("multi-pane").unsafeRunSync().evidence
    evidence.previewPlacements should have size 2
    evidence.previewPlacements.values.map(_.bounds).toSet should have size 2
    evidence.sourcePreviewMappings.values.exists(_.contains(0)) shouldBe true
    evidence.sourcePreviewMappings.values.exists(_.contains(2)) shouldBe true
    driver.state.unsafeRunSync().persisted.buffers(BufferId(50)).editing.cursors should have size 2
  }

  private def markdownDriver(name: String): UiScenarioDriver =
    val driver  = UiScenarioDriver.create(name).unsafeRunSync()
    val fixture = Paths.get(getClass.getResource("/ui-scenarios/markdown-lens.md").toURI)
    driver.stateManager.openFile(fixture).unsafeRunSync()
    driver.stateManager
      .executeCommand(
        Command.typed(
          "inline-lens",
          "Inline Lens",
          CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    driver
