package com.serenity.scenario

import scala.io.Source

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{MarkdownViewMode, MotionPreset}
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.{CursorPosition, Focus, PaneId, SurfaceContent}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiScenarioDriverSpec extends AnyFlatSpec with Matchers:

  "UiScenarioDriver" should "render deterministic frame evidence after real input events" in {
    val driver = UiScenarioDriver.create().unsafeRunSync()
    val source = Source.fromResource("ui-scenarios/markdown-lens.md")
    val fixture =
      try source.mkString
      finally source.close()
    val bufferId = driver.createBuffer(fixture).unsafeRunSync()
    driver.configureBuffer(bufferId, Some(LanguageId.Markdown), MarkdownViewMode.InlineLens).unsafeRunSync()
    driver.setCursor(PaneId(0), CursorPosition(0, 0)).unsafeRunSync()
    driver.dispatch(MoveDown).unsafeRunSync()

    val frame = driver.render().unsafeRunSync()

    frame.activeFocus shouldBe Focus.EditorPane(PaneId(0))
    frame.markdownRows.find(_.sourceLine == 2).map(_.text) should contain("Paragraph after heading.")
    val firstContentPixelRow = (0 until frame.image.getHeight)
      .find(row => (0 until frame.image.getWidth).exists(column => frame.image.getRGB(column, row) != 0))
      .getOrElse(fail("expected rendered Markdown pixels"))
    firstContentPixelRow should be < driver.cellMetrics.lineHeight
    frame.image.getWidth shouldBe 960
    frame.image.getHeight shouldBe 576
    frame.diagnostics shouldBe empty
  }

  it should "preserve Markdown lens source-to-preview geometry through blocks and scrolling" in {
    val driver = UiScenarioDriver.create(viewport = ViewportSize(80, 12)).unsafeRunSync()
    val source = Source.fromResource("ui-scenarios/markdown-lens.md")
    val fixture =
      try source.mkString
      finally source.close()
    val bufferId = driver.createBuffer(fixture).unsafeRunSync()
    driver.configureBuffer(bufferId, Some(LanguageId.Markdown), MarkdownViewMode.InlineLens).unsafeRunSync()
    List(0, 2, 4, 7, 11).foreach { line =>
      driver.setCursor(PaneId(0), CursorPosition(line, 0)).unsafeRunSync()
      val frame    = driver.render().unsafeRunSync()
      val rendered = frame.markdownRows.find(_.sourceLine == line).getOrElse(fail(s"rendered source line $line"))
      rendered.screenRow should (be >= 0 and be < 12)
      frame.image.getWidth shouldBe driver.cellMetrics.charWidth * 80
    }
    driver.dispatch(ResizeEvent(ViewportSize(80, 8))).unsafeRunSync()
    driver.setCursor(PaneId(0), CursorPosition(11, 0)).unsafeRunSync()
    val scrolled = driver.render().unsafeRunSync().markdownRows.find(_.sourceLine == 11).getOrElse(fail("code row"))
    scrolled.screenRow should (be >= 0 and be < 8)
  }

  it should "expose matching command item geometry for rendered rows and mouse events" in {
    val driver = UiScenarioDriver.create(viewport = ViewportSize(100, 30)).unsafeRunSync()

    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.dispatch(RunnerInsertChar('t')).unsafeRunSync()
    val frame = driver.render().unsafeRunSync()
    val item  = frame.itemRects.values.flatten.find(_.label.toLowerCase.contains("theme")).getOrElse(fail("theme item"))
    item.renderedRect shouldBe item.hitRect
    val beforeRunner = driver.snapshot.unsafeRunSync().commandRunnerSurface.map(_.content)

    driver.click(item.hitRect.x, item.hitRect.y).unsafeRunSync()
    val afterClick = driver.snapshot.unsafeRunSync()
    afterClick.commandRunnerSurface.map(_.content) should not be beforeRunner
    driver.render().unsafeRunSync().surfaceRects.values.exists(_.contains(item.hitRect.x, item.hitRect.y)) shouldBe true
  }

  it should "settle animation ticks without wall-clock timing and retain diagnostics on request" in {
    val driver = UiScenarioDriver.create().unsafeRunSync()

    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    val settled = driver.advanceUntilSettled().unsafeRunSync()
    val frame   = driver.render(writeDiagnostic = true).unsafeRunSync()

    settled shouldBe true
    frame.settled shouldBe true
    frame.diagnosticPng shouldBe defined
  }

  it should "keep contextual toolbar geometry compact while returning editor focus after toolbar interaction" in {
    val driver   = UiScenarioDriver.create(viewport = ViewportSize(18, 20)).unsafeRunSync()
    val bufferId = driver.createBuffer("# Toolbar").unsafeRunSync()
    driver.configureBuffer(bufferId, Some(LanguageId.Markdown), MarkdownViewMode.Source).unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()

    val state   = driver.snapshot.unsafeRunSync()
    val toolbar = state.contextualToolbarSurface.getOrElse(fail("toolbar surface"))
    state.focus shouldBe Focus.EditorPane(PaneId(0))
    val frame = driver.render().unsafeRunSync()
    frame.surfaceRects(toolbar.id).width should be < 18
    val toolbarRows = frame.itemRects(toolbar.id)
    toolbarRows.map(_.renderedRect.y).distinct.size should be > 1
    toolbarRows.foreach(item => item.renderedRect shouldBe item.hitRect)
    val previewRow = toolbarRows.find(_.label == "markdown-preview").getOrElse(fail("preview button"))
    driver.click(previewRow.hitRect.x, previewRow.hitRect.y).unsafeRunSync()
    val clickedState = driver.snapshot.unsafeRunSync()
    clickedState.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.MarkdownPreview]) shouldBe true
    clickedState.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "settle interrupted motion and honour reduced motion without wall-clock time" in {
    val driver = UiScenarioDriver.create().unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.advanceUntilSettled(1).unsafeRunSync() shouldBe false
    val openingRect = driver.render().unsafeRunSync().surfaceRects.values.head
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.advanceUntilSettled().unsafeRunSync() shouldBe true
    driver.render().unsafeRunSync().surfaceRects shouldBe empty

    driver
      .updateState(state => state.copy(config = state.config.withMotionPreset(MotionPreset.Reduced)))
      .unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.advanceUntilSettled().unsafeRunSync() shouldBe true
    val settledFrame = driver.render().unsafeRunSync()
    settledFrame.settled shouldBe true
    settledFrame.surfaceRects.values.head shouldBe openingRect
  }

  it should "close and reopen the command runner after nested settings navigation" in {
    val driver = UiScenarioDriver.create().unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.dispatch(RunnerInsertChar('s')).unsafeRunSync()
    driver.dispatch(RunnerNavigate(Direction.Down)).unsafeRunSync()
    driver.dispatch(RunnerSubmit).unsafeRunSync()
    driver.dispatch(RunnerDismiss).unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.render().unsafeRunSync().itemRects should not be empty
  }

  it should "show preset save and apply outcomes while recovering from an unavailable preset" in {
    val driver = UiScenarioDriver.create().unsafeRunSync()
    val save = Command.typed(
      "scenario-save",
      "Save scenario preset",
      CommandIntent.SaveUiPreset("Scenario"),
      CommandCategory.Settings
    )
    val applySaved = Command.typed(
      "scenario-apply",
      "Apply scenario preset",
      CommandIntent.ApplyUiPreset("Scenario"),
      CommandCategory.Settings
    )
    val applyMissing = Command.typed(
      "scenario-missing",
      "Apply missing preset",
      CommandIntent.ApplyUiPreset("Missing"),
      CommandCategory.Settings
    )

    driver.execute(save).unsafeRunSync()
    val savedConfig = driver.snapshot.unsafeRunSync().config
    driver
      .updateState(state => state.copy(config = state.config.withLineNumbers(!savedConfig.showLineNumbers)))
      .unsafeRunSync()
    driver.execute(applySaved).unsafeRunSync()
    driver.snapshot.unsafeRunSync().config.showLineNumbers shouldBe savedConfig.showLineNumbers
    driver.execute(applyMissing).unsafeRunSync()
    driver.snapshot.unsafeRunSync().config.showLineNumbers shouldBe savedConfig.showLineNumbers
  }
