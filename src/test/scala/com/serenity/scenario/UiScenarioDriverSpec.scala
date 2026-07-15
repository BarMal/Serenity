package com.serenity.scenario

import java.nio.file.Files

import scala.io.Source

import cats.effect.unsafe.implicits.global
import com.serenity.animation.TransitionKind
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{MarkdownViewMode, MotionPreset}
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiScenarioDriverSpec extends AnyFlatSpec with Matchers:
  private given Balance = Balance.default

  "UiScenarioDriver" should "keep heading and paragraph lens frames distinct through caret movement and wrapping" in {
    val driver = UiScenarioDriver.create(viewport = ViewportSize(24, 10)).unsafeRunSync()
    val source = Source.fromResource("ui-scenarios/markdown-lens.md")
    val fixture =
      try source.mkString
      finally source.close()
    val bufferId = driver.createBuffer(fixture).unsafeRunSync()
    driver.configureBuffer(bufferId, Some(LanguageId.Markdown), MarkdownViewMode.InlineLens).unsafeRunSync()
    driver.setCursor(PaneId(0), CursorPosition(0, 0)).unsafeRunSync()
    val headingFrame = driver.render().unsafeRunSync()
    driver.dispatch(MoveDown).unsafeRunSync()
    driver.dispatch(MoveDown).unsafeRunSync()
    val paragraphFrame = driver.render().unsafeRunSync()
    driver.dispatch(MoveDown).unsafeRunSync()
    driver.dispatch(MoveDown).unsafeRunSync()
    val wrappedFrame = driver.render().unsafeRunSync()

    val headingLens   = headingFrame.markdownLenses.find(_.sourceRange == (0 to 0)).getOrElse(fail("heading lens"))
    val paragraphLens = paragraphFrame.markdownLenses.find(_.sourceRange == (2 to 2)).getOrElse(fail("paragraph lens"))
    val wrappedLens   = wrappedFrame.markdownLenses.find(_.sourceRange.contains(4)).getOrElse(fail("wrapped lens"))

    headingFrame.activeFocus shouldBe Focus.EditorPane(PaneId(0))
    headingLens.rect.y should not be paragraphLens.rect.y
    paragraphFrame.markdownRows.find(_.sourceLine == 2).map(_.text) should contain("Paragraph after heading.")
    wrappedLens.rect.height should be > 1
    wrappedLens.rect.bottom should be <= 10
    headingFrame.diagnostics shouldBe empty
    paragraphFrame.diagnostics shouldBe empty
    wrappedFrame.diagnostics shouldBe empty
  }

  it should "preserve Markdown lens source-to-preview geometry through blocks and scrolling" in {
    val driver = UiScenarioDriver.create(viewport = ViewportSize(80, 12)).unsafeRunSync()
    val source = Source.fromResource("ui-scenarios/markdown-lens.md")
    val fixture =
      try source.mkString
      finally source.close()
    val bufferId = driver.createBuffer(fixture).unsafeRunSync()
    driver.configureBuffer(bufferId, Some(LanguageId.Markdown), MarkdownViewMode.InlineLens).unsafeRunSync()
    List(0, 2, 4, 8, 12, 19, 23).foreach { line =>
      driver.setCursor(PaneId(0), CursorPosition(line, 0)).unsafeRunSync()
      val frame = driver.render().unsafeRunSync()
      val lens  = frame.markdownLenses.find(_.sourceRange.contains(line)).getOrElse(fail(s"rendered lens line $line"))
      lens.rect.y should (be >= 0 and be < 12)
      frame.image.getWidth shouldBe driver.cellMetrics.charWidth * 80
    }
    driver.dispatch(ResizeEvent(ViewportSize(80, 8))).unsafeRunSync()
    driver.setCursor(PaneId(0), CursorPosition(23, 0)).unsafeRunSync()
    val scrolled =
      driver.render().unsafeRunSync().markdownLenses.find(_.sourceRange.contains(23)).getOrElse(fail("scrolled lens"))
    scrolled.rect.y should (be >= 0 and be < 8)
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

  it should "exercise deterministic light and dark frames at device scale two" in {
    val driver = UiScenarioDriver.create(deviceScale = 2.0).unsafeRunSync()
    driver.createBuffer("scale evidence").unsafeRunSync()
    val dark = driver.render().unsafeRunSync()
    driver.updateState(_.copy(theme = Theme.light)).unsafeRunSync()
    val light = driver.render().unsafeRunSync()

    dark.image.getWidth shouldBe 1920
    dark.image.getHeight shouldBe 1152
    light.image.getRGB(0, 0) should not be dark.image.getRGB(0, 0)
    dark.diagnostics shouldBe empty
    light.diagnostics shouldBe empty
  }

  it should "restore a saved preset after draft discard, restart, and a missing-preset failure" in {
    val root    = Files.createTempDirectory("ui-scenario-preset-restart")
    val driver  = UiScenarioDriver.create(sessionRootOverride = Some(root)).unsafeRunSync()
    val save    = scenarioCommand("save-preview", CommandIntent.SaveUiPreset("Restartable"))
    val apply   = scenarioCommand("apply-preview", CommandIntent.ApplyUiPreset("Restartable"))
    val discard = scenarioCommand("discard-draft", CommandIntent.DiscardUiPresetDraft("Restartable"))
    val missing = scenarioCommand("missing-preview", CommandIntent.ApplyUiPreset("Unavailable"))

    driver.execute(save).unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.snapshot.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner.uiPresetPreviews.map(_.name))
        case _                                     => None
    } should contain(List("Restartable"))
    val savedLineNumbers = driver.snapshot.unsafeRunSync().config.showLineNumbers
    driver.execute(scenarioCommand("draft-change", CommandIntent.ToggleLineNumbers)).unsafeRunSync()
    driver.snapshot.unsafeRunSync().config.showLineNumbers should not be savedLineNumbers
    driver.execute(discard).unsafeRunSync()
    driver.snapshot.unsafeRunSync().config.showLineNumbers shouldBe savedLineNumbers
    driver.execute(missing).unsafeRunSync()
    driver.snapshot.unsafeRunSync().config.showLineNumbers shouldBe savedLineNumbers

    val restarted = UiScenarioDriver.create(sessionRootOverride = Some(root)).unsafeRunSync()
    restarted.execute(apply).unsafeRunSync()
    restarted.execute(scenarioCommand("restart-draft-change", CommandIntent.ToggleLineNumbers)).unsafeRunSync()
    restarted.snapshot.unsafeRunSync().config.showLineNumbers should not be savedLineNumbers
    restarted.execute(discard).unsafeRunSync()
    restarted.snapshot.unsafeRunSync().config.showLineNumbers shouldBe savedLineNumbers
  }

  it should "drive toolbar dropdown and decimal input controls before returning focus to the editor" in {
    val driver   = UiScenarioDriver.create(viewport = ViewportSize(32, 24)).unsafeRunSync()
    val bufferId = driver.createBuffer("alpha beta").unsafeRunSync()
    driver
      .updateState { state =>
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val range     = RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 10))
        val document = RichTextDocument
          .fromPlainText("alpha beta")
          .setFontSize(range, 18.0f)
          .setParagraphRole(range, ParagraphRole.Body)
        state.copy(buffers =
          state.buffers.updated(
            bufferId,
            state
              .buffers(bufferId)
              .copy(
                content = Rope("alpha beta"),
                selection = Some(selection),
                cursors = List(selection.focus),
                richTextDocument = Some(document)
              )
          )
        )
      }
      .unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    focusToolbarItem(driver, "paragraph-role")
    driver.dispatch(Enter).unsafeRunSync()
    driver.dispatch(MoveRight).unsafeRunSync()
    driver.dispatch(Enter).unsafeRunSync()
    driver.snapshot.unsafeRunSync().focus shouldBe Focus.EditorPane(PaneId(0))

    focusToolbarItem(driver, "font-size")
    driver.dispatch(Enter).unsafeRunSync()
    (0 until 2).foreach(_ => driver.dispatch(DeleteBackward).unsafeRunSync())
    "20".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()
    val richText = driver.snapshot.unsafeRunSync().buffers(bufferId).richTextDocument.getOrElse(fail("rich text"))
    richText.paragraphs.headOption.flatMap(_.runs.find(_.text == "beta")).flatMap(_.style.fontSize) shouldBe Some(20.0f)
    driver.snapshot.unsafeRunSync().focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "retain nested settings spacing and motion-family geometry after settle" in {
    val driver = UiScenarioDriver.create().unsafeRunSync()
    driver.execute(scenarioCommand("spacing", CommandIntent.SetCommandRunnerItemGapRows(1))).unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    val spaced = driver.render().unsafeRunSync()
    val rows   = spaced.itemRects.values.flatten.toList.map(_.renderedRect.y).sorted
    rows.sliding(2).foreach {
      case List(first, second) => second - first should be >= 2
      case _                   => ()
    }
    driver.execute(scenarioCommand("motion-family", CommandIntent.SetMotionPreset(MotionPreset.Smooth))).unsafeRunSync()
    driver
      .execute(scenarioCommand("motion-override", CommandIntent.SetCommandRunnerTransitionKind(TransitionKind.Fade)))
      .unsafeRunSync()
    driver.snapshot.unsafeRunSync().config.motionPreset shouldBe MotionPreset.Custom
    driver.advanceUntilSettled().unsafeRunSync() shouldBe true
    val settled = driver.render().unsafeRunSync()
    val rect    = settled.surfaceRects.values.headOption.getOrElse(fail("settled command runner"))
    settled.image.getRGB(rect.x * driver.cellMetrics.charWidth, rect.y * driver.cellMetrics.lineHeight) should not be 0
    settled.diagnostics shouldBe empty
  }

  it should "edit command spacing through the nested settings runner" in {
    val driver = UiScenarioDriver.create().unsafeRunSync()
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    enterRunnerCategory(driver, CommandCategory.Settings)
    moveRunnerTo(driver, "settings-appearance-motion")
    driver.dispatch(RunnerSubmit).unsafeRunSync()
    moveRunnerSubmenuTo(driver, "settings-interface-layout")
    driver.dispatch(RunnerSubmit).unsafeRunSync()
    driver.snapshot.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.activeSubmenu.map(_.groupId)
        case _                                     => None
    } shouldBe Some("settings-interface-layout")
    moveRunnerSubmenuTo(driver, "command-runner-item-gap-rows")
    driver.dispatch(RunnerSubmit).unsafeRunSync()
    driver.dispatch(RunnerDeleteBackward).unsafeRunSync()
    driver.dispatch(RunnerInsertChar('1')).unsafeRunSync()
    driver.dispatch(RunnerSubmit).unsafeRunSync()
    driver.snapshot.unsafeRunSync().config.commandRunnerItemGapRows shouldBe 1
  }

  private def scenarioCommand(name: String, intent: CommandIntent): Command =
    Command.typed(name, name, intent, CommandCategory.Settings)

  private def focusToolbarItem(driver: UiScenarioDriver, itemId: String): Unit =
    driver
      .updateState { state =>
        val toolbarId = state.contextualToolbarSurface.map(_.id).getOrElse(fail("toolbar"))
        state.pushFocus(Focus.Surface(toolbarId))
      }
      .unsafeRunSync()
    val state  = driver.snapshot.unsafeRunSync()
    val items  = ContextualToolbar.itemsFor(state)
    val target = items.indexWhere(_.id == itemId)
    val current = state.contextualToolbarSurface
      .flatMap {
        _.content match
          case SurfaceContent.ContextualToolbar(toolbar) => Some(toolbar.focusedIndex)
          case _                                         => None
      }
      .getOrElse(fail("toolbar state"))
    (0 until ((target - current + items.length) % items.length)).foreach(_ =>
      driver.dispatch(MoveRight).unsafeRunSync()
    )

  private def moveRunnerTo(driver: UiScenarioDriver, itemId: String): Unit =
    val runner = driver.snapshot
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(value) => Some(value)
          case _                                    => None
      }
      .getOrElse(fail("command runner"))
    (0 until runner.visibleItems.indexWhere(_.id == itemId)).foreach(_ =>
      driver.dispatch(RunnerNavigate(Direction.Down)).unsafeRunSync()
    )

  private def enterRunnerCategory(driver: UiScenarioDriver, category: CommandCategory): Unit =
    val activeCategory = driver.snapshot
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner.activeCategory)
          case _                                     => None
      }
      .getOrElse(fail("command runner"))
    val categories = CommandCategory.values.toList
    (0 until ((categories
      .indexOf(category) - categories.indexOf(activeCategory) + categories.length) % categories.length))
      .foreach(_ => driver.dispatch(RunnerNextCategory).unsafeRunSync())

  private def moveRunnerSubmenuTo(driver: UiScenarioDriver, itemId: String): Unit =
    val runner = driver.snapshot
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(value) => Some(value)
          case _                                    => None
      }
      .getOrElse(fail("command runner"))
    (0 until runner.focusedSubmenuItems.indexWhere(_.id == itemId)).foreach(_ =>
      driver.dispatch(RunnerNavigate(Direction.Down)).unsafeRunSync()
    )
