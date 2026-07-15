package com.serenity.scenario

import scala.io.Source

import cats.effect.unsafe.implicits.global
import com.serenity.config.MarkdownViewMode
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.{CursorPosition, Focus, PaneId}
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
    frame.markdownMappings.map(_.sourceLine) should contain(2)
    frame.image.getWidth shouldBe 960
    frame.image.getHeight shouldBe 576
    frame.diagnostics shouldBe empty
  }

  it should "expose matching command item geometry for rendered rows and mouse events" in {
    val driver = UiScenarioDriver.create(viewport = ViewportSize(100, 30)).unsafeRunSync()

    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    driver.dispatch(RunnerInsertChar('t')).unsafeRunSync()
    val frame = driver.render().unsafeRunSync()
    val item  = frame.itemRects.values.flatten.find(_.label.toLowerCase.contains("theme")).getOrElse(fail("theme item"))

    driver.click(item.rect.x, item.rect.y).unsafeRunSync()
    driver.render().unsafeRunSync().activeFocus shouldBe frame.activeFocus
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
