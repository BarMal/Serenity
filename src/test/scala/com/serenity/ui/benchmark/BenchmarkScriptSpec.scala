package com.serenity.ui.benchmark

import com.serenity.rope.Balance
import com.serenity.state.models.{Damage, EditorPane}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenchmarkScriptSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewportLines = 40

  private def buffer(step: BenchmarkScript.Step) = step.state.persisted.buffers(BenchmarkDocument.bufferId)

  private def top(step: BenchmarkScript.Step): Int = buffer(step).viewport.topLine

  private def text(step: BenchmarkScript.Step): String = buffer(step).document.content.toString

  private def script(scenario: Scenario, frames: Int = 6) = BenchmarkScript.steps(scenario, frames, viewportLines)

  "BenchmarkDocument.initial" should "hold prose and code, with plenty of lines to scroll through" in {
    val document = BenchmarkDocument.initial
    val content  = document.persisted.buffers(BenchmarkDocument.bufferId).document.content
    content.lineCount should be >= 1000
    content.toString should include("lorem ipsum")
    content.toString should include("def ")
    document.persisted.layout.editorPanes.keys.toList should contain(
      EditorPane
        .withBuffer(
          document.persisted.layout.activeEditorPaneId.getOrElse(fail("no pane")),
          BenchmarkDocument.bufferId
        )
        .id
    )
  }

  "BenchmarkScript.steps" should "give exactly the frames asked for, in every scenario" in
    Scenario.values.foreach(scenario => withClue(scenario.label)(script(scenario, 9) should have size 9))

  it should "type one character per frame, with the damage of that edit" in {
    val steps = script(Scenario.TypeCharacter)
    steps.zip(steps.tail).foreach((before, after) => text(after).length shouldBe text(before).length + 1)
    steps.tail.foreach(step => step.damage should not be Damage.Nothing)
    steps.foreach(_.frame shouldBe BenchmarkScript.FrameKind.Full)
  }

  it should "scroll one line, five lines or a page per frame" in
    List(Scenario.ScrollOneLine -> 1, Scenario.ScrollFiveLines -> 5, Scenario.PageDown -> viewportLines).foreach {
      case (scenario, lines) =>
        val steps = script(scenario)
        steps
          .zip(steps.tail)
          .foreach((before, after) => withClue(scenario.label)(top(after) - top(before) shouldBe lines))
    }

  it should "keep scrolling inside the document however many frames are asked for" in {
    val lines = BenchmarkDocument.initial.persisted.buffers(BenchmarkDocument.bufferId).document.content.lineCount
    List(Scenario.ScrollOneLine, Scenario.ScrollFiveLines, Scenario.PageDown).foreach { scenario =>
      script(scenario, 3000).foreach(step => top(step) should (be >= 0 and be <= lines - viewportLines))
    }
  }

  it should "draw the same state again and again for the cold full frame, whole each time" in {
    val steps = script(Scenario.ColdFullFrame)
    steps.map(_.frame).toSet shouldBe Set(BenchmarkScript.FrameKind.ColdFull)
    steps.map(_.damage).toSet shouldBe Set(Damage.Everything)
    steps.map(text).toSet should have size 1
  }

  it should "toggle only the caret for the blink scenario, on one unchanged state" in {
    val steps = script(Scenario.CaretBlink)
    steps.map(_.frame) shouldBe Vector.tabulate(6)(index =>
      BenchmarkScript.FrameKind.CaretOnly(visible = index % 2 == 0)
    )
    steps.map(text).toSet should have size 1
    steps.map(top).toSet should have size 1
  }

  it should "redraw the same state whole with warm caches for the warm full frame" in {
    val steps = script(Scenario.WarmFullFrame)
    steps.map(_.frame).toSet shouldBe Set(BenchmarkScript.FrameKind.Full)
    steps.map(_.damage).toSet shouldBe Set(Damage.Everything)
    steps.map(text).toSet should have size 1
    steps.map(top).toSet should have size 1
  }

  private val windowPx = WindowPx(800, 600)

  private def resizedTo(step: BenchmarkScript.Step): WindowPx =
    step.frame match
      case BenchmarkScript.FrameKind.Resized(size) => size
      case other                                   => fail(s"expected a resized frame, got $other")

  it should "alternate the window between its own size and a smaller one for the resize scenario" in {
    val steps = BenchmarkScript.steps(Scenario.ResizeWindow, 6, viewportLines, windowPx)
    val sizes = steps.map(resizedTo)
    sizes.distinct should have size 2
    sizes.zip(sizes.tail).foreach((before, after) => after should not be before)
    sizes.distinct should contain(windowPx)
    val smaller = sizes.distinct.filterNot(_ == windowPx).head
    smaller.width should be < windowPx.width
    smaller.height should be < windowPx.height
  }

  it should "draw the same state whole at each size for the resize scenario" in {
    val steps = BenchmarkScript.steps(Scenario.ResizeWindow, 6, viewportLines, windowPx)
    steps.map(_.damage).toSet shouldBe Set(Damage.Everything)
    steps.map(text).toSet should have size 1
    steps.map(top).toSet should have size 1
  }

  it should "never ask for a resize in any other scenario" in
    Scenario.values.filterNot(_ == Scenario.ResizeWindow).foreach { scenario =>
      withClue(scenario.label) {
        script(scenario).collect { case BenchmarkScript.Step(_, _, BenchmarkScript.FrameKind.Resized(_)) => 1 } shouldBe empty
      }
    }

  it should "give both backends exactly the same script" in
    Scenario.values.foreach { scenario =>
      val (one, two) = (script(scenario), script(scenario))
      withClue(scenario.label) {
        one.map(_.frame) shouldBe two.map(_.frame)
        one.map(top) shouldBe two.map(top)
        one.map(text) shouldBe two.map(text)
        one.map(_.damage) shouldBe two.map(_.damage)
      }
    }
