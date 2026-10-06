package com.serenity

import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.renderer.RendererHighlights
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Find end to end (#1957), through the state pipeline, the background search and the renderer: the search starts from
  * the caret, the matches are painted in the text, options toggle from the find surface, and find-next after an edit
  * lands only on text that still matches.
  */
class FindUiScenarioSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val text = List("needle one", "plain", "Needle two", "plain", "needle three", "needle four").mkString("\n")

  private def caret(driver: UiScenarioDriver): CursorPosition =
    driver.state
      .map(state => state.activeBuffer.map(_.editing.cursors.head.position))
      .unsafeRunSync()
      .getOrElse(
        fail("no active buffer")
      )

  /** The editor focused on `content` with the caret at `at`, the start page out of the way. */
  private def editing(content: String, at: CursorPosition)(state: AppState): AppState =
    val withoutStartPage =
      state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.content match
        case SurfaceContent.StartPage(_) => true
        case _                           => false)))
    (withoutStartPage.persisted.layout.activeEditorPaneId, withoutStartPage.activeBuffer) match
      case (Some(paneId), Some(buffer)) =>
        withoutStartPage.copy(persisted =
          withoutStartPage.persisted.copy(
            focus = Focus.EditorPane(paneId),
            buffers = withoutStartPage.persisted.buffers.updated(
              buffer.id,
              buffer.copy(document = buffer.document.withContent(Rope(content)), editing = EditingState(List(at)))
            )
          )
        )
      case _ => fail("the scenario has no active editor pane")

  private def settle(driver: UiScenarioDriver)(events: Event*): Unit =
    (events.toList.traverse_(driver.dispatch) >> driver.stateManager.runtimeLifecycle.awaitEffects).unsafeRunSync()

  "Find" should "search from the caret, paint the matches, toggle options and never step onto stale text" in {
    val driver = UiScenarioDriver.create("find-from-caret").unsafeRunSync()
    driver.updateState(editing(text, CursorPosition(1, 2))).unsafeRunSync()

    settle(driver)(OpenFind)
    settle(driver)("needle".map(InsertChar(_))*)
    caret(driver) shouldBe CursorPosition(2, 0)

    val theme       = driver.state.unsafeRunSync().persisted.theme
    val frame       = driver.renderFrame("find-matches-painted").unsafeRunSync()
    val backgrounds = frame.evidence.paintedRegions.map(_.background).toSet
    backgrounds should contain(RendererHighlights.currentFindMatchBackground(theme))
    backgrounds should contain(RendererHighlights.findMatchBackground(theme))
    frame.evidence.drawnText.map(_.text).exists(_.contains("[ ] case")) shouldBe true

    settle(driver)(ModalToggleFindOption(FindOption.MatchCase))
    caret(driver) shouldBe CursorPosition(4, 0)
    val matchCaseFrame = driver.renderFrame("find-match-case").unsafeRunSync()
    matchCaseFrame.evidence.drawnText.map(_.text).exists(_.contains("[x] case")) shouldBe true

    // An edit the find did not make: the next match's line stops matching and a line is inserted above it.
    driver
      .updateState { state =>
        state.activeBuffer.fold(state) { buffer =>
          val edited =
            text.replace("needle four", "noodle four").replace("plain\nneedle three", "plain\nx\nneedle three")
          state.copy(persisted =
            state.persisted.copy(buffers =
              state.persisted.buffers
                .updated(buffer.id, buffer.copy(document = buffer.document.withContent(Rope(edited))))
            )
          )
        }
      }
      .unsafeRunSync()
    settle(driver)(FindNext)

    val landed = driver.state.unsafeRunSync().activeBuffer.getOrElse(fail("no active buffer"))
    val at     = landed.editing.cursors.head.position
    val offset = landed.document.content.lineColumnToOffset(at.line, at.column)
    landed.document.content.sliceString(offset, offset + "needle".length) shouldBe "needle"
  }

  it should "show an invalid regex as an error on the find surface instead of searching" in {
    val driver = UiScenarioDriver.create("find-invalid-regex").unsafeRunSync()
    driver.updateState(editing("needle", CursorPosition(0, 0))).unsafeRunSync()

    settle(driver)(OpenFind, ModalToggleFindOption(FindOption.Regex))
    settle(driver)("ne(dle".map(InsertChar(_))*)

    val frame = driver.renderFrame("find-invalid-regex").unsafeRunSync()
    frame.evidence.drawnText.map(_.text).exists(_.startsWith("Invalid regex")) shouldBe true
    driver.state.unsafeRunSync().activeBuffer.flatMap(_.findState) shouldBe None
  }
