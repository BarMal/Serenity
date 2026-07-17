package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextualToolbarUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Contextual toolbar UI scenario" should "summon, follow the caret, and return focus on close" in {
    val driver = UiScenarioDriver
      .create("contextual-toolbar", UiScenarioEnvironment(viewport = com.serenity.ui.layout.ViewportSize(60, 18)))
      .unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    val opened    = driver.renderFrame("opened").unsafeRunSync()
    val surfaceId = opened.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected toolbar rectangle"))
    val initial   = opened.evidence.surfaceRects(surfaceId)
    initial.width should be < driver.environment.viewport.width
    opened.evidence.focus shouldBe Focus.EditorPane(PaneId(0))

    driver.stateManager.setCursorPosition(PaneId(0), 0, 1).unsafeRunSync()
    driver.dispatch(InsertChar('x')).unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    val closed = driver.renderFrame("closed").unsafeRunSync()

    closed.evidence.surfaceRects shouldBe empty
    closed.evidence.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "exercise button, dropdown, input, and wrapped narrow navigation" in {
    val driver = UiScenarioDriver
      .create(
        "contextual-toolbar-controls",
        UiScenarioEnvironment(viewport = com.serenity.ui.layout.ViewportSize(42, 18))
      )
      .unsafeRunSync()
    driver
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(BufferId(0))
        val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, buffer))
      }
      .unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    val opened    = driver.renderFrame("wrapped").unsafeRunSync()
    val surfaceId = opened.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected toolbar"))
    opened.evidence.surfaceRects(surfaceId).width should be < driver.environment.viewport.width

    focusItem(driver, "bold")
    driver.dispatch(Enter).unsafeRunSync()
    driver.state.unsafeRunSync().buffers.values.flatMap(_.richTextDocument).toList should not be empty

    focusItem(driver, "paragraph-role")
    driver.dispatch(Enter).unsafeRunSync()
    toolbarState(driver).detailState.getOrElse(fail("Expected dropdown detail")) shouldBe
      a[ContextualToolbarDetailState.Dropdown]
    driver.dispatch(MoveRight).unsafeRunSync()
    driver.dispatch(Enter).unsafeRunSync()
    driver.state.unsafeRunSync().focus shouldBe Focus.EditorPane(PaneId(0))

    focusItem(driver, "font-size")
    driver.dispatch(Enter).unsafeRunSync()
    toolbarState(driver).detailState.getOrElse(fail("Expected input detail")) shouldBe
      a[ContextualToolbarDetailState.Input]
    driver.dispatch(DeleteBackward).unsafeRunSync()
    driver.dispatch(DeleteBackward).unsafeRunSync()
    driver.dispatch(InsertChar('2')).unsafeRunSync()
    driver.dispatch(InsertChar('0')).unsafeRunSync()
    driver.dispatch(Enter).unsafeRunSync()
    driver.state.unsafeRunSync().focus shouldBe Focus.EditorPane(PaneId(0))
  }

  private def focusItem(driver: UiScenarioDriver, itemId: String): Unit =
    driver
      .updateState { state =>
        val surface = state.contextualToolbarSurface.getOrElse(fail("Expected toolbar"))
        val items   = ContextualToolbar.itemsFor(state)
        val index   = items.indexWhere(_.id == itemId)
        val toolbar = surface.content match
          case SurfaceContent.ContextualToolbar(value) => value.copy(focusedIndex = index, detailState = None)
          case _                                       => fail("Expected toolbar content")
        state.copy(
          uiSurfaces = state.uiSurfaces.map(current =>
            if current.id == surface.id then current.copy(content = SurfaceContent.ContextualToolbar(toolbar))
            else current
          ),
          focus = Focus.Surface(surface.id)
        )
      }
      .unsafeRunSync()

  private def toolbarState(driver: UiScenarioDriver): ContextualToolbarState =
    driver.state.unsafeRunSync().contextualToolbarSurface.getOrElse(fail("Expected toolbar")).content match
      case SurfaceContent.ContextualToolbar(value) => value
      case _                                       => fail("Expected toolbar content")
