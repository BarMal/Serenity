package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Enter, Escape, InsertChar, MouseClick, MousePress}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.seededStateManager
import com.serenity.state.models.*
import com.serenity.state.reducers.{FallbackHeaderReducer, ModalStateReducer, PeekStateReducer}
import com.serenity.ui.layout.{PeekContent, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The fallback header takes Escape before anything else, and a click on it, and is left alone by every other key. */
class FallbackHeaderEscapeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(80, 24)
  private val message  = "Skia could not load: no raster surface. Running in the terminal."

  private def withHeader(shape: AppState => AppState = identity): StateManager =
    seededStateManager { initial =>
      val sized = initial.copy(runtime = initial.runtime.copy(viewportSize = Some(viewport)))
      FallbackHeaderReducer.shown(shape(sized), message)
    }.unsafeRunSync()

  private def state(manager: StateManager): AppState = manager.getCurrentState.unsafeRunSync()

  private def shown(manager: StateManager): Boolean = FallbackHeaderReducer.isShown(state(manager))

  "Escape" should "dismiss the header" in {
    val manager = withHeader()

    manager.applyEvent(Escape).unsafeRunSync()

    shown(manager) shouldBe false
  }

  it should "reach a blocking modal only on the second press, the header having taken the first" in {
    val manager = withHeader(ModalStateReducer.show(Modal.Confirm(ConfirmPrompt.startupNotice("Hello")), _).state)
    state(manager).hasBlockingModal shouldBe true

    manager.applyEvent(Escape).unsafeRunSync()
    shown(manager) shouldBe false
    state(manager).hasBlockingModal shouldBe true

    manager.applyEvent(Escape).unsafeRunSync()
    state(manager).hasBlockingModal shouldBe false
  }

  it should "reach a peek only on the second press, the header having taken the first" in {
    val manager = withHeader(PeekStateReducer.show(PeekContent.QuickInfo("hover"), CursorPosition(0, 0), _).state)
    state(manager).floatingSurfaces.exists(_.isFloatingPeek) shouldBe true

    manager.applyEvent(Escape).unsafeRunSync()
    shown(manager) shouldBe false
    state(manager).floatingSurfaces.exists(_.isFloatingPeek) shouldBe true

    manager.applyEvent(Escape).unsafeRunSync()
    state(manager).floatingSurfaces.exists(_.isFloatingPeek) shouldBe false
  }

  it should "do nothing to the surfaces when there is no header" in {
    val manager = seededStateManager(identity).unsafeRunSync()
    val before  = state(manager).runtime.uiSurfaces

    manager.applyEvent(Escape).unsafeRunSync()

    state(manager).runtime.uiSurfaces shouldBe before
  }

  "Any other key" should "leave the header up" in {
    val manager = withHeader()

    manager.applyEvent(InsertChar('a')).unsafeRunSync()
    manager.applyEvent(Enter).unsafeRunSync()

    shown(manager) shouldBe true
  }

  "A click on the header's row" should "dismiss it" in {
    val manager = withHeader()

    manager.applyEvent(MouseClick(5, 0)).unsafeRunSync()

    shown(manager) shouldBe false
  }

  it should "be consumed: a press there changes nothing and leaves the header up" in {
    val manager = withHeader()
    val before  = state(manager)

    manager.applyEvent(MousePress(5, 0)).unsafeRunSync()

    state(manager).persisted.focus shouldBe before.persisted.focus
    shown(manager) shouldBe true
  }

  "A click below the header's row" should "leave it up" in {
    val manager = withHeader()

    manager.applyEvent(MouseClick(5, 3)).unsafeRunSync()

    shown(manager) shouldBe true
  }

end FallbackHeaderEscapeSpec
