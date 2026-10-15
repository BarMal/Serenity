package com.serenity.ui.terminal

import java.awt.event.{InputEvent, KeyEvent}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import javax.swing.JPanel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AltMenuBarControllerSpec extends AnyFlatSpec with Matchers:

  final private class FakePresenter extends AltMenuBarController.Presenter:
    private val showing                       = new AtomicBoolean(false)
    private val showCount                     = new AtomicInteger(0)
    private val hideCount                     = new AtomicInteger(0)
    private val duringShow                    = new AtomicReference[() => Unit](() => ())
    def shown: Boolean                        = showing.get
    def shows: Int                            = showCount.get
    def hides: Int                            = hideCount.get
    def whenShowing(action: () => Unit): Unit = duringShow.set(action)
    def isShown: Boolean                      = showing.get

    def show(): Unit =
      showing.set(true)
      showCount.incrementAndGet()
      duringShow.get().apply()

    def hide(): Unit =
      showing.set(false)
      hideCount.incrementAndGet()

  private val source = new JPanel

  private def key(id: Int, code: Int, modifiersEx: Int = 0): KeyEvent =
    new KeyEvent(source, id, 0L, modifiersEx, code, KeyEvent.CHAR_UNDEFINED)

  private def altDown(controller: AltMenuBarController): Unit =
    controller.onKey(key(KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT, InputEvent.ALT_DOWN_MASK))

  private def altUp(controller: AltMenuBarController): Unit =
    controller.onKey(key(KeyEvent.KEY_RELEASED, KeyEvent.VK_ALT))

  private def fixture =
    val presenter = new FakePresenter
    (presenter, new AltMenuBarController(presenter))

  "A tap of Alt" should "show a bar that is hidden and hide one that is shown" in {
    val (presenter, controller) = fixture

    altDown(controller)
    presenter.shown shouldBe false
    altUp(controller)
    presenter.shown shouldBe true

    altDown(controller)
    altUp(controller)
    presenter.shown shouldBe false
    (presenter.shows, presenter.hides) shouldBe (1, 1)
  }

  "Alt held for a shortcut" should "neither show nor hide the bar" in {
    val (presenter, controller) = fixture

    altDown(controller)
    controller.onKey(key(KeyEvent.KEY_PRESSED, KeyEvent.VK_F, InputEvent.ALT_DOWN_MASK))
    controller.onKey(key(KeyEvent.KEY_RELEASED, KeyEvent.VK_F, InputEvent.ALT_DOWN_MASK))
    altUp(controller)

    (presenter.shows, presenter.hides) shouldBe (0, 0)
  }

  it should "not hide a bar that is shown" in {
    val (presenter, controller) = fixture
    altDown(controller)
    altUp(controller)

    altDown(controller)
    controller.onKey(key(KeyEvent.KEY_PRESSED, KeyEvent.VK_S, InputEvent.ALT_DOWN_MASK))
    altUp(controller)

    presenter.shown shouldBe true
  }

  "A click while Alt is down" should "stop the release being a tap" in {
    val (presenter, controller) = fixture

    altDown(controller)
    controller.onPointerPressed()
    altUp(controller)

    presenter.shows shouldBe 0
  }

  "A menu selection that ends" should "hide a bar that is shown" in {
    val (presenter, controller) = fixture
    altDown(controller)
    altUp(controller)

    controller.onMenuSelectionCleared()

    presenter.shown shouldBe false
  }

  "Focus returning to the editor" should "hide a bar that is shown and leave a hidden one alone" in {
    val (presenter, controller) = fixture

    controller.onEditorFocused()
    presenter.hides shouldBe 0

    altDown(controller)
    altUp(controller)
    controller.onEditorFocused()

    presenter.shown shouldBe false
    presenter.hides shouldBe 1
  }

  "A menu selection that ends with no bar shown" should "leave things alone" in {
    val (presenter, controller) = fixture

    controller.onMenuSelectionCleared()

    presenter.hides shouldBe 0
  }

  it should "not hide the bar while it is still being shown and the selection is being set" in {
    val (presenter, controller) = fixture
    presenter.whenShowing(() => controller.onMenuSelectionCleared())

    altDown(controller)
    altUp(controller)

    presenter.shown shouldBe true
    presenter.hides shouldBe 0
  }
