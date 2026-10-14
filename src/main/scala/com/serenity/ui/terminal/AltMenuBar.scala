package com.serenity.ui.terminal

import java.awt.event.{AWTEventListener, FocusAdapter, FocusEvent, KeyEvent, MouseEvent}
import java.awt.{AWTEvent, KeyEventDispatcher, KeyboardFocusManager, Toolkit}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.event.ChangeListener
import javax.swing.{JMenuBar, MenuElement, MenuSelectionManager, SwingUtilities}

import cats.effect.{IO, Resource}

/** The `alt` setting of `ui.menu_bar`: [[AltMenuBarController]] wired to the window's keyboard, pointer and menu
  * selection. None of the listeners consumes an event, so the editor sees every key exactly as it does without them.
  */
object AltMenuBar:

  def resource(window: SwingWindow, bar: JMenuBar): Resource[IO, Unit] =
    Resource
      .make(IO.blocking(onEventDispatchThread(() => attach(window, bar))))(detach =>
        IO.blocking(onEventDispatchThread(detach))
      )
      .map(_ => ())

  private def attach(window: SwingWindow, bar: JMenuBar): () => Unit =
    val controller = new AltMenuBarController(presenter(window, bar))
    val keys: KeyEventDispatcher = (event: KeyEvent) =>
      if window.isActive then controller.onKey(event)
      false
    val pointer: AWTEventListener = (event: AWTEvent) =>
      if event.getID == MouseEvent.MOUSE_PRESSED then controller.onPointerPressed()
    val selection: ChangeListener = _ =>
      if MenuSelectionManager.defaultManager().getSelectedPath.isEmpty then controller.onMenuSelectionCleared()

    val editorFocus = new FocusAdapter:
      override def focusGained(event: FocusEvent): Unit = controller.onEditorFocused()

    val focus = KeyboardFocusManager.getCurrentKeyboardFocusManager
    window.canvas.addFocusListener(editorFocus)
    focus.addKeyEventDispatcher(keys)
    Toolkit.getDefaultToolkit.addAWTEventListener(pointer, AWTEvent.MOUSE_EVENT_MASK)
    MenuSelectionManager.defaultManager().addChangeListener(selection)
    () =>
      window.canvas.removeFocusListener(editorFocus)
      focus.removeKeyEventDispatcher(keys)
      Toolkit.getDefaultToolkit.removeAWTEventListener(pointer)
      MenuSelectionManager.defaultManager().removeChangeListener(selection)
      window.removeMenuBar()

  private def presenter(window: SwingWindow, bar: JMenuBar): AltMenuBarController.Presenter =
    new AltMenuBarController.Presenter:
      def isShown: Boolean = window.isMenuBarInstalled

      def show(): Unit =
        window.installMenuBar(bar)
        bar.setFocusable(true)
        val _ = bar.requestFocusInWindow()
        Option
          .when(bar.getMenuCount > 0)(bar.getMenu(0))
          .foreach: first =>
            MenuSelectionManager.defaultManager().setSelectedPath(Array[MenuElement](bar, first))

      def hide(): Unit =
        window.removeMenuBar()
        MenuSelectionManager.defaultManager().clearSelectedPath()
        val _ = window.canvas.requestFocusInWindow()

  private def onEventDispatchThread[A](action: () => A): A =
    if SwingUtilities.isEventDispatchThread then action()
    else
      val result = new AtomicReference[A]()
      SwingUtilities.invokeAndWait(() => result.set(action()))
      result.get()
