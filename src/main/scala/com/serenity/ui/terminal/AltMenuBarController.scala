package com.serenity.ui.terminal

import java.awt.event.KeyEvent
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

/** Shows the menu bar for a tap of Alt and hides it again when the menu it opened is finished with, so the window has
  * no bar the rest of the time. Everything here runs on the event-dispatch thread.
  */
final class AltMenuBarController(presenter: AltMenuBarController.Presenter):

  private val tap     = new AtomicReference(AltTap.State.Idle)
  private val opening = new AtomicBoolean(false)

  def onKey(event: KeyEvent): Unit =
    AltTap.inputOf(event).foreach(feed)

  def onPointerPressed(): Unit = feed(AltTap.Input.OtherDown)

  /** A menu command ran, Escape was pressed or a click went elsewhere: whichever it was, no menu is selected. */
  def onMenuSelectionCleared(): Unit = hideUnlessOpening()

  def onEditorFocused(): Unit = hideUnlessOpening()

  private def hideUnlessOpening(): Unit =
    if presenter.isShown && !opening.get then presenter.hide()

  private def feed(input: AltTap.Input): Unit =
    val (next, tapped) = AltTap.step(tap.get(), input)
    tap.set(next)
    if tapped then toggle()

  private def toggle(): Unit =
    if presenter.isShown then presenter.hide()
    else
      opening.set(true)
      try presenter.show()
      finally opening.set(false)

object AltMenuBarController:

  trait Presenter:
    def isShown: Boolean

    /** Installs the bar, focuses it so the menu keys reach it, and selects its first menu. */
    def show(): Unit

    def hide(): Unit
