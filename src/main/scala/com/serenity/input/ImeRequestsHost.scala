package com.serenity.input

import java.awt.Rectangle
import java.awt.im.InputMethodRequests

/** A component whose `getInputMethodRequests` can be supplied after construction, and that knows where its caret is.
  * AWT reads the requests object from an overridable getter, so a component cannot be handed one from outside unless it
  * opts in with this; `caretBounds` is where the platform is told to put its candidate window.
  */
trait ImeRequestsHost:
  def installInputMethodRequests(requests: InputMethodRequests): Unit

  /** The primary caret in the component's own coordinates, once one has been drawn. */
  def caretBounds: Option[Rectangle]
