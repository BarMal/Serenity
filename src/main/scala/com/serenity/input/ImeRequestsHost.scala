package com.serenity.input

import java.awt.im.InputMethodRequests

/** A component whose `getInputMethodRequests` can be supplied after construction. AWT reads the requests object from an
  * overridable getter, so a component cannot be handed one from outside unless it opts in with this.
  */
trait ImeRequestsHost:
  def installInputMethodRequests(requests: InputMethodRequests): Unit
