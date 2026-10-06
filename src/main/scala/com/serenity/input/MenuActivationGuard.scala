package com.serenity.input

import javax.swing.KeyStroke

import com.serenity.keystroke.events.Event

/** Keeps a menu accelerator from running a command the keymap already ran.
  *
  * The canvas key listener runs before Swing's accelerator bindings and never consumes the key, so one keystroke can
  * reach both paths. The keymap is the only path that dispatches keys: an activation whose accelerator is the stroke
  * the canvas last queued is that same keystroke, either arriving as a key event or within [[KeyPressWindowMillis]] of
  * it, and is dropped. A click, or an accelerator the canvas never saw, is sent.
  */
final class MenuActivationGuard(
    lastPress: () => Option[(KeyStroke, Long)],
    send: Event => Unit,
    now: () => Long = () => System.currentTimeMillis
):

  def activate(event: Event, accelerator: Option[KeyStroke], viaKeyEvent: Boolean): Unit =
    if !alreadyDispatchedByKeymap(accelerator, viaKeyEvent) then send(event)

  private def alreadyDispatchedByKeymap(accelerator: Option[KeyStroke], viaKeyEvent: Boolean): Boolean =
    (accelerator, lastPress()) match
      case (Some(stroke), Some((pressed, at))) =>
        stroke == pressed && (viaKeyEvent || now() - at < MenuActivationGuard.KeyPressWindowMillis)
      case _ => false

object MenuActivationGuard:
  val KeyPressWindowMillis: Long = 100L
