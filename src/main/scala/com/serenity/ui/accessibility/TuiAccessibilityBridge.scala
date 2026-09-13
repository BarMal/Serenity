package com.serenity.ui.accessibility

import java.util.concurrent.atomic.AtomicReference

/** Publishes canvas semantics as terminal escape sequences a screen reader can pick up (issue #1447).
  *
  * [[SwingAccessibilityBridge]] fabricates a parallel native widget tree because that is the only way Swing lets a
  * custom-painted canvas expose semantics to assistive tech. A terminal has no such accessibility-tree API to publish
  * into -- the standard baseline is the terminal emulator rendering text and an external screen reader (Orca via AT-SPI
  * on the emulator's own widget, a console screen reader, ...) reading that text and the window title directly. This
  * bridge speaks to that baseline instead of inventing one:
  *
  *   - OSC 0 retitles the terminal window with the focused node's role, name, value and selection state on every focus
  *     change, so a screen reader's window-title-changed announcement fires. The title is otherwise dead weight the
  *     emulator paints once at startup and never revisits.
  *   - An OSC 9 desktop notification carries every [[AccessibilityAnnouncement]] `AccessibilitySnapshot` computes,
  *     since raw text alone -- what a generic screen reader reading the terminal's cell grid already gets -- carries no
  *     semantic role, focus or selection state (a bare "Save" reads the same whether it is a button label or a status
  *     line).
  *
  * `write` is a plain `String => Unit` rather than a concrete terminal type so this stays exactly as surface-agnostic
  * as the [[AccessibilitySnapshot]] it consumes; the TUI runtime supplies the real terminal's writer.
  */
final class TuiAccessibilityBridge(write: String => Unit):
  import TuiAccessibilityBridge.*

  private val previousTitle = new AtomicReference[Option[String]](None)

  def publish(snapshot: AccessibilitySnapshot): Unit =
    val title = snapshot.focused.map(describe).getOrElse(DefaultTitle)
    if !previousTitle.get.contains(title) then
      write(setTitle(title))
      previousTitle.set(Some(title))
    snapshot.announcements.foreach(announcement => write(notifyAnnouncement(announcement.message)))

  private def describe(node: AccessibleNode): String =
    val value     = node.value.filter(_.nonEmpty).fold("")(current => s": $current")
    val selection = if node.selected then ", selected" else ""
    s"${node.role.toString.toLowerCase} ${node.name}$value$selection"

object TuiAccessibilityBridge:
  private val DefaultTitle = "Serenity editor"

  private val Escape: Char = 0x1b.toChar
  private val Bel: Char    = 0x07.toChar

  // Neither ESC nor BEL can appear in a well-formed OSC body, so a node name or announcement that happens to
  // contain one is stripped rather than terminating the sequence early and leaking the rest of it onto the
  // terminal as if it were a second, attacker-chosen escape sequence.
  private def escape(text: String): String = text.replace(Escape.toString, "").replace(Bel.toString, "")

  /** OSC 0 sets both the window and icon title -- the broadest-supported form (some terminals treat OSC 2 alone as
    * icon-only) and the one most screen-reader/AT-SPI integrations key off.
    */
  private[accessibility] def setTitle(title: String): String =
    s"$Escape]0;${escape(title)}$Bel"

  /** OSC 9 (iTerm2's growl-style notification, now widely supported) rather than OSC 777 (Konsole-specific, and
    * requiring a second `;`-delimited title field) -- broader support at the cost of no notification title.
    *
    * Named `notifyAnnouncement` rather than `notify` -- the latter silently resolves to `AnyRef.notify()` (an inherited
    * member always wins over a same-named import), which would fail with a mystifying arity mismatch at every call site
    * instead of a name clash.
    */
  private[accessibility] def notifyAnnouncement(message: String): String =
    s"$Escape]9;${escape(message)}$Bel"
