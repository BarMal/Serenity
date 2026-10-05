package com.serenity.state.models

import scala.concurrent.duration.*

/** How much a [[Notice]] matters, which decides how long it stays: an error waits for the user to dismiss it. */
enum NoticeLevel(val label: String, val autoDismissAfter: Option[FiniteDuration]):
  case Info    extends NoticeLevel("Info", Some(4.seconds))
  case Warning extends NoticeLevel("Warning", Some(8.seconds))
  case Error   extends NoticeLevel("Error", None)

/** What a notice is about. A newer notice on a topic replaces the older one, so a retry that fails again does not stack
  * a second copy, and the topic's own success can clear it.
  */
enum NoticeTopic:
  case FileSave(bufferId: BufferId)
  case SessionSave

/** A message shown in a screen corner that never takes focus: keys go on to whatever has it, and Escape dismisses it.
  * `hint` names the keys for what the user can do next, as text: the notice itself has no controls to focus.
  */
final case class Notice(
    level: NoticeLevel,
    message: String,
    hint: Option[String] = None,
    topic: Option[NoticeTopic] = None
)
