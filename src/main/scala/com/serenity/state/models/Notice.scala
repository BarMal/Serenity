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

  /** What reloading the config file after an outside edit had to say. */
  case ConfigFile

  /** A language server's message, so the same words repeated while still on screen show once. */
  case ServerMessage(server: String, text: String)

  /** A language server stopping and being restarted or given up on: the later word replaces the earlier. */
  case ServerStatus(server: String)

/** Identifies the question a [[NoticePrompt]] asks, so its answer finds its way back to whoever asked. */
final case class NoticePromptId(value: Long)

/** The actions a notice offers (a language server's `window/showMessageRequest`). `highlighted` starts empty so that an
  * Enter typed into the editor when the prompt appears never chooses for the user.
  */
final case class NoticePrompt(id: NoticePromptId, actions: List[String], highlighted: Option[Int] = None)

object NoticePrompt:

  /** More than this are not reachable by a glance or a few keys; a server offering more is asking too much of a corner.
    */
  val MaxActions: Int = 5

/** A message shown in a screen corner that never takes focus: keys go on to whatever has it, and Escape dismisses it.
  * `hint` names the keys for what the user can do next, as text: the notice itself has no controls to focus.
  *
  * A notice with a `prompt` is a question and does take focus, so its actions can be chosen from the keyboard; it stays
  * until answered whatever its level, since its asker is waiting on the reply.
  */
final case class Notice(
    level: NoticeLevel,
    message: String,
    hint: Option[String] = None,
    topic: Option[NoticeTopic] = None,
    prompt: Option[NoticePrompt] = None
):

  def autoDismissAfter: Option[FiniteDuration] =
    if prompt.isDefined then None else level.autoDismissAfter
