package com.serenity.lsp.model

/** One step of a server's `$/progress` work-done report (LSP 3.17 §3.18.1), already stripped of its token. */
enum LspProgress:
  case Begin(title: String, message: Option[String], percentage: Option[Int])
  case Report(message: Option[String], percentage: Option[Int])
  case End(message: Option[String])

/** A unit of server work still running, as the status line shows it. */
final case class LspProgressTask(
    token: String,
    title: String,
    message: Option[String],
    percentage: Option[Int]
):

  def display: String =
    (Some(title) ++ message.filter(_.nonEmpty) ++ percentage.map(percent => s"$percent%")).mkString(" ")

object LspProgressTask:

  /** `Begin` starts (or restarts) the task, `Report` refines the one it names, `End` finishes it; a report for a task
    * that never began is dropped, as the spec forbids it and there is no title to show.
    */
  def advance(tasks: List[LspProgressTask], token: String, progress: LspProgress): List[LspProgressTask] =
    progress match
      case LspProgress.Begin(title, message, percentage) =>
        tasks.filterNot(_.token == token) :+ LspProgressTask(token, title, message, percentage)
      case LspProgress.Report(message, percentage) =>
        tasks.map { task =>
          if task.token == token then
            task.copy(message = message.orElse(task.message), percentage = percentage.orElse(task.percentage))
          else task
        }
      case LspProgress.End(_) => tasks.filterNot(_.token == token)
