package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.serenity.lsp.client.{LspMessageLevel, LspMessageRequest, LspServerMessage}
import com.serenity.state.models.{Notice, NoticeLevel, NoticePrompt, NoticePromptId, NoticeTopic}

/** Where the application takes the notices language servers cause. `withdraw` closes a question whose server has
  * stopped waiting for the answer.
  */
final case class LspNotices(show: Notice => IO[Unit], withdraw: NoticePromptId => IO[Unit])

object LspNotices:

  val ignoring: LspNotices = LspNotices(_ => IO.unit, _ => IO.unit)

/** What a language server tells the user, as corner notices (#1847): its `window/showMessage` texts, the questions it
  * asks with `window/showMessageRequest`, and its dying. A message repeated within `duplicateWindow` of its last
  * showing is dropped, since a server that complains does so in bursts and every copy would only push the one that
  * matters out of the corner.
  */
final private[lsp] class LspServerNotices private (
    notices: LspNotices,
    duplicateWindow: FiniteDuration,
    lastShown: Ref[IO, Map[LspServerNotices.Shown, FiniteDuration]],
    questions: Ref[IO, Map[NoticePromptId, Deferred[IO, Option[Int]]]],
    questionCount: Ref[IO, Long]
):

  import LspServerNotices.*

  def message(server: String, message: LspServerMessage): IO[Unit] =
    noticeLevel(message.level).filter(_ => message.shownToUser).traverse_ { level =>
      val text = plain(message.text)
      IO.monotonic.flatMap { now =>
        lastShown
          .modify { shown =>
            val current = shown.filter((_, at) => now - at < duplicateWindow)
            val key     = Shown(server, level, text)
            if current.contains(key) then (current, false) else (current.updated(key, now), true)
          }
          .ifM(notices.show(serverMessage(server, level, text)), IO.unit)
      }
    }

  /** The index of the action the user chose, or `None` for a dismissal. Waits for as long as the caller does: when it
    * is cancelled, the question is withdrawn from the screen. A question with no actions to choose from is shown as the
    * message it is, and answered at once.
    */
  def ask(server: String, request: LspMessageRequest): IO[Option[Int]] =
    if request.actions.isEmpty then message(server, LspServerMessage(request.level, request.text)).as(None)
    else
      for
        id     <- questionCount.updateAndGet(_ + 1).map(NoticePromptId(_))
        answer <- Deferred[IO, Option[Int]]
        _      <- questions.update(_ + (id -> answer))
        choice <- (notices.show(question(server, request, id)) >> answer.get)
          .onCancel(notices.withdraw(id))
          .guarantee(questions.update(_ - id))
      yield choice

  /** A second answer to the same question, or one to a question no longer waiting, is ignored. */
  def answer(id: NoticePromptId, choice: Option[Int]): IO[Unit] =
    questions.get.flatMap(_.get(id).traverse_(_.complete(choice).void))

  def stopped(server: String): IO[Unit] =
    notices.show(status(server, s"The $server language server stopped. Restarting it."))

  def gaveUp(server: String, explanation: String): IO[Unit] =
    notices.show(status(server, explanation))

private[lsp] object LspServerNotices:

  val DuplicateWindow: FiniteDuration = 5.seconds

  final private case class Shown(server: String, level: NoticeLevel, text: String)

  def create(notices: LspNotices, duplicateWindow: FiniteDuration = DuplicateWindow): IO[LspServerNotices] =
    for
      lastShown     <- Ref.of[IO, Map[Shown, FiniteDuration]](Map.empty)
      questions     <- Ref.of[IO, Map[NoticePromptId, Deferred[IO, Option[Int]]]](Map.empty)
      questionCount <- Ref.of[IO, Long](0L)
    yield new LspServerNotices(notices, duplicateWindow, lastShown, questions, questionCount)

  private def noticeLevel(level: LspMessageLevel): Option[NoticeLevel] =
    level match
      case LspMessageLevel.Error   => Some(NoticeLevel.Error)
      case LspMessageLevel.Warning => Some(NoticeLevel.Warning)
      case LspMessageLevel.Info    => Some(NoticeLevel.Info)
      case LspMessageLevel.Log     => None

  /** Servers wrap their texts to suit a log; the notice wraps its own. */
  private def plain(text: String): String =
    text.trim.split("\\s+").mkString(" ")

  private def serverMessage(server: String, level: NoticeLevel, text: String): Notice =
    Notice(level, s"$server language server: $text", topic = Some(NoticeTopic.ServerMessage(server, text)))

  private def question(server: String, request: LspMessageRequest, id: NoticePromptId): Notice =
    Notice(
      noticeLevel(request.level).getOrElse(NoticeLevel.Info),
      s"$server language server: ${plain(request.text)}",
      prompt = Some(NoticePrompt(id, request.actions.take(NoticePrompt.MaxActions).map(_.title)))
    )

  private def status(server: String, text: String): Notice =
    Notice(NoticeLevel.Warning, text, topic = Some(NoticeTopic.ServerStatus(server)))
