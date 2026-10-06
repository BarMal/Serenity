package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import com.serenity.state.models.{Notice, NoticeLevel, NoticePromptId, NoticeTopic}
import com.serenity.testkit.VirtualTime.runVirtual
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What `LspManager` shows the user of a server's traffic (#1847): the messages it asks to have shown, and the
  * questions it asks, each as a corner notice naming the server -- and what comes back to the server from the user's
  * answer.
  */
class LspManagerNoticeSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  private val server      = com.serenity.lsp.config.LanguageId.Scala.displayName
  private val importBuild = Json.obj("title" -> "Import build".asJson, "kind" -> "import".asJson)
  private val notNow      = Json.obj("title" -> "Not now".asJson)

  final private case class Screen(shown: Ref[IO, List[Notice]], withdrawn: Ref[IO, List[NoticePromptId]]):

    val notices: LspNotices =
      LspNotices(notice => shown.update(_ :+ notice), id => withdrawn.update(_ :+ id))

  private val screen: IO[Screen] =
    for
      shown     <- Ref.of[IO, List[Notice]](Nil)
      withdrawn <- Ref.of[IO, List[NoticePromptId]](Nil)
    yield Screen(shown, withdrawn)

  private def notification(method: String, level: Int, text: String): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "method"  -> method.asJson,
      "params"  -> Json.obj("type" -> level.asJson, "message" -> text.asJson)
    )

  private def showMessage(level: Int, text: String): Json = notification("window/showMessage", level, text)

  private def showMessageRequest(id: Long, actions: List[Json]): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> id.asJson,
      "method"  -> "window/showMessageRequest".asJson,
      "params" -> Json.obj(
        "type"    -> 3.asJson,
        "message" -> "New build detected".asJson,
        "actions" -> Json.fromValues(actions)
      )
    )

  private def message(level: NoticeLevel, text: String): Notice =
    Notice(level, s"$server language server: $text", topic = Some(NoticeTopic.ServerMessage(server, text)))

  private def promptIdOf(notice: Notice): NoticePromptId =
    notice.prompt.map(_.id).getOrElse(fail(s"not a question: $notice"))

  private def answer(manager: Harness, id: NoticePromptId, choice: Option[Int]): IO[Unit] =
    manager.effects.offer(Some(LspEffect.MessageRequestAnswered(id, choice)))

  private def result(reply: Json): Option[Json] = reply.hcursor.downField("result").focus

  "A server's window/showMessage" should "show an error, a warning and an info message as notices of that severity" in
    runVirtual(
      for
        on <- screen
        shown <- harness(notices = on.notices).use { manager =>
          for
            _     <- open(manager)
            _     <- manager.connection.handleIncomingJson(showMessage(1, "Build failed"))
            _     <- manager.connection.handleIncomingJson(showMessage(2, "Slow build"))
            _     <- manager.connection.handleIncomingJson(showMessage(3, "Build imported"))
            _     <- IO.sleep(1.second)
            shown <- on.shown.get
            _     <- manager.stop
          yield shown
        }
      yield shown shouldBe List(
        message(NoticeLevel.Error, "Build failed"),
        message(NoticeLevel.Warning, "Slow build"),
        message(NoticeLevel.Info, "Build imported")
      )
    )

  it should "keep log-level messages and everything in window/logMessage out of the corner" in
    runVirtual(
      for
        on <- screen
        shown <- harness(notices = on.notices).use { manager =>
          for
            _     <- open(manager)
            _     <- manager.connection.handleIncomingJson(showMessage(4, "Compiling"))
            _     <- manager.connection.handleIncomingJson(notification("window/logMessage", 1, "indexing failed"))
            _     <- IO.sleep(1.second)
            shown <- on.shown.get
            _     <- manager.stop
          yield shown
        }
      yield shown shouldBe empty
    )

  it should "show an identical message once within a short window, and again after it" in
    runVirtual(
      for
        on <- screen
        counts <- harness(notices = on.notices).use { manager =>
          val repeat = manager.connection.handleIncomingJson(showMessage(2, "Slow build"))
          for
            _      <- open(manager)
            _      <- repeat >> repeat
            _      <- IO.sleep(1.second)
            within <- on.shown.get.map(_.size)
            _      <- repeat
            _      <- IO.sleep(LspServerNotices.DuplicateWindow)
            _      <- repeat
            _      <- IO.sleep(1.second)
            after  <- on.shown.get.map(_.size)
            _      <- manager.stop
          yield (within, after)
        }
      yield counts shouldBe (1, 2)
    )

  it should "show messages that differ in their words, or only in their severity, separately" in
    runVirtual(
      for
        on <- screen
        shown <- harness(notices = on.notices).use { manager =>
          for
            _     <- open(manager)
            _     <- manager.connection.handleIncomingJson(showMessage(2, "Slow build"))
            _     <- manager.connection.handleIncomingJson(showMessage(1, "Slow build"))
            _     <- manager.connection.handleIncomingJson(showMessage(2, "Slower build"))
            _     <- IO.sleep(1.second)
            shown <- on.shown.get
            _     <- manager.stop
          yield shown.map(_.level)
        }
      yield shown shouldBe List(NoticeLevel.Warning, NoticeLevel.Error, NoticeLevel.Warning)
    )

  "A server's window/showMessageRequest" should "show its actions as a question, and reply with the action chosen" in
    runVirtual(
      for
        on <- screen
        outcome <- harness(notices = on.notices).use { manager =>
          for
            _     <- open(manager)
            _     <- manager.connection.handleIncomingJson(showMessageRequest(51, List(importBuild, notNow)))
            _     <- IO.sleep(1.second)
            shown <- on.shown.get
            quiet <- manager.connection.tryTakeOutgoing
            _     <- answer(manager, promptIdOf(shown.head), Some(0))
            reply <- takeMessage(manager.connection)
            _     <- manager.stop
          yield (shown, quiet, reply)
        }
      yield
        val (shown, quiet, reply) = outcome
        shown.map(_.copy(prompt = None)) shouldBe List(
          Notice(NoticeLevel.Info, s"$server language server: New build detected")
        )
        shown.flatMap(_.prompt).map(_.actions) shouldBe List(List("Import build", "Not now"))
        quiet shouldBe None
        result(reply) shouldBe Some(importBuild)
        reply.hcursor.downField("id").as[Long].toOption shouldBe Some(51L)
    )

  it should "reply null when the question is dismissed" in
    runVirtual(
      for
        on <- screen
        reply <- harness(notices = on.notices).use { manager =>
          for
            _     <- open(manager)
            _     <- manager.connection.handleIncomingJson(showMessageRequest(52, List(importBuild, notNow)))
            _     <- IO.sleep(1.second)
            shown <- on.shown.get
            _     <- answer(manager, promptIdOf(shown.head), None)
            reply <- takeMessage(manager.connection)
            _     <- manager.stop
          yield reply
        }
      yield result(reply) shouldBe Some(Json.Null)
    )

  it should "reply null, and withdraw the question, when nobody answers within the request timeout" in
    runVirtual(
      for
        on <- screen
        outcome <- harness(notices = on.notices).use { manager =>
          for
            _         <- open(manager)
            _         <- manager.connection.handleIncomingJson(showMessageRequest(53, List(importBuild, notNow)))
            _         <- IO.sleep(1.second)
            shown     <- on.shown.get
            _         <- IO.sleep(11.seconds)
            reply     <- takeMessage(manager.connection)
            withdrawn <- on.withdrawn.get
            _         <- manager.stop
          yield (shown.map(promptIdOf), reply, withdrawn)
        }
      yield
        val (asked, reply, withdrawn) = outcome
        result(reply) shouldBe Some(Json.Null)
        withdrawn shouldBe asked
    )

  it should "reply once, whether the user answers twice or answers after the timeout" in
    runVirtual(
      for
        on <- screen
        outcome <- harness(notices = on.notices).use { manager =>
          for
            _     <- open(manager)
            _     <- manager.connection.handleIncomingJson(showMessageRequest(54, List(importBuild, notNow)))
            _     <- manager.connection.handleIncomingJson(showMessageRequest(55, List(importBuild, notNow)))
            _     <- IO.sleep(1.second)
            shown <- on.shown.get
            first  = promptIdOf(shown.head)
            second = promptIdOf(shown(1))
            _        <- answer(manager, first, Some(1)) >> answer(manager, first, Some(0))
            reply    <- takeMessage(manager.connection)
            _        <- IO.sleep(11.seconds)
            timedOut <- takeMessage(manager.connection)
            _        <- answer(manager, second, Some(0))
            _        <- IO.sleep(1.second)
            nothing  <- manager.connection.tryTakeOutgoing
            _        <- manager.stop
          yield (reply, timedOut, nothing)
        }
      yield
        val (reply, timedOut, nothing) = outcome
        result(reply) shouldBe Some(notNow)
        result(timedOut) shouldBe Some(Json.Null)
        nothing shouldBe None
    )

  it should "show a question with no actions as the message it is, and reply null at once" in
    runVirtual(
      for
        on <- screen
        outcome <- harness(notices = on.notices).use { manager =>
          for
            _     <- open(manager)
            _     <- manager.connection.handleIncomingJson(showMessageRequest(56, Nil))
            reply <- takeMessage(manager.connection)
            _     <- IO.sleep(1.second)
            shown <- on.shown.get
            _     <- manager.stop
          yield (reply, shown)
        }
      yield
        val (reply, shown) = outcome
        result(reply) shouldBe Some(Json.Null)
        shown shouldBe List(message(NoticeLevel.Info, "New build detected"))
    )
