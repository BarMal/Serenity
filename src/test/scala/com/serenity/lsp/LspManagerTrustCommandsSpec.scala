package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.Diagnostic
import com.serenity.state.models.Notice
import com.serenity.testkit.RopeText
import fs2.Stream
import io.circe.Json
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The palette's "Trust This Folder" and "Revoke Folder Trust" (#2526): trusting goes the way accepting the notice
  * does, and revoking stops the running servers and announces the documents again, so only folders still trusted get
  * theirs back.
  */
class LspManagerTrustCommandsSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  private val testTimeout = 20.seconds
  private val uri         = "file:///workspace/One.scala"

  final private case class Running(
      effects: Queue[IO, Option[LspEffect]],
      live: Ref[IO, Int],
      events: Ref[IO, List[Event]],
      notices: Ref[IO, List[Notice]],
      asked: Ref[IO, List[String]]
  ):
    def offer(effect: LspEffect): IO[Unit] = effects.offer(Some(effect))

    def eventually[A](read: IO[A])(done: A => Boolean): IO[A] =
      (IO.sleep(20.millis) >> read).iterateUntil(done).timeoutTo(5.seconds, read)

  private def running(change: LspManager.TrustChange)(test: Running => IO[Assertion]): Assertion =
    val program =
      for
        effects <- Queue.unbounded[IO, Option[LspEffect]]
        live    <- Ref.of[IO, Int](0)
        events  <- Ref.of[IO, List[Event]](Nil)
        notices <- Ref.of[IO, List[Notice]](Nil)
        asked   <- Ref.of[IO, List[String]](Nil)
        provider = new LspManager.ConnectionProvider:
          def resolve(
            languageId: LanguageId,
            fileUri: DocumentUri,
            onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit]
          ): IO[Option[LspManager.ResolvedConnection]] =
            val root = WorkspaceRootUri("file:///workspace")
            IO.pure(Some(LspManager.ResolvedConnection(LspManager.ConnectionIdentity(root, scalaServer), server(root))))

          override def trustFolderOf(languageId: LanguageId, fileUri: DocumentUri): IO[LspManager.TrustChange] =
            asked.update(_ :+ s"trust ${fileUri.value}").as(change)

          override def revokeTrustOf(languageId: LanguageId, fileUri: DocumentUri): IO[LspManager.TrustChange] =
            asked.update(_ :+ s"revoke ${fileUri.value}").as(change)

          private def server(root: WorkspaceRootUri): Resource[IO, LspConnection] =
            MockLspServer
              .resource(Map("initialize" -> Json.obj("capabilities" -> Json.obj())), logger)
              .flatMap(mock => LspConnection.connect(LanguageId.Scala, mock.clientIn, mock.clientOut, root, logger))
              .evalTap(_ => live.update(_ + 1))
              .onFinalize(live.update(_ - 1))
        result <- Resource
          .make(
            LspManager
              .runWithProvider(
                Stream.fromQueueNoneTerminated(effects),
                event => events.update(_ :+ event),
                logger,
                provider,
                notices = LspNotices(notice => notices.update(_ :+ notice), _ => IO.unit)
              )
              .start
          )(_.cancel)
          .use(_ => test(Running(effects, live, events, notices, asked)))
      yield result
    program.timeout(testTimeout * 2).unsafeRunSync()

  "Trust This Folder" should "trust the document's folder and say so, leaving running servers alone" in
    running(LspManager.TrustChange("Trusted /workspace.", stopServers = false)) { manager =>
      for
        _       <- manager.offer(LspEffect.FileOpened(uri, LanguageId.Scala, RopeText("object One")))
        _       <- manager.eventually(manager.live.get)(_ == 1)
        _       <- manager.offer(LspEffect.TrustFolder(uri, LanguageId.Scala))
        notices <- manager.eventually(manager.notices.get)(_.nonEmpty)
        asked   <- manager.asked.get
        live    <- manager.live.get
      yield
        asked shouldBe List(s"trust $uri")
        notices.map(_.message) shouldBe List("Trusted /workspace.")
        live shouldBe 1
    }

  "Revoke Folder Trust" should "stop the running servers and have the documents announced again" in
    running(LspManager.TrustChange("Revoked trust for /workspace.", stopServers = true)) { manager =>
      for
        _       <- manager.offer(LspEffect.FileOpened(uri, LanguageId.Scala, RopeText("object One")))
        _       <- manager.eventually(manager.live.get)(_ == 1)
        _       <- manager.offer(LspEffect.RevokeFolderTrust(uri, LanguageId.Scala))
        live    <- manager.eventually(manager.live.get)(_ == 0)
        events  <- manager.eventually(manager.events.get)(_.contains(LspEvent.LspTrustRevoked))
        notices <- manager.notices.get
      yield
        live shouldBe 0
        events should contain(LspEvent.LspTrustRevoked)
        notices.map(_.message) shouldBe List("Revoked trust for /workspace.")
    }

  it should "leave everything running when nothing was trusted" in
    running(LspManager.TrustChange("/workspace is not in a trusted folder.", stopServers = false)) { manager =>
      for
        _       <- manager.offer(LspEffect.FileOpened(uri, LanguageId.Scala, RopeText("object One")))
        _       <- manager.eventually(manager.live.get)(_ == 1)
        _       <- manager.offer(LspEffect.RevokeFolderTrust(uri, LanguageId.Scala))
        notices <- manager.eventually(manager.notices.get)(_.nonEmpty)
        _       <- IO.sleep(300.millis)
        live    <- manager.live.get
        events  <- manager.events.get
      yield
        live shouldBe 1
        events should not contain LspEvent.LspTrustRevoked
        notices.map(_.message) shouldBe List("/workspace is not in a trusted folder.")
    }
