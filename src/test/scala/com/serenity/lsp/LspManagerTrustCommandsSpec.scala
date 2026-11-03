package com.serenity.lsp

import java.nio.file.Paths

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
  * does; revoking stops the servers of the revoked folder only, through the pool's general `releaseRoot`, and leaves
  * every other project's servers running.
  */
class LspManagerTrustCommandsSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  private val testTimeout = 20.seconds
  private val oneRoot     = "file:///workspace-one"
  private val twoRoot     = "file:///workspace-two"
  private val oneUri      = s"$oneRoot/One.scala"
  private val twoUri      = s"$twoRoot/Two.scala"

  final private case class Running(
      effects: Queue[IO, Option[LspEffect]],
      servers: Queue[IO, (String, MockLspServer)],
      live: Ref[IO, Set[String]],
      events: Ref[IO, List[Event]],
      notices: Ref[IO, List[Notice]],
      asked: Ref[IO, List[String]]
  ):
    def offer(effect: LspEffect): IO[Unit] = effects.offer(Some(effect))

    def nextServer: IO[(String, MockLspServer)] = servers.take.timeout(testTimeout)

    def eventually[A](read: IO[A])(done: A => Boolean): IO[A] =
      (IO.sleep(20.millis) >> read).iterateUntil(done).timeoutTo(5.seconds, read)

    def receivedWithin(server: MockLspServer, count: Int): IO[List[Json]] =
      Stream.repeatEval(server.takeReceived).take(count.toLong).interruptAfter(10.seconds).compile.toList

  private def running(change: LspManager.TrustChange)(test: Running => IO[Assertion]): Assertion =
    val program =
      for
        effects <- Queue.unbounded[IO, Option[LspEffect]]
        servers <- Queue.unbounded[IO, (String, MockLspServer)]
        live    <- Ref.of[IO, Set[String]](Set.empty)
        events  <- Ref.of[IO, List[Event]](Nil)
        notices <- Ref.of[IO, List[Notice]](Nil)
        asked   <- Ref.of[IO, List[String]](Nil)
        provider = new LspManager.ConnectionProvider:
          def resolve(
            languageId: LanguageId,
            fileUri: DocumentUri,
            onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit]
          ): IO[Option[LspManager.ResolvedConnection]] =
            val root = WorkspaceRootUri(fileUri.value.substring(0, fileUri.value.lastIndexOf('/')))
            IO.pure(Some(LspManager.ResolvedConnection(LspManager.ConnectionIdentity(root, scalaServer), server(root))))

          override def trustFolderOf(languageId: LanguageId, fileUri: DocumentUri): IO[LspManager.TrustChange] =
            asked.update(_ :+ s"trust ${fileUri.value}").as(change)

          override def revokeTrustOf(languageId: LanguageId, fileUri: DocumentUri): IO[LspManager.TrustChange] =
            asked.update(_ :+ s"revoke ${fileUri.value}").as(change)

          private def server(root: WorkspaceRootUri): Resource[IO, LspConnection] =
            MockLspServer
              .resource(Map("initialize" -> Json.obj("capabilities" -> Json.obj())), logger)
              .evalTap(mock => servers.offer(root.value -> mock))
              .flatMap(mock =>
                // The mock stops reading when it is released, which would lose an `exit` still in its pipe.
                Resource.onFinalize(IO.sleep(500.millis)) *>
                  LspConnection.connect(LanguageId.Scala, mock.clientIn, mock.clientOut, root, logger)
              )
              .evalTap(_ => live.update(_ + root.value))
              .onFinalize(live.update(_ - root.value))
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
          .use(_ => test(Running(effects, servers, live, events, notices, asked)))
      yield result
    program.timeout(testTimeout * 2).unsafeRunSync()

  private def methods(messages: List[Json]): List[Option[String]] =
    messages.map(_.hcursor.downField("method").as[String].toOption)

  private def bothOpen(manager: Running): IO[Map[String, MockLspServer]] =
    for
      _      <- manager.offer(LspEffect.FileOpened(oneUri, LanguageId.Scala, RopeText("object One")))
      first  <- manager.nextServer
      _      <- first._2.drainReceived(3).timeout(testTimeout)
      _      <- manager.offer(LspEffect.FileOpened(twoUri, LanguageId.Scala, RopeText("object Two")))
      second <- manager.nextServer
      _      <- second._2.drainReceived(3).timeout(testTimeout)
    yield Map(first, second)

  "Trust This Folder" should "trust the document's folder and say so, leaving running servers alone" in
    running(LspManager.TrustChange("Trusted /workspace-one.", revoked = Nil)) { manager =>
      for
        _       <- bothOpen(manager)
        _       <- manager.offer(LspEffect.TrustFolder(oneUri, LanguageId.Scala))
        notices <- manager.eventually(manager.notices.get)(_.nonEmpty)
        asked   <- manager.asked.get
        live    <- manager.live.get
      yield
        asked shouldBe List(s"trust $oneUri")
        notices.map(_.message) shouldBe List("Trusted /workspace-one.")
        live shouldBe Set(oneRoot, twoRoot)
    }

  "Revoke Folder Trust" should "stop only the revoked folder's server, and leave the other project's running" in
    running(LspManager.TrustChange("Revoked trust for /workspace-one.", revoked = List(Paths.get("/workspace-one")))) {
      manager =>
        for
          servers <- bothOpen(manager)
          _       <- manager.offer(LspEffect.RevokeFolderTrust(oneUri, LanguageId.Scala))
          stopped <- manager.receivedWithin(servers(oneRoot), 2)
          live    <- manager.eventually(manager.live.get)(_ == Set(twoRoot))
          _ <- manager.offer(LspEffect.FileChanged(twoUri, LanguageId.Scala, RopeText("object Two2"), version = 2))
          stillFed <- manager.receivedWithin(servers(twoRoot), 1)
          events   <- manager.eventually(manager.events.get)(_.exists(_.isInstanceOf[LspEvent.LspTrustRevoked]))
          notices  <- manager.notices.get
        yield
          methods(stopped) shouldBe List(Some("shutdown"), Some("exit"))
          live shouldBe Set(twoRoot)
          methods(stillFed) shouldBe List(Some("textDocument/didChange"))
          events should contain(LspEvent.LspTrustRevoked(List(Paths.get("/workspace-one"))))
          notices.map(_.message) shouldBe List("Revoked trust for /workspace-one.")
    }

  it should "stop nothing when nothing was trusted" in
    running(LspManager.TrustChange("/workspace-one is not in a trusted folder.", revoked = Nil)) { manager =>
      for
        _       <- bothOpen(manager)
        _       <- manager.offer(LspEffect.RevokeFolderTrust(oneUri, LanguageId.Scala))
        notices <- manager.eventually(manager.notices.get)(_.nonEmpty)
        _       <- IO.sleep(300.millis)
        live    <- manager.live.get
        events  <- manager.events.get
      yield
        live shouldBe Set(oneRoot, twoRoot)
        events.collect { case revoked: LspEvent.LspTrustRevoked => revoked } shouldBe empty
        notices.map(_.message) shouldBe List("/workspace-one is not in a trusted folder.")
    }
