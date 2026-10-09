package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.Diagnostic
import com.serenity.testkit.RopeText
import fs2.Stream
import io.circe.Json
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `ReleaseAll` is how the editor lets every language server go when the workspace leaves code mode: each server is
  * asked to `shutdown` and `exit` and its process released, nothing is left to restart it, and a document opened
  * afterwards starts afresh.
  */
class LspManagerReleaseAllSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  private val testTimeout = 20.seconds
  private val oneUri      = "file:///workspace-one/One.scala"
  private val twoUri      = "file:///workspace-two/Two.scala"

  final private case class Running(
      effects: Queue[IO, Option[LspEffect]],
      servers: Queue[IO, MockLspServer],
      live: Ref[IO, Int]
  ):
    def offer(effect: LspEffect): IO[Unit] = effects.offer(Some(effect))

    def nextServer: IO[MockLspServer] = servers.take.timeout(testTimeout)

    def serverWithin(wait: FiniteDuration): IO[Option[MockLspServer]] =
      servers.take.timeout(wait).attempt.map(_.toOption)

    /** What `server` was sent, up to `count` messages, within a bound that keeps a missing message an assertion. */
    def receivedWithin(server: MockLspServer, count: Int): IO[List[Json]] =
      Stream.repeatEval(server.takeReceived).take(count.toLong).interruptAfter(10.seconds).compile.toList

    /** How many servers are still running once none is, or `wait` has passed. */
    def serversRunningAfter(wait: FiniteDuration): IO[Int] =
      (IO.sleep(20.millis) >> live.get).iterateUntil(_ == 0).timeoutTo(wait, live.get)

  /** Each workspace directory gets its own server, started in-memory and handed to the test as it starts. */
  private def running(policy: LspSupervisionPolicy = LspSupervisionPolicy.Default, hangUpOn: Set[String] = Set.empty)(
    test: Running => IO[Assertion]
  ): Assertion =
    val program =
      for
        effects <- Queue.unbounded[IO, Option[LspEffect]]
        servers <- Queue.unbounded[IO, MockLspServer]
        live    <- Ref.of[IO, Int](0)
        provider = new LspManager.ConnectionProvider:
          def resolve(
            languageId: LanguageId,
            fileUri: DocumentUri,
            onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit]
          ): IO[Option[LspManager.ResolvedConnection]] =
            val root = WorkspaceRootUri(fileUri.value.substring(0, fileUri.value.lastIndexOf('/')))
            IO.pure(Some(LspManager.ResolvedConnection(LspManager.ConnectionIdentity(root, scalaServer), server(root))))

          private def server(root: WorkspaceRootUri): Resource[IO, LspConnection] =
            MockLspServer
              .resource(Map("initialize" -> Json.obj("capabilities" -> Json.obj())), logger, hangUpOn)
              .evalTap(servers.offer)
              .flatMap(mock =>
                // The mock stops reading when it is released, which would lose an `exit` still in its pipe.
                Resource.onFinalize(IO.sleep(500.millis)) *>
                  LspConnection.connect(LanguageId.Scala, mock.clientIn, mock.clientOut, root, logger)
              )
              .evalTap(_ => live.update(_ + 1))
              .onFinalize(live.update(_ - 1))
        result <- Resource
          .make(
            LspManager
              .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider, policy)
              .start
          )(_.cancel)
          .use(_ => test(Running(effects, servers, live)))
      yield result
    program.timeout(testTimeout * 2).unsafeRunSync()

  private def methods(messages: List[Json]): List[Option[String]] =
    messages.map(_.hcursor.downField("method").as[String].toOption)

  private def openedDocument(didOpen: Json): (Option[String], Option[Int]) =
    val document = didOpen.hcursor.downField("params").downField("textDocument")
    (document.downField("text").as[String].toOption, document.downField("version").as[Int].toOption)

  private def changedVersion(didChange: Json): Option[Int] =
    didChange.hcursor.downField("params").downField("textDocument").downField("version").as[Int].toOption

  private def handshake(server: MockLspServer): IO[List[Json]] = server.drainReceived(3).timeout(testTimeout)

  "ReleaseAll" should "ask every running server to shut down and exit, and release its process" in
    running() { manager =>
      for
        _         <- manager.offer(LspEffect.FileOpened(oneUri, LanguageId.Scala, RopeText("object One")))
        first     <- manager.nextServer
        _         <- handshake(first)
        _         <- manager.offer(LspEffect.FileOpened(twoUri, LanguageId.Scala, RopeText("object Two")))
        second    <- manager.nextServer
        _         <- handshake(second)
        _         <- manager.offer(LspEffect.ReleaseAll)
        firstly   <- manager.receivedWithin(first, 2)
        secondly  <- manager.receivedWithin(second, 2)
        remaining <- manager.serversRunningAfter(3.seconds)
      yield
        methods(firstly) shouldBe List(Some("shutdown"), Some("exit"))
        methods(secondly) shouldBe List(Some("shutdown"), Some("exit"))
        remaining shouldBe 0
    }

  it should "start a new server at version 1 for a document opened afterwards, and sync it from there" in
    running() { manager =>
      for
        _        <- manager.offer(LspEffect.FileOpened(oneUri, LanguageId.Scala, RopeText("object One")))
        first    <- manager.nextServer
        _        <- handshake(first)
        _        <- manager.offer(LspEffect.FileChanged(oneUri, LanguageId.Scala, RopeText("object One2"), version = 2))
        _        <- first.takeReceived
        _        <- manager.offer(LspEffect.ReleaseAll)
        _        <- manager.serversRunningAfter(3.seconds)
        _        <- manager.offer(LspEffect.FileOpened(oneUri, LanguageId.Scala, RopeText("object One3")))
        second   <- manager.serverWithin(5.seconds)
        reopened <- second.traverse(handshake)
        _        <- manager.offer(LspEffect.FileChanged(oneUri, LanguageId.Scala, RopeText("object One4"), version = 2))
        changed  <- second.traverse(server => manager.receivedWithin(server, 1))
      yield
        reopened.flatMap(_.lift(2)).map(openedDocument) shouldBe Some((Some("object One3"), Some(1)))
        changed.flatMap(_.headOption).flatMap(changedVersion) shouldBe Some(2)
    }

  it should "leave nothing to restart a server that was waiting out a backoff after crashing" in
    running(LspSupervisionPolicy.Default.copy(initialBackoff = 1500.millis)) { manager =>
      for
        _       <- manager.offer(LspEffect.FileOpened(oneUri, LanguageId.Scala, RopeText("object One")))
        first   <- manager.nextServer
        _       <- handshake(first)
        _       <- first.shutdown()
        _       <- manager.serversRunningAfter(3.seconds)
        _       <- manager.offer(LspEffect.ReleaseAll)
        restart <- manager.serverWithin(3.seconds)
      yield restart shouldBe None
    }

  it should "do nothing when no server is running, however often it is sent" in
    running() { manager =>
      for
        _         <- manager.offer(LspEffect.ReleaseAll)
        _         <- manager.offer(LspEffect.ReleaseAll)
        started   <- manager.serverWithin(500.millis)
        _         <- manager.offer(LspEffect.FileOpened(oneUri, LanguageId.Scala, RopeText("object One")))
        server    <- manager.nextServer
        handshook <- handshake(server)
      yield
        started shouldBe None
        methods(handshook) shouldBe List(Some("initialize"), Some("initialized"), Some("textDocument/didOpen"))
    }

  it should "release a server that hangs up instead of answering shutdown, without counting it as a crash" in
    running(hangUpOn = Set("shutdown")) { manager =>
      for
        _         <- manager.offer(LspEffect.FileOpened(oneUri, LanguageId.Scala, RopeText("object One")))
        first     <- manager.nextServer
        _         <- handshake(first)
        _         <- manager.offer(LspEffect.ReleaseAll)
        remaining <- manager.serversRunningAfter(5.seconds)
        restart   <- manager.serverWithin(1.second)
      yield
        remaining shouldBe 0
        restart shouldBe None
    }
