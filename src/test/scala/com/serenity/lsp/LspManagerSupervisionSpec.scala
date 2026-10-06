package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity}
import com.serenity.testkit.RopeText
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.Stream
import io.circe.Json
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How `LspManager` keeps servers alive and lets them go: a server that dies is restarted with its documents reopened
  * (#1847), and a server whose last document closed is shut down after a grace period (#1844).
  */
class LspManagerSupervisionSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  private val workspace   = WorkspaceRootUri("file:///workspace")
  private val testTimeout = 20.seconds
  private val grace       = LspSupervisionPolicy.Default.idleShutdownGrace

  private val fastRestarts = LspSupervisionPolicy.Default.copy(initialBackoff = 10.millis)

  final private case class Supervised(
      effects: Queue[IO, Option[LspEffect]],
      servers: Queue[IO, MockLspServer],
      events: Queue[IO, Event]
  ):
    def nextServer: IO[MockLspServer] = servers.take.timeout(testTimeout)

  /** Every connection the manager starts gets a fresh in-memory server over pipes, handed to the test as it starts. */
  private def supervised(policy: LspSupervisionPolicy)(test: Supervised => IO[Assertion]): Assertion =
    val program =
      for
        effects <- Queue.unbounded[IO, Option[LspEffect]]
        servers <- Queue.unbounded[IO, MockLspServer]
        events  <- Queue.unbounded[IO, Event]
        serverProcess =
          MockLspServer
            .resource(Map("initialize" -> Json.obj("capabilities" -> Json.obj())), logger)
            .evalTap(servers.offer)
            .flatMap(server =>
              LspConnection.connect(LanguageId.Scala, server.clientIn, server.clientOut, workspace, logger)
            )
        provider = new LspManager.ConnectionProvider:
          def resolve(
            languageId: LanguageId,
            fileUri: DocumentUri,
            onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit]
          ): IO[Option[LspManager.ResolvedConnection]] =
            IO.pure(
              Some(LspManager.ResolvedConnection(LspManager.ConnectionIdentity(workspace, scalaServer), serverProcess))
            )
        result <- Resource
          .make(
            LspManager
              .runWithProvider(Stream.fromQueueNoneTerminated(effects), events.offer, logger, provider, policy)
              .start
          )(_.cancel)
          .use(_ => test(Supervised(effects, servers, events)))
      yield result
    program.timeout(testTimeout * 2).unsafeRunSync()

  private def methods(messages: List[Json]): List[Option[String]] =
    messages.map(_.hcursor.downField("method").as[String].toOption)

  private def openedText(didOpen: Json): (Option[String], Option[Int]) =
    val document = didOpen.hcursor.downField("params").downField("textDocument")
    (document.downField("text").as[String].toOption, document.downField("version").as[Int].toOption)

  private val handshakeAndOpen = List(Some("initialize"), Some("initialized"), Some("textDocument/didOpen"))

  "LspManager" should "restart a server that dies and reopen its documents with their latest text" in
    supervised(fastRestarts) { manager =>
      for
        _     <- manager.effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, RopeText("object Foo"))))
        first <- manager.nextServer
        firstMessages <- first.drainReceived(3)
        _ <- manager.effects.offer(
          Some(LspEffect.FileChanged(uri, LanguageId.Scala, RopeText("object Foo2"), version = 2))
        )
        _              <- first.takeReceived
        _              <- first.shutdown()
        second         <- manager.nextServer
        secondMessages <- second.drainReceived(3).timeout(testTimeout)
      yield
        methods(firstMessages) shouldBe handshakeAndOpen
        methods(secondMessages) shouldBe handshakeAndOpen
        openedText(secondMessages(2)) shouldBe (Some("object Foo2"), Some(2))
    }

  it should "stop restarting a server that keeps dying and leave a warning on its documents" in
    supervised(fastRestarts.copy(maxRestarts = 2)) { manager =>
      val crashOnce = manager.nextServer.flatMap(server => server.drainReceived(3) >> server.shutdown())
      for
        _ <- manager.effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, RopeText("object Foo"))))
        _ <- crashOnce
        _ <- crashOnce
        _ <- crashOnce
        warning <- Stream
          .fromQueueUnterminated(manager.events)
          .collectFirst { case LspEvent.LspDiagnosticsReceived(`uri`, List(diagnostic)) => diagnostic }
          .compile
          .lastOrError
          .timeout(testTimeout)
        anotherStart <- manager.servers.take.timeout(500.millis).attempt
      yield
        warning.severity shouldBe Some(DiagnosticSeverity.Warning)
        warning.message should include("stopped after crashing 3 times")
        anotherStart.isLeft shouldBe true
    }

  it should "shut an idle server down and exit it once its last document has stayed closed for the grace period" in
    runVirtual(
      harness().use { manager =>
        for
          _             <- open(manager)
          _             <- manager.effects.offer(Some(LspEffect.FileClosed(uri, LanguageId.Scala)))
          _             <- expectNotification(manager.connection, "textDocument/didClose", uri)
          _             <- IO.sleep(grace - 1.second)
          earlyMessage  <- manager.connection.tryTakeOutgoing
          releasedEarly <- manager.released.tryGet
          shutdown      <- takeMessage(manager.connection).timeout(grace)
          _             <- manager.connection.handleIncomingJson(response(requestId(shutdown), Json.Null))
          exit          <- takeMessage(manager.connection)
          _             <- manager.released.get.timeout(grace)
          _             <- manager.stop
        yield
          earlyMessage shouldBe None
          releasedEarly shouldBe None
          methods(List(shutdown, exit)) shouldBe List(Some("shutdown"), Some("exit"))
          shutdown.hcursor.downField("params").succeeded shouldBe false
      }
    )

  it should "keep a server whose document reopens within the grace period" in
    runVirtual(
      harness().use { manager =>
        for
          _        <- open(manager)
          _        <- manager.effects.offer(Some(LspEffect.FileClosed(uri, LanguageId.Scala)))
          _        <- expectNotification(manager.connection, "textDocument/didClose", uri)
          _        <- IO.sleep(grace / 2)
          _        <- open(manager)
          _        <- IO.sleep(grace * 2)
          message  <- manager.connection.tryTakeOutgoing
          released <- manager.released.tryGet
          _        <- manager.stop
        yield
          message shouldBe None
          released shouldBe None
      }
    )

  it should "keep serving other workspaces while one server has stopped reading" in {
    val stalledUri = "file:///workspace-one/Stalled.scala"
    val healthyUri = "file:///workspace-two/Healthy.scala"
    val program =
      for
        effects <- Queue.unbounded[IO, Option[LspEffect]]
        stalled <- LspConnection.create(LanguageId.Scala, logger)
        healthy <- LspConnection.create(LanguageId.Scala, logger)
        provider = new LspManager.ConnectionProvider:
          def resolve(
            languageId: LanguageId,
            fileUri: DocumentUri,
            onDiagnostics: (DocumentUri, List[Diagnostic]) => IO[Unit]
          ): IO[Option[LspManager.ResolvedConnection]] =
            IO.pure(
              Some(
                if fileUri.value == stalledUri then
                  resolvedConnection(WorkspaceRootUri("file:///workspace-one"), stalled)
                else resolvedConnection(WorkspaceRootUri("file:///workspace-two"), healthy)
              )
            )
        result <- Resource
          .make(
            LspManager.runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider).start
          )(_.cancel)
          .use { managerFiber =>
            val edits = (2 to 301).toList.map(version =>
              Some(LspEffect.FileChanged(stalledUri, LanguageId.Scala, RopeText(s"object Stalled$version"), version))
            )
            for
              _ <- effects.offer(Some(LspEffect.FileOpened(stalledUri, LanguageId.Scala, RopeText("object Stalled"))))
              _ <- edits.traverse_(effects.offer)
              _ <- effects.offer(Some(LspEffect.FileOpened(healthyUri, LanguageId.Scala, RopeText("object Healthy"))))
              _ <- expectNotification(healthy, "textDocument/didOpen", healthyUri).timeout(testTimeout)
              _ <- effects.offer(None)
              _ <- managerFiber.joinWithNever
            yield succeed
          }
      yield result

    runVirtual(program)
  }
