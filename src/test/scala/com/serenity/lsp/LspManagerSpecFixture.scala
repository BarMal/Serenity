package com.serenity.lsp

import cats.effect.std.Queue
import cats.effect.{Deferred, Fiber, IO, Ref, Resource}
import com.serenity.keystroke.events.Event
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** The harness [[LspManagerSpec]] and [[LspManagerReferenceRenameSpec]] share to drive a real `LspManager` against an
  * in-memory [[LspConnection]] -- split into its own trait so the split between the two spec classes doesn't duplicate
  * this fixture.
  */
private[lsp] trait LspManagerSpecFixture extends Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  protected val logger: Logger[IO]           = LoggerFactory[IO].getLogger(using LoggerName("LspManagerSpec"))
  protected val uri: String                  = "file:///workspace/Foo.scala"
  protected val scalaServer: LspServerConfig = LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)

  protected def resolvedConnection(
    rootUri: WorkspaceRootUri,
    connection: LspConnection,
    release: IO[Unit] = IO.unit
  ): LspManager.ResolvedConnection =
    LspManager.ResolvedConnection(
      LspManager.ConnectionIdentity(rootUri, scalaServer),
      Resource.make(IO.pure(connection))(_ => release)
    )

  final protected case class Harness(
      effects: Queue[IO, Option[LspEffect]],
      events: Ref[IO, List[Event]],
      eventApplied: Deferred[IO, Unit],
      connection: LspConnection,
      released: Deferred[IO, Unit],
      managerFiber: Fiber[IO, Throwable, Unit]
  ):
    def stop: IO[Unit] =
      effects.offer(None) >> managerFiber.joinWithNever

  /** #1357: a bare `.start` here left `managerFiber` (and any LSP request it was supervising, including its own
    * internal 10s `sendRequest` timeout) running on the shared global runtime whenever a test's own IO chain exited
    * before reaching `manager.stop` -- for instance a test body erroring out early under CI scheduling pressure. The
    * leaked fiber would then surface an unrelated `LspRequestTimeout` failure up to 10 seconds later, attributed to
    * whatever was running at that point. `Resource.make` guarantees `managerFiber.cancel` on every exit path (normal,
    * error, or cancellation) rather than only the happy path `manager.stop` covered; cancelling a fiber that already
    * finished via `manager.stop` is a no-op, so this changes nothing on that path.
    */
  protected def harness(serverAvailable: Boolean = true): Resource[IO, Harness] =
    for
      effects      <- Resource.eval(Queue.unbounded[IO, Option[LspEffect]])
      events       <- Resource.eval(Ref.of[IO, List[Event]](Nil))
      eventApplied <- Resource.eval(Deferred[IO, Unit])
      connection   <- Resource.eval(LspConnection.create(LanguageId.Scala, logger))
      released     <- Resource.eval(Deferred[IO, Unit])
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          if serverAvailable then
            IO.pure(
              Some(resolvedConnection(WorkspaceRootUri("file:///workspace"), connection, released.complete(()).void))
            )
          else IO.pure(None)
      managerFiber <- Resource.make(
        LspManager
          .runWithProvider(
            Stream.fromQueueNoneTerminated(effects),
            event => events.update(_ :+ event) >> eventApplied.complete(()).void,
            logger,
            provider
          )
          .start
      )(_.cancel)
    yield Harness(effects, events, eventApplied, connection, released, managerFiber)

  protected def takeMessage(connection: LspConnection): IO[Json] =
    connection.takeOutgoing.flatMap(IO.fromOption(_)(new RuntimeException("Missing LSP message")))

  protected def expectNotification(connection: LspConnection, method: String, uri: String): IO[Unit] =
    takeMessage(connection).map { message =>
      message.hcursor.downField("method").as[String].toOption shouldBe Some(method)
      message.hcursor.downField("params").downField("textDocument").downField("uri").as[String].toOption shouldBe Some(
        uri
      )
    }

  protected def noMessage(connection: LspConnection): IO[Unit] =
    connection.tryTakeOutgoing.map(_ shouldBe None)

  protected def requestId(message: Json): Long =
    message.hcursor.downField("id").as[Long].toOption.getOrElse(fail("Request was missing an id"))

  protected def response(id: Long, result: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id.asJson, "result" -> result)

  protected def open(manager: Harness): IO[Unit] =
    manager.effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, "object Foo"))) >>
      takeMessage(manager.connection).flatMap { message =>
        IO(message.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didOpen"))
      }
