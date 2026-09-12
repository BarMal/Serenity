package com.serenity.lsp

import cats.effect.std.Queue
import cats.effect.{Fiber, IO, Ref, Resource}
import com.serenity.keystroke.events.Event
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import com.serenity.lsp.model.TextDocumentSyncKind
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.Stream
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Split out of `LspManagerSpec` to keep that file under the architecture ratchet's line-count target -- this covers
  * only the `documentTexts` mirror's behavior when a didChange notification fails to send (see
  * `LspManager.sendDidChange`).
  */
class LspManagerDidChangeMirrorSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspManagerDidChangeMirrorSpec"))
  private val uri         = "file:///workspace/Foo.scala"
  private val scalaServer = LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)

  final private case class Harness(
      effects: Queue[IO, Option[LspEffect]],
      connection: LspConnection,
      managerFiber: Fiber[IO, Throwable, Unit]
  ):
    def stop: IO[Unit] =
      effects.offer(None) >> managerFiber.joinWithNever

  private def harness: Resource[IO, Harness] =
    for
      effects    <- Resource.eval(Queue.unbounded[IO, Option[LspEffect]])
      events     <- Resource.eval(Ref.of[IO, List[Event]](Nil))
      connection <- Resource.eval(LspConnection.create(LanguageId.Scala, logger))
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          IO.pure(
            Some(
              LspManager.ResolvedConnection(
                LspManager.ConnectionIdentity(WorkspaceRootUri("file:///workspace"), scalaServer),
                Resource.eval(IO.pure(connection))
              )
            )
          )
      managerFiber <- Resource.make(
        LspManager
          .runWithProvider(
            Stream.fromQueueNoneTerminated(effects),
            event => events.update(_ :+ event),
            logger,
            provider
          )
          .start
      )(_.cancel)
    yield Harness(effects, connection, managerFiber)

  private def takeMessage(connection: LspConnection): IO[Json] =
    connection.takeOutgoing.flatMap(IO.fromOption(_)(new RuntimeException("Missing LSP message")))

  private def open(manager: Harness): IO[Unit] =
    manager.effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, "object Foo"))) >>
      takeMessage(manager.connection).void

  "LspManager" should "keep its previous-text mirror unchanged when a didChange notification fails to send" in
    runVirtual(
      harness
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.connection.setSyncKind(TextDocumentSyncKind.Incremental)
            _ <- manager.connection.failNextNotification
            // This didChange fails to send (simulated) -- the server never saw "object Foo1", so the manager's
            // documentTexts mirror must not advance to it, or the next diff below would be computed against text
            // the server was never told about.
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo1", version = 2)))
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 3)))
            change <- takeMessage(manager.connection)
            contentChange = change.hcursor.downField("params").downField("contentChanges").downArray
            _             = contentChange.downField("rangeLength").as[Int].toOption shouldBe Some(0)
            _             = contentChange.downField("text").as[String].toOption shouldBe Some("2")
            _ <- manager.stop
          yield succeed
        }
    )
