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
  * `sendDidChange`'s dispatch between a range-based diff and a full-text notification based on the connection's
  * negotiated sync kind (see `LspManager.sendDidChange`). The `documentTexts` mirror's own advance-on-failure
  * behavior is covered separately in `LspManagerDidChangeMirrorSpec`.
  */
class LspManagerIncrementalSyncSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspManagerIncrementalSyncSpec"))
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

  "LspManager" should "send a range-based didChange when the connection negotiated incremental sync" in
    runVirtual(
      harness
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.connection.setSyncKind(TextDocumentSyncKind.Incremental)
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
            change <- takeMessage(manager.connection)
            _ = change.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didChange")
            contentChange = change.hcursor.downField("params").downField("contentChanges").downArray
            _             = contentChange.downField("range").succeeded shouldBe true
            _             = contentChange.downField("rangeLength").as[Int].toOption shouldBe Some(0)
            _             = contentChange.downField("text").as[String].toOption shouldBe Some("2")
            _ <- manager.stop
          yield succeed
        }
    )

  it should "keep sending full-text didChange when the connection has not negotiated incremental sync" in
    runVirtual(
      harness
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
            change <- takeMessage(manager.connection)
            contentChange = change.hcursor.downField("params").downField("contentChanges").downArray
            _             = contentChange.downField("range").succeeded shouldBe false
            _             = contentChange.downField("text").as[String].toOption shouldBe Some("object Foo2")
            _ <- manager.stop
          yield succeed
        }
    )

  it should "diff incremental didChange against the text from the most recent open, not a stale one" in
    runVirtual(
      harness
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(Some(LspEffect.FileClosed(uri, LanguageId.Scala)))
            _ <- takeMessage(manager.connection) // didClose
            _ <- manager.effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, "object Reopened")))
            _ <- takeMessage(manager.connection) // didOpen
            _ <- manager.connection.setSyncKind(TextDocumentSyncKind.Incremental)
            _ <- manager.effects.offer(
              Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Reopened2", version = 2))
            )
            change <- takeMessage(manager.connection)
            contentChange = change.hcursor.downField("params").downField("contentChanges").downArray
            _             = contentChange.downField("rangeLength").as[Int].toOption shouldBe Some(0)
            _             = contentChange.downField("text").as[String].toOption shouldBe Some("2")
            _ <- manager.stop
          yield succeed
        }
    )
