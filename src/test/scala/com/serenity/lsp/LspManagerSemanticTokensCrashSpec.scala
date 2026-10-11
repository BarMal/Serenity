package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.serenity.keystroke.events.Event
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.Diagnostic
import com.serenity.testkit.RopeText
import fs2.Stream
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A semantic tokens refresh waiting out its delay is dropped when the document changes while its server is down, so it
  * cannot start the server the restart policy is holding back.
  */
class LspManagerSemanticTokensCrashSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  private val workspace = WorkspaceRootUri("file:///workspace")

  private val slowRestarts = LspSupervisionPolicy.Default.copy(initialBackoff = 4.seconds)

  "LspManager" should "not start a server for a refresh that was waiting when the document changed with its server down" in {
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
              .runWithProvider(
                Stream.fromQueueNoneTerminated(effects),
                events.offer,
                logger,
                provider,
                slowRestarts,
                semanticTokensDebounce = 1.second
              )
              .start
          )(_.cancel)
          .use { _ =>
            for
              _      <- effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, RopeText("object Foo"))))
              first  <- servers.take.timeout(20.seconds)
              _      <- first.drainReceived(3)
              _      <- effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, RopeText("object Foo2"), 2)))
              _      <- first.takeReceived
              _      <- first.shutdown()
              _      <- IO.sleep(300.millis)
              _      <- effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, RopeText("object Foo3"), 3)))
              _      <- IO.sleep(1500.millis)
              second <- servers.take.timeout(500.millis).attempt
            yield second.isLeft
          }
      yield result
    program.timeout(60.seconds).unsafeRunSync() shouldBe true
  }
