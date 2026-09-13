package com.serenity.lsp

import cats.effect.std.Queue
import cats.effect.{Deferred, Fiber, IO, Ref, Resource}
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import com.serenity.lsp.model.{SemanticToken, SemanticTokensLegend}
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Split out of `LspManagerSpec` to keep that file under the architecture ratchet's line-count target -- this covers
  * only the `SemanticTokensRequested` effect (#859/#1177 slice 2): sending `textDocument/semanticTokens/full` and
  * applying the decoded response, or not sending it at all when the connection's legend is absent.
  */
class LspManagerSemanticTokensSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspManagerSemanticTokensSpec"))
  private val uri         = "file:///workspace/Foo.scala"
  private val scalaServer = LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)

  final private case class Harness(
      effects: Queue[IO, Option[LspEffect]],
      events: Ref[IO, List[Event]],
      eventApplied: Deferred[IO, Unit],
      connection: LspConnection,
      managerFiber: Fiber[IO, Throwable, Unit]
  ):
    def stop: IO[Unit] =
      effects.offer(None) >> managerFiber.joinWithNever

  private def harness: Resource[IO, Harness] =
    for
      effects      <- Resource.eval(Queue.unbounded[IO, Option[LspEffect]])
      events       <- Resource.eval(Ref.of[IO, List[Event]](Nil))
      eventApplied <- Resource.eval(Deferred[IO, Unit])
      connection   <- Resource.eval(LspConnection.create(LanguageId.Scala, logger))
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
            event => events.update(_ :+ event) >> eventApplied.complete(()).void,
            logger,
            provider
          )
          .start
      )(_.cancel)
    yield Harness(effects, events, eventApplied, connection, managerFiber)

  private def takeMessage(connection: LspConnection): IO[Json] =
    connection.takeOutgoing.flatMap(IO.fromOption(_)(new RuntimeException("Missing LSP message")))

  private def noMessage(connection: LspConnection): IO[Unit] =
    connection.tryTakeOutgoing.map(_ shouldBe None)

  private def requestId(message: Json): Long =
    message.hcursor.downField("id").as[Long].toOption.getOrElse(fail("Request was missing an id"))

  private def response(id: Long, result: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id.asJson, "result" -> result)

  private def open(manager: Harness): IO[Unit] =
    manager.effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, "object Foo"))) >>
      takeMessage(manager.connection).void

  "LspManager" should "send a semanticTokens/full request and emit the decoded tokens when the server supports it" in {
    val legend         = SemanticTokensLegend(tokenTypes = List("keyword"), tokenModifiers = Nil)
    val expectedMethod = "textDocument/semanticTokens/full"
    val expectedToken =
      SemanticToken(line = 0, startCharacter = 0, length = 3, tokenType = "keyword", tokenModifiers = Set.empty)
    runVirtual(
      harness
        .use { manager =>
          for
            _       <- open(manager)
            _       <- manager.connection.recordSemanticTokensLegend(Some(legend))
            _       <- manager.effects.offer(Some(LspEffect.SemanticTokensRequested(uri, LanguageId.Scala)))
            request <- takeMessage(manager.connection)
            _ = request.hcursor.downField("method").as[String].toOption shouldBe Some(expectedMethod)
            _ = request.hcursor
              .downField("params")
              .downField("textDocument")
              .downField("uri")
              .as[String]
              .toOption shouldBe Some(uri)
            _ <- manager.connection.handleIncomingJson(
              response(requestId(request), Json.obj("data" -> List(0, 0, 3, 0, 0).map(_.asJson).asJson))
            )
            _      <- manager.eventApplied.get
            events <- manager.events.get
            _ = events shouldBe List(LspEvent.LspSemanticTokensReceived(uri, List(expectedToken)))
            _ <- manager.stop
          yield succeed
        }
    )
  }

  it should "send no semanticTokens request when the server never declared the capability" in
    runVirtual(
      harness
        .use { manager =>
          for
            _ <- open(manager)
            // No `recordSemanticTokensLegend` call here -- the connection's legend stays `None`, exactly as it would
            // for a real server that never declared `semanticTokensProvider` during its handshake.
            _ <- manager.effects.offer(Some(LspEffect.SemanticTokensRequested(uri, LanguageId.Scala)))
            _ <- noMessage(manager.connection)
            _ <- manager.stop
          yield succeed
        }
    )

  it should "discard a semantic tokens response after its document version changes" in
    runVirtual(
      harness
        .use { manager =>
          for
            _       <- open(manager)
            _       <- manager.connection.recordSemanticTokensLegend(Some(SemanticTokensLegend(List("keyword"), Nil)))
            _       <- manager.effects.offer(Some(LspEffect.SemanticTokensRequested(uri, LanguageId.Scala)))
            request <- takeMessage(manager.connection)
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
            _ <- takeMessage(manager.connection)
            _ <- manager.connection.handleIncomingJson(
              response(requestId(request), Json.obj("data" -> List(0, 0, 3, 0, 0).map(_.asJson).asJson))
            )
            events <- manager.events.get
            _ = events shouldBe Nil
            _ <- manager.stop
          yield succeed
        }
    )
