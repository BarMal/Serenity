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

/** `LspManager`'s semantic-tokens request/response wiring: sending `textDocument/semanticTokens/full` only when a
  * connection's captured legend says the server supports it, discarding stale responses, the automatic re-request on
  * `FileOpened`/`FileChanged`, and the three-way `Pending`/`Unavailable`/`Available` split this covers from the manager
  * side (issue #859/#1177's rendering-slice review finding -- `AppState.semanticTokensIndexByBuffer` and
  * `SystemEventReducer` are covered by `SemanticTokensRenderingSpec`). Split out of `LspManagerSpec` to keep both files
  * under the architecture ratchet's file-length target, matching the name `LanguageAwareHighlightingSpec` already
  * references for this test group.
  */
class LspManagerSemanticTokensRenderingSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspManagerSemanticTokensRenderingSpec"))
  private val uri         = "file:///workspace/Foo.scala"
  private val scalaServer = LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)

  private def resolvedConnection(
    rootUri: WorkspaceRootUri,
    connection: LspConnection,
    release: IO[Unit] = IO.unit
  ): LspManager.ResolvedConnection =
    LspManager.ResolvedConnection(
      LspManager.ConnectionIdentity(rootUri, scalaServer),
      Resource.make(IO.pure(connection))(_ => release)
    )

  final private case class Harness(
      effects: Queue[IO, Option[LspEffect]],
      events: Ref[IO, List[Event]],
      eventApplied: Deferred[IO, Unit],
      connection: LspConnection,
      released: Deferred[IO, Unit],
      managerFiber: Fiber[IO, Throwable, Unit]
  ):
    def stop: IO[Unit] =
      effects.offer(None) >> managerFiber.joinWithNever

  /** See `LspManagerSpec.harness`'s #1357 note: `Resource.make` here guarantees `managerFiber.cancel` on every exit
    * path, not only the happy path through `manager.stop`.
    */
  private def harness: Resource[IO, Harness] =
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
          IO.pure(
            Some(resolvedConnection(WorkspaceRootUri("file:///workspace"), connection, released.complete(()).void))
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
    yield Harness(effects, events, eventApplied, connection, released, managerFiber)

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
      takeMessage(manager.connection).flatMap { message =>
        IO(message.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didOpen"))
      }

  "LspManager" should "send a semanticTokens/full request and emit the decoded tokens when the server supports it" in {
    val legend = SemanticTokensLegend(tokenTypes = List("keyword"), tokenModifiers = Nil)
    runVirtual(
      harness
        .use { manager =>
          for
            _       <- open(manager)
            _       <- manager.connection.recordSemanticTokensLegend(Some(legend))
            _       <- manager.effects.offer(Some(LspEffect.SemanticTokensRequested(uri, LanguageId.Scala)))
            request <- takeMessage(manager.connection)
            _ = request.hcursor.downField("method").as[String].toOption shouldBe Some(
              "textDocument/semanticTokens/full"
            )
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
            _ = events shouldBe List(
              LspEvent.LspSemanticTokensReceived(
                uri,
                List(
                  SemanticToken(
                    line = 0,
                    startCharacter = 0,
                    length = 3,
                    tokenType = "keyword",
                    tokenModifiers = Set.empty
                  )
                )
              )
            )
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

  it should "automatically request semantic tokens after a successful didOpen when the server supports them" in
    runVirtual(
      harness
        .use { manager =>
          for
            _       <- manager.connection.recordSemanticTokensLegend(Some(SemanticTokensLegend(List("keyword"), Nil)))
            _       <- open(manager) // consumes the didOpen message
            request <- takeMessage(manager.connection)
            _ = request.hcursor.downField("method").as[String].toOption shouldBe Some(
              "textDocument/semanticTokens/full"
            )
            _ <- manager.stop
          yield succeed
        }
    )

  it should "not request semantic tokens after didOpen when the server never declared the capability" in
    runVirtual(
      harness
        .use { manager =>
          for
            _ <- open(manager) // no recordSemanticTokensLegend call -- the connection's legend stays None
            _ <- noMessage(manager.connection)
            _ <- manager.stop
          yield succeed
        }
    )

  it should "automatically request semantic tokens after a successful didChange when the server supports them" in
    runVirtual(
      harness
        .use { manager =>
          for
            _ <- manager.connection.recordSemanticTokensLegend(Some(SemanticTokensLegend(List("keyword"), Nil)))
            _ <- open(manager)
            openTokensRequest <- takeMessage(manager.connection) // the didOpen-triggered semanticTokens request
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
            // The didOpen-triggered request above is still tracked as in-flight (nothing prunes a completed fiber's
            // entry proactively), so FileChanged's invalidateDocument cancels it first -- the same cancel-before-
            // continuing behavior `LspManagerSpec` covers for hover, now also reachable through semantic tokens.
            cancel <- takeMessage(manager.connection)
            _ = cancel.hcursor.downField("method").as[String].toOption shouldBe Some("$/cancelRequest")
            _ = cancel.hcursor.downField("params").downField("id").as[Long].toOption shouldBe
              openTokensRequest.hcursor.downField("id").as[Long].toOption
            change <- takeMessage(manager.connection)
            _ = change.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didChange")
            request <- takeMessage(manager.connection)
            _ = request.hcursor.downField("method").as[String].toOption shouldBe Some(
              "textDocument/semanticTokens/full"
            )
            _ <- manager.stop
          yield succeed
        }
    )

  it should "not emit any semantic tokens event while a request to a connected, capable server is still pending" in
    runVirtual(
      harness
        .use { manager =>
          for
            _ <- manager.connection.recordSemanticTokensLegend(
              Some(SemanticTokensLegend(tokenTypes = List("keyword"), tokenModifiers = Nil))
            )
            _       <- open(manager)
            request <- takeMessage(manager.connection)
            _ = request.hcursor.downField("method").as[String].toOption shouldBe Some(
              "textDocument/semanticTokens/full"
            )
            // The request is on the wire but nothing has answered it yet -- this is exactly the round-trip window
            // the #859/#1177 rendering-slice review flagged: no event at all must reach the reducer here, so
            // AppState.semanticTokensIndexByBuffer stays Pending (not Unavailable) for the length of it.
            eventsWhilePending <- manager.events.get
            _ = eventsWhilePending shouldBe Nil
            _ <- manager.connection.handleIncomingJson(
              response(requestId(request), Json.obj("data" -> List(0, 0, 3, 0, 0).map(_.asJson).asJson))
            )
            _              <- manager.eventApplied.get
            eventsOnAnswer <- manager.events.get
            _ = eventsOnAnswer shouldBe List(
              LspEvent.LspSemanticTokensReceived(
                uri,
                List(
                  SemanticToken(
                    line = 0,
                    startCharacter = 0,
                    length = 3,
                    tokenType = "keyword",
                    tokenModifiers = Set.empty
                  )
                )
              )
            )
            _ <- manager.stop
          yield succeed
        }
    )

  it should "emit LspSemanticTokensUnavailable, not a hover message, when no server exists for this document's language" in {
    val program = for
      effects <- Queue.unbounded[IO, Option[LspEffect]]
      events  <- Ref.of[IO, List[Event]](Nil)
      applied <- Deferred[IO, Unit]
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] = IO.pure(None)
      result <- Resource
        .make(
          LspManager
            .runWithProvider(
              Stream.fromQueueNoneTerminated(effects),
              event => events.update(_ :+ event) >> applied.complete(()).void,
              logger,
              provider
            )
            .start
        )(_.cancel)
        .use { managerFiber =>
          for
            _    <- effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, "object Foo")))
            _    <- applied.get
            seen <- events.get
            _    <- effects.offer(None)
            _    <- managerFiber.joinWithNever
          yield seen shouldBe List(LspEvent.LspSemanticTokensUnavailable(uri))
        }
    yield result

    runVirtual(program)
  }

  it should "emit LspSemanticTokensUnavailable for a directly-requested SemanticTokensRequested effect with no connection" in {
    val program = for
      effects <- Queue.unbounded[IO, Option[LspEffect]]
      events  <- Ref.of[IO, List[Event]](Nil)
      applied <- Deferred[IO, Unit]
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] = IO.pure(None)
      result <- Resource
        .make(
          LspManager
            .runWithProvider(
              Stream.fromQueueNoneTerminated(effects),
              event => events.update(_ :+ event) >> applied.complete(()).void,
              logger,
              provider
            )
            .start
        )(_.cancel)
        .use { managerFiber =>
          for
            _    <- effects.offer(Some(LspEffect.SemanticTokensRequested(uri, LanguageId.Scala)))
            _    <- applied.get
            seen <- events.get
            _    <- effects.offer(None)
            _    <- managerFiber.joinWithNever
          yield seen shouldBe List(LspEvent.LspSemanticTokensUnavailable(uri))
        }
    yield result

    runVirtual(program)
  }
