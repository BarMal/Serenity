package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.{Deferred, Fiber, IO, Ref, Resource}
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.{DocumentUri, LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import com.serenity.state.models.CursorPosition
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class LspManagerSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspManagerSpec"))
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

  /** #1357: a bare `.start` here left `managerFiber` (and any LSP request it was supervising, including its own
    * internal 10s `sendRequest` timeout) running on the shared global runtime whenever a test's own IO chain exited
    * before reaching `manager.stop` -- for instance a test body erroring out early under CI scheduling pressure. The
    * leaked fiber would then surface an unrelated `LspRequestTimeout` failure up to 10 seconds later, attributed to
    * whatever was running at that point. `Resource.make` guarantees `managerFiber.cancel` on every exit path (normal,
    * error, or cancellation) rather than only the happy path `manager.stop` covered; cancelling a fiber that already
    * finished via `manager.stop` is a no-op, so this changes nothing on that path.
    */
  private def harness(serverAvailable: Boolean = true): Resource[IO, Harness] =
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

  private def takeMessage(connection: LspConnection): IO[Json] =
    connection.takeOutgoing.flatMap(IO.fromOption(_)(new RuntimeException("Missing LSP message")))

  private def expectNotification(connection: LspConnection, method: String, uri: String): IO[Unit] =
    takeMessage(connection).map { message =>
      message.hcursor.downField("method").as[String].toOption shouldBe Some(method)
      message.hcursor.downField("params").downField("textDocument").downField("uri").as[String].toOption shouldBe Some(
        uri
      )
    }

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

  "LspManager" should "send document changes while a hover response is pending" in
    runVirtual(
      harness()
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(
              Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, CursorPosition(0, 1)))
            )
            hover <- takeMessage(manager.connection)
            _ = hover.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/hover")
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
            // The change supersedes the in-flight hover, so the manager cancels it -- and cancelling now tells the
            // server to stop (#1285) before the new work goes out, rather than leaving it computing an answer nobody
            // will read.
            cancel <- takeMessage(manager.connection)
            _ = cancel.hcursor.downField("method").as[String].toOption shouldBe Some("$/cancelRequest")
            _ = cancel.hcursor.downField("params").downField("id").as[Long].toOption shouldBe Some(requestId(hover))
            change <- takeMessage(manager.connection)
            _ = change.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didChange")
            _ <- manager.stop
          yield succeed
        }
    )

  it should "discard a definition response after its document version changes" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness()
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(
              Some(LspEffect.DefinitionRequested(uri, LanguageId.Scala, 0, 1, anchor, "Foo"))
            )
            request <- takeMessage(manager.connection)
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
            _ <- takeMessage(manager.connection)
            pending <- manager.connection.pendingRequestCount
            _ = pending shouldBe 0
            _ <- manager.connection.handleIncomingJson(
              response(
                requestId(request),
                Json.obj(
                  "uri" -> uri.asJson,
                  "range" -> Json.obj(
                    "start" -> Json.obj("line" -> 0.asJson, "character" -> 0.asJson),
                    "end"   -> Json.obj("line" -> 0.asJson, "character" -> 3.asJson)
                  )
                )
              )
            )
            events <- manager.events.get
            _ = events shouldBe Nil
            _ <- manager.stop
          yield succeed
        }
    )
  }

  it should "cancel a superseded hover request and retain only the current anchor" in {
    val firstAnchor  = CursorPosition(0, 1)
    val secondAnchor = CursorPosition(0, 2)
    runVirtual(
      harness()
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, firstAnchor)))
            firstRequest <- takeMessage(manager.connection)
            _ <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 2, secondAnchor)))
            // A second hover always supersedes an in-flight one (unconditionally, unlike definition requests), so the
            // manager cancels the first before sending the second -- the same cancel-before-continuing shape as the
            // FileChanged case above, and in the same order on the wire.
            cancel <- takeMessage(manager.connection)
            _ = cancel.hcursor.downField("method").as[String].toOption shouldBe Some("$/cancelRequest")
            _ = cancel.hcursor.downField("params").downField("id").as[Long].toOption shouldBe Some(
              requestId(firstRequest)
            )
            secondRequest <- takeMessage(manager.connection)
            _ <- manager.connection.handleIncomingJson(
              response(requestId(firstRequest), Json.obj("contents" -> "stale".asJson))
            )
            _ <- manager.connection.handleIncomingJson(
              response(requestId(secondRequest), Json.obj("contents" -> "current".asJson))
            )
            _      <- manager.eventApplied.get
            events <- manager.events.get
            _ = events shouldBe List(LspEvent.LspHoverReceived("current", secondAnchor))
            _ <- manager.stop
          yield succeed
        }
    )
  }

  it should "cancel pending request fibers before releasing connections on shutdown" in
    runVirtual(
      harness()
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(
              Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, CursorPosition(0, 1)))
            )
            _        <- takeMessage(manager.connection)
            _        <- manager.stop
            pending  <- manager.connection.pendingRequestCount
            released <- manager.released.tryGet
          yield
            pending shouldBe 0
            released shouldBe Some(())
        }
    )

  it should "cancel a pending request instead of leaking it when the caller's IO chain is interrupted" in {
    // Fiber#cancel doesn't return until the cancelled fiber has actually finished unwinding, so checking
    // `pendingRequestCount` only after `harness.use` itself returns (rather than from inside its body) is what
    // proves `Resource.make(...)(_.cancel)` really ran -- not merely that the internal 200ms timeout below fired.
    val program: IO[Int] =
      for
        connectionRef <- Ref.of[IO, Option[LspConnection]](None)
        _ <- harness().use { manager =>
          connectionRef.set(Some(manager.connection)) >>
            (for
              _ <- open(manager)
              _ <- manager.effects.offer(
                Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, CursorPosition(0, 1)))
              )
              // Consume the outgoing hover request so it's genuinely pending on the connection, then hang --
              // standing in for any test step slow enough to blow its own deadline under CI load, before ever
              // reaching `stop`.
              _ <- takeMessage(manager.connection)
              _ <- IO.never
            yield ()).timeout(200.millis).attempt.void
        }
        connection <- connectionRef.get.map(_.getOrElse(fail("harness never ran")))
        pending    <- connection.pendingRequestCount
      yield pending

    val pendingAfterInterruption = runVirtual(program)
    pendingAfterInterruption shouldBe 0
  }

  it should "not block a later hover on an outstanding, never-completing completion request" in
    // #1441: CompletionRequested used to be handled inline inside handleEffect, so a completion response that never
    // arrives (or is simply slow) would stall `effects.evalMap(handleEffect).compile.drain` and every LSP effect
    // queued behind it. Forking completion through `startRequest`/`supervisor.supervise` -- the same path hover and
    // definition already use -- means the sequential effects loop moves on to the next effect as soon as the
    // completion request is sent, without waiting for its response.
    runVirtual(
      harness()
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(
              Some(LspEffect.CompletionRequested(uri, LanguageId.Scala, 0, 1, CursorPosition(0, 1)))
            )
            completion <- takeMessage(manager.connection)
            _ = completion.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/completion")
            // No response is ever sent for the completion request above -- it stays pending forever.
            _ <- manager.effects.offer(
              Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 2, CursorPosition(0, 2)))
            )
            hover <- takeMessage(manager.connection)
            _ = hover.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/hover")
            _ <- manager.stop
          yield succeed
        }
    )

  // #1508: the "no server available" fallback in `startRequest` used to hardcode `LspHoverReceived` regardless of
  // which request kind was actually being made. These three cover the fallback's kind-appropriate event for each of
  // the request kinds that exist today (Hover, Completion, Definition); a semantic-tokens case will need the same
  // treatment once #1506 (currently unmerged) introduces that request kind.
  it should "emit an explanatory hover message when no LSP server is available" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness(serverAvailable = false)
        .use { manager =>
          for
            _ <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, anchor)))
            _      <- manager.stop
            events <- manager.events.get
          yield events shouldBe List(
            LspEvent.LspHoverReceived(s"No LSP server available for ${LanguageId.Scala.displayName}", anchor)
          )
        }
    )
  }

  it should "emit an empty completion list when no LSP server is available" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness(serverAvailable = false)
        .use { manager =>
          for
            _ <- manager.effects.offer(Some(LspEffect.CompletionRequested(uri, LanguageId.Scala, 0, 1, anchor)))
            _      <- manager.stop
            events <- manager.events.get
          yield events shouldBe List(LspEvent.LspCompletionReceived(Nil, anchor))
        }
    )
  }

  it should "emit no event for a definition request when no LSP server is available" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness(serverAvailable = false)
        .use { manager =>
          for
            _ <- manager.effects.offer(
              Some(LspEffect.DefinitionRequested(uri, LanguageId.Scala, 0, 1, anchor, "Foo"))
            )
            _      <- manager.stop
            events <- manager.events.get
          yield events shouldBe Nil
        }
    )
  }

  it should "create separate connections for same-language documents in different workspaces" in {
    val firstUri  = "file:///workspace-one/Foo.scala"
    val secondUri = "file:///workspace-two/Bar.scala"
    val program = for
      effects          <- Queue.unbounded[IO, Option[LspEffect]]
      firstConnection  <- LspConnection.create(LanguageId.Scala, logger)
      secondConnection <- LspConnection.create(LanguageId.Scala, logger)
      connected        <- Ref.of[IO, List[String]](Nil)
      bothConnected    <- Deferred[IO, Unit]
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          connected.update(_ :+ fileUri.value) >>
            (if fileUri.value == secondUri then bothConnected.complete(()).void else IO.unit) >>
            IO.pure(
              Some(
                resolvedConnection(
                  if fileUri.value == firstUri then WorkspaceRootUri("file:///workspace-one")
                  else WorkspaceRootUri("file:///workspace-two"),
                  if fileUri.value == firstUri then firstConnection else secondConnection
                )
              )
            )
      result <- Resource
        .make(
          LspManager
            .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
            .start
        )(_.cancel)
        .use { managerFiber =>
          for
            _                    <- effects.offer(Some(LspEffect.FileOpened(firstUri, LanguageId.Scala, "object Foo")))
            _                    <- takeMessage(firstConnection)
            _                    <- effects.offer(Some(LspEffect.FileOpened(secondUri, LanguageId.Scala, "object Bar")))
            _                    <- bothConnected.get
            _                    <- takeMessage(secondConnection)
            _                    <- effects.offer(None)
            _                    <- managerFiber.joinWithNever
            attemptedConnections <- connected.get
          yield attemptedConnections shouldBe List(firstUri, secondUri)
        }
    yield result

    runVirtual(program)
  }

  it should "route lifecycle notifications and releases to their workspace connection" in {
    val firstUri  = "file:///workspace-one/Foo.scala"
    val secondUri = "file:///workspace-two/Bar.scala"
    val program = for
      effects          <- Queue.unbounded[IO, Option[LspEffect]]
      firstConnection  <- LspConnection.create(LanguageId.Scala, logger)
      secondConnection <- LspConnection.create(LanguageId.Scala, logger)
      firstReleased    <- Deferred[IO, Unit]
      secondReleased   <- Deferred[IO, Unit]
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          val (rootUri, connection, release) =
            if fileUri.value == firstUri then
              (WorkspaceRootUri("file:///workspace-one"), firstConnection, firstReleased.complete(()).void)
            else (WorkspaceRootUri("file:///workspace-two"), secondConnection, secondReleased.complete(()).void)
          IO.pure(Some(resolvedConnection(rootUri, connection, release)))
      result <- Resource
        .make(
          LspManager
            .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
            .start
        )(_.cancel)
        .use { managerFiber =>
          for
            _ <- effects.offer(Some(LspEffect.FileOpened(firstUri, LanguageId.Scala, "object Foo")))
            _ <- expectNotification(firstConnection, "textDocument/didOpen", firstUri)
            _ <- effects.offer(Some(LspEffect.FileOpened(secondUri, LanguageId.Scala, "object Bar")))
            _ <- expectNotification(secondConnection, "textDocument/didOpen", secondUri)
            _ <- effects.offer(Some(LspEffect.FileChanged(firstUri, LanguageId.Scala, "object Foo2", version = 2)))
            _ <- expectNotification(firstConnection, "textDocument/didChange", firstUri)
            _ <- noMessage(secondConnection)
            _ <- effects.offer(Some(LspEffect.FileChanged(secondUri, LanguageId.Scala, "object Bar2", version = 2)))
            _ <- expectNotification(secondConnection, "textDocument/didChange", secondUri)
            _ <- noMessage(firstConnection)
            _ <- effects.offer(Some(LspEffect.FileClosed(firstUri, LanguageId.Scala)))
            _ <- expectNotification(firstConnection, "textDocument/didClose", firstUri)
            _ <- firstReleased.get
            _ <- secondReleased.tryGet.map(_ shouldBe None)
            _ <- noMessage(secondConnection)
            _ <- effects.offer(Some(LspEffect.FileClosed(secondUri, LanguageId.Scala)))
            _ <- expectNotification(secondConnection, "textDocument/didClose", secondUri)
            _ <- secondReleased.get
            _ <- effects.offer(None)
            _ <- managerFiber.joinWithNever
          yield succeed
        }
    yield result

    runVirtual(program)
  }

  it should "evict the resolution cache for a document exactly when it closes" in {
    val program = for
      effects    <- Queue.unbounded[IO, Option[LspEffect]]
      connection <- LspConnection.create(LanguageId.Scala, logger)
      evictions  <- Ref.of[IO, List[(LanguageId, String)]](Nil)
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          IO.pure(Some(resolvedConnection(WorkspaceRootUri("file:///workspace"), connection)))
        override def evictResolution(languageId: LanguageId, fileUri: DocumentUri): IO[Unit] =
          evictions.update(_ :+ (languageId -> fileUri.value))
      result <- Resource
        .make(
          LspManager
            .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
            .start
        )(_.cancel)
        .use { managerFiber =>
          for
            _           <- effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, "object Foo")))
            _           <- takeMessage(connection)
            beforeClose <- evictions.get
            _           <- effects.offer(Some(LspEffect.FileClosed(uri, LanguageId.Scala)))
            _           <- takeMessage(connection)
            afterClose  <- evictions.get
            _           <- effects.offer(None)
            _           <- managerFiber.joinWithNever
          yield
            beforeClose shouldBe Nil
            afterClose shouldBe List(LanguageId.Scala -> uri)
        }
    yield result

    runVirtual(program)
  }

  it should "reuse a workspace connection until its last document closes" in {
    val firstUri  = "file:///workspace/Foo.scala"
    val secondUri = "file:///workspace/Bar.scala"
    val program = for
      effects    <- Queue.unbounded[IO, Option[LspEffect]]
      connection <- LspConnection.create(LanguageId.Scala, logger)
      acquired   <- Ref.of[IO, Int](0)
      released   <- Deferred[IO, Unit]
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
                Resource.make(acquired.update(_ + 1).as(connection))(_ => released.complete(()).void)
              )
            )
          )
      result <- Resource
        .make(
          LspManager
            .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
            .start
        )(_.cancel)
        .use { managerFiber =>
          for
            _             <- effects.offer(Some(LspEffect.FileOpened(firstUri, LanguageId.Scala, "object Foo")))
            _             <- takeMessage(connection)
            _             <- effects.offer(Some(LspEffect.FileOpened(secondUri, LanguageId.Scala, "object Bar")))
            _             <- takeMessage(connection)
            _             <- effects.offer(Some(LspEffect.FileClosed(firstUri, LanguageId.Scala)))
            _             <- takeMessage(connection)
            firstRelease  <- released.tryGet
            _             <- effects.offer(Some(LspEffect.FileClosed(secondUri, LanguageId.Scala)))
            _             <- takeMessage(connection)
            _             <- released.get
            _             <- effects.offer(None)
            _             <- managerFiber.joinWithNever
            acquiredCount <- acquired.get
          yield
            firstRelease shouldBe None
            acquiredCount shouldBe 1
        }
    yield result

    runVirtual(program)
  }

  it should "separate connections when a workspace resolves different server configurations" in {
    val firstUri       = "file:///workspace/Foo.scala"
    val secondUri      = "file:///workspace/Bar.scala"
    val overrideConfig = scalaServer.copy(defaultArgs = List("--alternate"))
    val program = for
      effects          <- Queue.unbounded[IO, Option[LspEffect]]
      firstConnection  <- LspConnection.create(LanguageId.Scala, logger)
      secondConnection <- LspConnection.create(LanguageId.Scala, logger)
      connected        <- Deferred[IO, Unit]
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: DocumentUri,
          onDiagnostics: (DocumentUri, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          val (connection, config) =
            if fileUri.value == firstUri then firstConnection -> scalaServer
            else secondConnection                             -> overrideConfig
          (if fileUri.value == secondUri then connected.complete(()).void else IO.unit) >>
            IO.pure(
              Some(
                LspManager.ResolvedConnection(
                  LspManager.ConnectionIdentity(WorkspaceRootUri("file:///workspace"), config),
                  Resource.pure(connection)
                )
              )
            )
      result <- Resource
        .make(
          LspManager
            .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
            .start
        )(_.cancel)
        .use { managerFiber =>
          for
            _ <- effects.offer(Some(LspEffect.FileOpened(firstUri, LanguageId.Scala, "object Foo")))
            _ <- takeMessage(firstConnection)
            _ <- effects.offer(Some(LspEffect.FileOpened(secondUri, LanguageId.Scala, "object Bar")))
            _ <- connected.get
            _ <- takeMessage(secondConnection)
            _ <- effects.offer(None)
            _ <- managerFiber.joinWithNever
          yield succeed
        }
    yield result

    runVirtual(program)
  }
