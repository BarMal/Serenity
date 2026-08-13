package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, Fiber, IO, Ref, Resource}
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.LspConnection
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import com.serenity.state.models.CursorPosition
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
    rootUri: String,
    connection: LspConnection,
    release: IO[Unit] = IO.unit
  ): LspManager.ResolvedConnection =
    LspManager.ResolvedConnection(
      LspManager.ConnectionIdentity(rootUri, scalaServer),
      Resource.make(IO.pure(connection))(_ => release)
    )

  private case class Harness(
      effects: Queue[IO, Option[LspEffect]],
      events: Ref[IO, List[Event]],
      eventApplied: Deferred[IO, Unit],
      connection: LspConnection,
      released: Deferred[IO, Unit],
      managerFiber: Fiber[IO, Throwable, Unit]
  ):
    def stop: IO[Unit] =
      effects.offer(None) >> managerFiber.joinWithNever

  private def harness: IO[Harness] =
    for
      effects      <- Queue.unbounded[IO, Option[LspEffect]]
      events       <- Ref.of[IO, List[Event]](Nil)
      eventApplied <- Deferred[IO, Unit]
      connection   <- LspConnection.create(LanguageId.Scala, logger)
      released     <- Deferred[IO, Unit]
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: String,
          onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          IO.pure(Some(resolvedConnection("file:///workspace", connection, released.complete(()).void)))
      managerFiber <- LspManager
        .runWithProvider(
          Stream.fromQueueNoneTerminated(effects),
          event => events.update(_ :+ event) >> eventApplied.complete(()).void,
          logger,
          provider
        )
        .start
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
    connection.takeOutgoing.timeoutTo(100.millis, IO.pure(None)).map(_ shouldBe None)

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
    (for
      manager <- harness
      _       <- open(manager)
      _ <- manager.effects.offer(
        Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, CursorPosition(0, 1)))
      )
      hover <- takeMessage(manager.connection)
      _ = hover.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/hover")
      _      <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
      change <- takeMessage(manager.connection)
      _ = change.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didChange")
      _ <- manager.stop
    yield succeed).timeout(3.seconds).unsafeRunSync()

  it should "discard a definition response after its document version changes" in {
    val anchor = CursorPosition(0, 1)
    (for
      manager <- harness
      _       <- open(manager)
      _ <- manager.effects.offer(
        Some(LspEffect.DefinitionRequested(uri, LanguageId.Scala, 0, 1, anchor, "Foo"))
      )
      request <- takeMessage(manager.connection)
      _       <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
      _       <- takeMessage(manager.connection)
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
    yield succeed).timeout(3.seconds).unsafeRunSync()
  }

  it should "cancel a superseded hover request and retain only the current anchor" in {
    val firstAnchor  = CursorPosition(0, 1)
    val secondAnchor = CursorPosition(0, 2)
    (for
      manager       <- harness
      _             <- open(manager)
      _             <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, firstAnchor)))
      firstRequest  <- takeMessage(manager.connection)
      _             <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 2, secondAnchor)))
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
    yield succeed).timeout(3.seconds).unsafeRunSync()
  }

  it should "cancel pending request fibers before releasing connections on shutdown" in
    (for
      manager <- harness
      _       <- open(manager)
      _ <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, CursorPosition(0, 1))))
      _ <- takeMessage(manager.connection)
      _ <- manager.stop
      pending  <- manager.connection.pendingRequestCount
      released <- manager.released.tryGet
    yield
      pending shouldBe 0
      released shouldBe Some(())
    ).timeout(3.seconds).unsafeRunSync()

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
          fileUri: String,
          onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          connected.update(_ :+ fileUri) >>
            (if fileUri == secondUri then bothConnected.complete(()).void else IO.unit) >>
            IO.pure(
              Some(
                resolvedConnection(
                  if fileUri == firstUri then "file:///workspace-one" else "file:///workspace-two",
                  if fileUri == firstUri then firstConnection else secondConnection
                )
              )
            )
      managerFiber <- LspManager
        .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
        .start
      _                    <- effects.offer(Some(LspEffect.FileOpened(firstUri, LanguageId.Scala, "object Foo")))
      _                    <- takeMessage(firstConnection)
      _                    <- effects.offer(Some(LspEffect.FileOpened(secondUri, LanguageId.Scala, "object Bar")))
      _                    <- bothConnected.get
      _                    <- takeMessage(secondConnection)
      _                    <- effects.offer(None)
      _                    <- managerFiber.joinWithNever
      attemptedConnections <- connected.get
    yield attemptedConnections shouldBe List(firstUri, secondUri)

    program.timeout(3.seconds).unsafeRunSync()
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
          fileUri: String,
          onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          val (rootUri, connection, release) =
            if fileUri == firstUri then ("file:///workspace-one", firstConnection, firstReleased.complete(()).void)
            else ("file:///workspace-two", secondConnection, secondReleased.complete(()).void)
          IO.pure(Some(resolvedConnection(rootUri, connection, release)))
      managerFiber <- LspManager
        .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
        .start
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

    program.timeout(5.seconds).unsafeRunSync()
  }

  it should "evict the resolution cache for a document exactly when it closes" in {
    val program = for
      effects    <- Queue.unbounded[IO, Option[LspEffect]]
      connection <- LspConnection.create(LanguageId.Scala, logger)
      evictions  <- Ref.of[IO, List[(LanguageId, String)]](Nil)
      provider = new LspManager.ConnectionProvider:
        def resolve(
          languageId: LanguageId,
          fileUri: String,
          onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          IO.pure(Some(resolvedConnection("file:///workspace", connection)))
        override def evictResolution(languageId: LanguageId, fileUri: String): IO[Unit] =
          evictions.update(_ :+ (languageId -> fileUri))
      managerFiber <- LspManager
        .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
        .start
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

    program.timeout(3.seconds).unsafeRunSync()
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
          fileUri: String,
          onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          IO.pure(
            Some(
              LspManager.ResolvedConnection(
                LspManager.ConnectionIdentity("file:///workspace", scalaServer),
                Resource.make(acquired.update(_ + 1).as(connection))(_ => released.complete(()).void)
              )
            )
          )
      managerFiber <- LspManager
        .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
        .start
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

    program.timeout(3.seconds).unsafeRunSync()
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
          fileUri: String,
          onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[LspManager.ResolvedConnection]] =
          val (connection, config) =
            if fileUri == firstUri then firstConnection -> scalaServer
            else secondConnection                       -> overrideConfig
          (if fileUri == secondUri then connected.complete(()).void else IO.unit) >>
            IO.pure(
              Some(
                LspManager.ResolvedConnection(
                  LspManager.ConnectionIdentity("file:///workspace", config),
                  Resource.pure(connection)
                )
              )
            )
      managerFiber <- LspManager
        .runWithProvider(Stream.fromQueueNoneTerminated(effects), _ => IO.unit, logger, provider)
        .start
      _ <- effects.offer(Some(LspEffect.FileOpened(firstUri, LanguageId.Scala, "object Foo")))
      _ <- takeMessage(firstConnection)
      _ <- effects.offer(Some(LspEffect.FileOpened(secondUri, LanguageId.Scala, "object Bar")))
      _ <- connected.get
      _ <- takeMessage(secondConnection)
      _ <- effects.offer(None)
      _ <- managerFiber.joinWithNever
    yield succeed

    program.timeout(3.seconds).unsafeRunSync()
  }
