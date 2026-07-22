package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.{Deferred, Fiber, IO, Ref, Resource}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Event, LspEvent}
import com.serenity.lsp.client.LspConnection
import com.serenity.lsp.config.LanguageId
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

  private val logger = LoggerFactory[IO].getLogger(using LoggerName("LspManagerSpec"))
  private val uri    = "file:///workspace/Foo.scala"

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
      effects    <- Queue.unbounded[IO, Option[LspEffect]]
      events     <- Ref.of[IO, List[Event]](Nil)
      eventApplied <- Deferred[IO, Unit]
      connection <- LspConnection.create(LanguageId.Scala, logger)
      released   <- Deferred[IO, Unit]
      provider = new LspManager.ConnectionProvider:
        def connect(
          languageId: LanguageId,
          fileUri: String,
          onDiagnostics: (String, List[com.serenity.lsp.model.Diagnostic]) => IO[Unit]
        ): IO[Option[Resource[IO, LspConnection]]] =
          IO.pure(Some(Resource.make(IO.pure(connection))(_ => released.complete(()).void)))
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

  private def requestId(message: Json): Long =
    message.hcursor.downField("id").as[Long].toOption.getOrElse(fail("Request was missing an id"))

  private def response(id: Long, result: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id.asJson, "result" -> result)

  private def open(manager: Harness): IO[Unit] =
    manager.effects.offer(Some(LspEffect.FileOpened(uri, LanguageId.Scala, "object Foo"))) >>
      takeMessage(manager.connection).flatMap { message =>
        IO(message.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didOpen"))
      }

  "LspManager" should "send document changes while a hover response is pending" in {
    (for
      manager <- harness
      _       <- open(manager)
      _ <- manager.effects.offer(
        Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, CursorPosition(0, 1)))
      )
      hover <- takeMessage(manager.connection)
      _ = hover.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/hover")
      _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
      change <- takeMessage(manager.connection)
      _ = change.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didChange")
      _ <- manager.stop
    yield succeed).timeout(3.seconds).unsafeRunSync()
  }

  it should "discard a definition response after its document version changes" in {
    val anchor = CursorPosition(0, 1)
    (for
      manager <- harness
      _       <- open(manager)
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
              "end" -> Json.obj("line" -> 0.asJson, "character" -> 3.asJson)
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
      manager <- harness
      _       <- open(manager)
      _ <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, firstAnchor)))
      firstRequest <- takeMessage(manager.connection)
      _ <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 2, secondAnchor)))
      secondRequest <- takeMessage(manager.connection)
      _ <- manager.connection.handleIncomingJson(
        response(requestId(firstRequest), Json.obj("contents" -> "stale".asJson))
      )
      _ <- manager.connection.handleIncomingJson(
        response(requestId(secondRequest), Json.obj("contents" -> "current".asJson))
      )
      _ <- manager.eventApplied.get
      events <- manager.events.get
      _ = events shouldBe List(LspEvent.LspHoverReceived("current", secondAnchor))
      _ <- manager.stop
    yield succeed).timeout(3.seconds).unsafeRunSync()
  }

  it should "cancel pending request fibers before releasing connections on shutdown" in {
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
      released shouldBe Some(())).timeout(3.seconds).unsafeRunSync()
  }
