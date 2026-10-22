package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.serenity.lsp.client.{LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The `initialize` handshake has a budget of its own: a connection whose requests give up quickly must not make the
  * handshake race that same budget (a macOS runner stalled past 300 ms in LspServerRequestSpec, which keeps its request
  * timeout short to test giving up on a hook).
  */
class LspHandshakeTimeoutSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspHandshakeTimeoutSpec"))

  private val serverTakes = 400.millis

  private def connection(requestTimeout: FiniteDuration, handshakeTimeout: Option[FiniteDuration]): Resource[IO, Unit] =
    for
      server <- MockLspServer.resource(
        Map("initialize" -> Json.obj("capabilities" -> Json.obj())),
        logger,
        replyDelays = Map("initialize" -> serverTakes)
      )
      _ <- LspConnection.connect(
        LanguageId.Scala,
        server.clientIn,
        server.clientOut,
        WorkspaceRootUri("file:///workspace"),
        logger,
        requestTimeout = requestTimeout,
        handshakeTimeout = handshakeTimeout
      )
    yield ()

  private def connected(requestTimeout: FiniteDuration, handshakeTimeout: Option[FiniteDuration]) =
    connection(requestTimeout, handshakeTimeout).use(_ => IO.unit).timeout(30.seconds).attempt.unsafeRunSync()

  "A handshake" should "wait for a slow server for the handshake timeout, not the request timeout" in {
    connected(requestTimeout = 100.millis, handshakeTimeout = Some(20.seconds)) shouldBe Right(())
  }

  it should "give up at the handshake timeout when the server is slower than that" in {
    val outcome = connected(requestTimeout = 20.seconds, handshakeTimeout = Some(100.millis))

    outcome.left.map(_.getMessage) shouldBe Left("LSP request timed out: scala initialize after 100 ms")
  }

  it should "use the request timeout when it has no timeout of its own" in {
    val outcome = connected(requestTimeout = 100.millis, handshakeTimeout = None)

    outcome.left.map(_.getMessage) shouldBe Left("LSP request timed out: scala initialize after 100 ms")
  }
