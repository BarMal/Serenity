package com.serenity.lsp

import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.serenity.lsp.client.{LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.TextDocumentSyncKind
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger

class LspConnectionStreamIntegrationSpec extends AnyFlatSpec with Matchers:

  private val logger = new Logger[IO]:
    def error(t: Throwable)(message: => String): IO[Unit] = IO.unit
    def warn(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def info(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def debug(t: Throwable)(message: => String): IO[Unit] = IO.unit
    def trace(t: Throwable)(message: => String): IO[Unit] = IO.unit

    def error(message: => String): IO[Unit] = IO.unit
    def warn(message: => String): IO[Unit]  = IO.unit
    def info(message: => String): IO[Unit]  = IO.unit
    def debug(message: => String): IO[Unit] = IO.unit
    def trace(message: => String): IO[Unit] = IO.unit

  private val testTimeout = 5.seconds

  private def loadFixture(name: String): Json =
    val stream = getClass.getClassLoader.getResourceAsStream(s"lsp/fixtures/$name")
    require(stream != null, s"Fixture not found on classpath: lsp/fixtures/$name")
    parse(new String(stream.readAllBytes())).fold(
      err => throw RuntimeException(s"Bad JSON in fixture $name: $err"),
      identity
    )

  private val initResult            = loadFixture("initialize_result.json")
  private val incrementalInitResult = loadFixture("initialize_result_incremental_sync.json")

  private def connectionResource(initializeResult: Json = initResult): Resource[IO, (MockLspServer, LspConnection)] =
    for
      server <- MockLspServer.resource(Map("initialize" -> initializeResult), logger)
      conn <- LspConnection.connect(
        LanguageId.Scala,
        server.clientIn,
        server.clientOut,
        WorkspaceRootUri("file:///workspace"),
        logger
      )
    yield (server, conn)

  private def serverClosesDuringInitializeResource(): Resource[IO, LspConnection] =
    for
      server <- MockLspServer.resource(Map.empty, logger, closeOnMethods = Set("initialize"))
      conn <- LspConnection.connect(
        LanguageId.Scala,
        server.clientIn,
        server.clientOut,
        WorkspaceRootUri("file:///workspace"),
        logger
      )
    yield conn

  "LspConnection.connect" should "complete the initialize handshake over streams" in
    connectionResource()
      .use { (server, _) =>
        server.drainReceived(2).map { msgs =>
          msgs(0).hcursor.downField("method").as[String].toOption shouldBe Some("initialize")
          msgs(1).hcursor.downField("method").as[String].toOption shouldBe Some("initialized")
        }
      }
      .timeout(testTimeout)
      .unsafeRunSync()

  it should "complete incoming processing when a malformed frame is read" in
    connectionResource()
      .use { (server, conn) =>
        val malformedFrame = "Content-Length: 1\r\n\r\n{".getBytes(StandardCharsets.UTF_8)
        for
          processor <- conn.processIncoming((_, _) => IO.unit).start
          _         <- server.writeRaw(malformedFrame)
          _         <- processor.joinWithNever.timeout(testTimeout)
        yield succeed
      }
      .timeout(testTimeout)
      .unsafeRunSync()

  it should "default to full-text sync when the server's initialize response doesn't declare textDocumentSync" in
    connectionResource()
      .use { (_, conn) => conn.syncKind.map(_ shouldBe TextDocumentSyncKind.Full) }
      .timeout(testTimeout)
      .unsafeRunSync()

  it should "negotiate incremental sync from the server's initialize response" in
    connectionResource(incrementalInitResult)
      .use { (_, conn) => conn.syncKind.map(_ shouldBe TextDocumentSyncKind.Incremental) }
      .timeout(testTimeout)
      .unsafeRunSync()

  it should "fail promptly when the server closes during initialize" in {
    val failure =
      serverClosesDuringInitializeResource()
        .use(_ => IO.unit)
        .attempt
        .timeout(testTimeout)
        .unsafeRunSync()
        .left
        .getOrElse(fail("Expected initialize to fail"))

    failure.getMessage should include("LSP connection closed")
  }
