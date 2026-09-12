package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.Resource
import cats.effect.unsafe.implicits.global
import cats.effect.IO
import com.serenity.lsp.client.{LspConnection, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.SemanticTokensLegend
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger

/** Covers the #859/#1177 handshake-side wiring: `LspConnection.connect` must capture the server's semantic tokens
  * legend off its `initialize` result, since [[LspManager]]'s semantic tokens request path (`semanticTokensLegend`)
  * depends on it having been recorded there rather than parsed anew per request.
  */
class LspConnectionSemanticTokensSpec extends AnyFlatSpec with Matchers:

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

  private def connectionResource(initializeResult: Json): Resource[IO, LspConnection] =
    for
      server <- MockLspServer.resource(Map("initialize" -> initializeResult), logger)
      conn <- LspConnection.connect(
        LanguageId.Scala,
        server.clientIn,
        server.clientOut,
        WorkspaceRootUri("file:///workspace"),
        logger
      )
    yield conn

  "LspConnection.connect" should "capture the server's semantic tokens legend from its initialize result" in {
    val initializeResult = Json.obj(
      "capabilities" -> Json.obj(
        "semanticTokensProvider" -> Json.obj(
          "legend" -> Json.obj(
            "tokenTypes"     -> Json.arr("keyword".asJson, "string".asJson),
            "tokenModifiers" -> Json.arr("readonly".asJson)
          ),
          "full" -> true.asJson
        )
      )
    )

    val legend = connectionResource(initializeResult)
      .use(conn => conn.semanticTokensLegend)
      .timeout(testTimeout)
      .unsafeRunSync()

    legend shouldBe Some(SemanticTokensLegend(List("keyword", "string"), List("readonly")))
  }

  it should "leave the semantic tokens legend absent when the server's initialize result omits it" in {
    val legend = connectionResource(loadFixture("initialize_result.json"))
      .use(conn => conn.semanticTokensLegend)
      .timeout(testTimeout)
      .unsafeRunSync()

    legend shouldBe None
  }
