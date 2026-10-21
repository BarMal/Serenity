package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.serenity.lsp.client.{LspConnection, LspProtocol, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.PositionEncoding
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger

/** Positions are UTF-16 columns in Serenity, so the client says so on `initialize` and refuses a server that answers
  * with another encoding rather than mis-position every edit it sends (#1965).
  */
class LspPositionEncodingSpec extends AnyFlatSpec with Matchers:

  private val quiet = new Logger[IO]:
    def error(t: Throwable)(message: => String): IO[Unit] = IO.unit
    def warn(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def info(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def debug(t: Throwable)(message: => String): IO[Unit] = IO.unit
    def trace(t: Throwable)(message: => String): IO[Unit] = IO.unit
    def error(message: => String): IO[Unit]               = IO.unit
    def warn(message: => String): IO[Unit]                = IO.unit
    def info(message: => String): IO[Unit]                = IO.unit
    def debug(message: => String): IO[Unit]               = IO.unit
    def trace(message: => String): IO[Unit]               = IO.unit

  private def initializeResult(encoding: Option[String]): Json =
    val capabilities = encoding.fold("{}")(kind => s"""{"positionEncoding": "$kind"}""")
    parse(s"""{"capabilities": $capabilities}""").fold(throw _, identity)

  "The initialize request" should "offer UTF-16 as the only position encoding" in {
    val params = LspProtocol.initializeParams(1, WorkspaceRootUri("file:///workspace"))

    params.hcursor
      .downField("capabilities")
      .downField("general")
      .downField("positionEncodings")
      .as[List[String]] shouldBe Right(List("utf-16"))
  }

  "The server's position encoding" should "be UTF-16 when its initialize result names none" in {
    PositionEncoding.fromInitializeResult(initializeResult(None)) shouldBe PositionEncoding.Utf16
  }

  it should "be read from capabilities.positionEncoding" in {
    List("utf-8", "utf-16", "utf-32", "ucs-4").map(kind =>
      PositionEncoding.fromInitializeResult(initializeResult(Some(kind)))
    ) shouldBe List(
      PositionEncoding.Utf8,
      PositionEncoding.Utf16,
      PositionEncoding.Utf32,
      PositionEncoding.Other("ucs-4")
    )
  }

  private def connect(encoding: Option[String]): Resource[IO, LspConnection] =
    for
      server <- MockLspServer.resource(Map("initialize" -> initializeResult(encoding)), quiet)
      conn <- LspConnection.connect(
        LanguageId.Scala,
        server.clientIn,
        server.clientOut,
        WorkspaceRootUri("file:///workspace"),
        quiet
      )
    yield conn

  "Connecting" should "succeed with a server that uses UTF-16, named or not" in
    List(None, Some("utf-16")).foreach { encoding =>
      connect(encoding).use(_ => IO.unit).timeout(5.seconds).unsafeRunSync()
    }

  it should "fail, naming the encoding, with a server that answers in another" in {
    val failure = connect(Some("utf-8"))
      .use(_ => IO.unit)
      .attempt
      .timeout(5.seconds)
      .unsafeRunSync()
      .left
      .getOrElse(fail("Expected the handshake to be refused"))

    failure.getMessage should (include("utf-8") and include("position encoding"))
  }
