package com.serenity.lsp

import java.io.{ByteArrayOutputStream, EOFException}
import java.net.{InetAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.serenity.lsp.client.{LspConnection, LspFramer}
import com.serenity.lsp.config.LanguageId
import com.serenity.testkit.RealBoundaryTest
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger

/** The real-socket counterpart to `LspConnectionStreamIntegrationSpec`: every other LSP spec drives `LspConnection`
  * over `MockLspServer`'s `QueueBytePipe`, an in-memory `LinkedBlockingQueue`-backed duplex pipe chosen precisely
  * because it has no thread-identity or OS-buffering quirks of its own (see `MockLspServer`'s doc comment). That makes
  * it a fast, deterministic double for a real subprocess's stdin/stdout, but it also means no test in the fast suite
  * ever proves `LspFramer`'s header/body reassembly against bytes that arrived across genuinely independent OS read
  * calls, the way a real loopback TCP socket delivers them -- kernel buffering, Nagle's algorithm, and scheduling can
  * all split or coalesce writes in ways an in-memory queue never will.
  *
  * This spec opens one real loopback `ServerSocket`/`Socket` pair, reads the framed `initialize` request one byte at a
  * time on the server side (the most fragmented a stream can possibly be consumed as), and writes the response back in
  * small flushed fragments -- forcing genuinely separate TCP segments rather than whatever a single bulk write happens
  * to produce -- to prove the same `LspFramer.decode` pipe `LspConnection` uses in production reassembles a real byte
  * stream correctly in both directions.
  *
  * Tagged [[RealBoundaryTest]] and excluded from the default parallel suite (see `build.sbt`): binding a real loopback
  * port is real OS state the fast suite must never touch -- not because the assertions here are flaky. Run explicitly
  * via the `realBoundaryTest` command.
  */
class LspConnectionRealSocketIntegrationSpec extends AnyFlatSpec with Matchers:

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

  private val testTimeout = 10.seconds

  private def loadFixture(name: String): Json =
    val stream = getClass.getClassLoader.getResourceAsStream(s"lsp/fixtures/$name")
    require(stream != null, s"Fixture not found on classpath: lsp/fixtures/$name")
    parse(new String(stream.readAllBytes())).fold(
      err => throw RuntimeException(s"Bad JSON in fixture $name: $err"),
      identity
    )

  private val initResult = loadFixture("initialize_result.json")

  /** A real loopback TCP client/server pair, playing the same "other end of the wire" role `MockLspServer` plays
    * everywhere else in the suite.
    */
  private def loopbackSockets(): Resource[IO, (Socket, Socket)] =
    for
      serverSocket <- Resource.make(IO.blocking(new ServerSocket(0, 1, InetAddress.getLoopbackAddress)))(s =>
        IO.blocking(s.close()).attempt.void
      )
      clientSocket <- Resource.make(
        IO.blocking(new Socket(InetAddress.getLoopbackAddress, serverSocket.getLocalPort))
      )(s => IO.blocking(s.close()).attempt.void)
      acceptedSocket <- Resource.make(IO.blocking(serverSocket.accept()))(s => IO.blocking(s.close()).attempt.void)
    yield (acceptedSocket, clientSocket)

  /** Reads the framed `initialize` request off a real socket one byte at a time and writes the framed response back in
    * small flushed fragments -- deliberately the opposite of one bulk read/write -- to prove `LspFramer`'s reassembly
    * against genuinely fragmented OS-level delivery rather than whatever a single `read`/`write` call happens to
    * produce on a fast loopback connection.
    */
  private def readHeaderLength(in: java.io.InputStream, headerSoFar: String): Int =
    val next = in.read()
    if next < 0 then throw new EOFException("server socket closed before the request header was fully framed")
    val header = headerSoFar + next.toChar
    if !header.endsWith("\r\n\r\n") then readHeaderLength(in, header)
    else
      header
        .split("\r\n")
        .toList
        .collectFirst {
          case line if line.startsWith("Content-Length:") =>
            line.drop("Content-Length:".length).trim.toInt
        }
        .getOrElse(throw new IllegalStateException(s"missing Content-Length in: $header"))

  private def readBody(in: java.io.InputStream, remaining: Int, body: ByteArrayOutputStream): Unit =
    if remaining > 0 then
      val next = in.read()
      if next < 0 then throw new EOFException("server socket closed before the request body was fully framed")
      body.write(next)
      readBody(in, remaining - 1, body)

  private def respondToInitialize(serverSocket: Socket): IO[Json] =
    IO.blocking {
      val in  = serverSocket.getInputStream
      val out = serverSocket.getOutputStream

      val length = readHeaderLength(in, "")
      val body   = new ByteArrayOutputStream()
      readBody(in, length, body)

      val request = parse(new String(body.toByteArray, StandardCharsets.UTF_8))
        .getOrElse(throw new IllegalStateException("malformed request JSON"))
      val id = request.hcursor.downField("id").as[Long].getOrElse(0L)

      val response = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id.asJson, "result" -> initResult)
      LspFramer.encode(response).grouped(3).foreach { fragment =>
        out.write(fragment)
        out.flush()
      }
      request
    }

  "LspConnection.connect" should
    "complete the initialize handshake over a real loopback TCP socket" taggedAs RealBoundaryTest in
    loopbackSockets()
      .use { (serverSocket, clientSocket) =>
        for
          serverRequest <- respondToInitialize(serverSocket).start
          connection <- LspConnection
            .connect(
              LanguageId.Scala,
              clientSocket.getInputStream,
              clientSocket.getOutputStream,
              "file:///workspace",
              logger
            )
            .allocated
          (_, release) = connection
          request <- serverRequest.joinWithNever
          _       <- release
        yield request.hcursor.downField("method").as[String].toOption shouldBe Some("initialize")
      }
      .timeout(testTimeout)
      .unsafeRunSync()
