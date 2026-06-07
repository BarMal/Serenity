package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import com.serenity.lsp.client.{LspConnection, LspProtocol}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity}

class LspConnectionSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspConnectionSpec"))
  private val testTimeout = 3.seconds

  override protected def beforeEach(): Unit =
    logger.info("[test] starting LspConnectionSpec case").unsafeRunSync()

  override protected def afterEach(): Unit =
    logger.info("[test] finished LspConnectionSpec case").unsafeRunSync()

  private def loadFixture(name: String): Json =
    val stream = getClass.getClassLoader.getResourceAsStream(s"lsp/fixtures/$name")
    require(stream != null, s"Fixture not found on classpath: lsp/fixtures/$name")
    parse(new String(stream.readAllBytes())).fold(
      err => throw RuntimeException(s"Bad JSON in fixture $name: $err"),
      identity
    )

  private val pubDiagsErrors = loadFixture("publish_diagnostics_errors.json")
  private val pubDiagsEmpty  = loadFixture("publish_diagnostics_empty.json")
  private val windowLogNotif = loadFixture("window_log_notification.json")

  private def makeConnection(): IO[LspConnection] =
    LspConnection.create(LanguageId.Scala, logger)

  private def withIncomingProcessor[A](
    conn: LspConnection
  )(
    use: Queue[IO, (String, List[Diagnostic])] => IO[A]
  ): IO[A] =
    for
      callQueue <- Queue.unbounded[IO, (String, List[Diagnostic])]
      fiber     <- conn.processIncoming((uri, diags) => callQueue.offer((uri, diags))).start
      result    <- use(callQueue).timeout(testTimeout).guarantee(fiber.cancel)
    yield result

  "LspConnection.sendRequest" should "enqueue a request and complete when the matching response arrives" in
    (for
      conn         <- makeConnection()
      requestFiber <- conn.sendRequest("initialize", LspProtocol.initializeParams(123, "file:///workspace")).start
      outgoing     <- conn.takeOutgoing
      requestJson  <- IO.fromOption(outgoing)(new RuntimeException("Missing outgoing request"))
      requestId <- IO
        .fromOption(requestJson.hcursor.downField("id").as[Long].toOption)(new RuntimeException("Missing request id"))
      _ <- conn.handleIncomingJson(
        Json.obj("jsonrpc" -> "2.0".asJson, "id" -> requestId.asJson, "result" -> Json.obj())
      )
      response <- requestFiber.joinWithNever
    yield
      requestJson.hcursor.downField("method").as[String].toOption shouldBe Some("initialize")
      response.hcursor.downField("id").as[Long].toOption shouldBe Some(requestId)
    ).timeout(testTimeout).unsafeRunSync()

  "LspConnection.sendNotification" should "enqueue a notification without an id" in
    (for
      conn <- makeConnection()
      _ <- conn.sendNotification(
        "textDocument/didOpen",
        LspProtocol.didOpenParams("file:///workspace/Foo.scala", "scala", 1, "object Foo")
      )
      outgoing    <- conn.takeOutgoing
      messageJson <- IO.fromOption(outgoing)(new RuntimeException("Missing outgoing notification"))
    yield
      messageJson.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/didOpen")
      messageJson.hcursor.downField("id").as[Long].toOption shouldBe None
      messageJson.hcursor
        .downField("params")
        .downField("textDocument")
        .downField("uri")
        .as[String]
        .toOption shouldBe Some("file:///workspace/Foo.scala")
    ).timeout(testTimeout).unsafeRunSync()

  "LspConnection.processIncoming" should "route publishDiagnostics to the callback" in
    (for
      conn <- makeConnection()
      _ <- withIncomingProcessor(conn) { callQueue =>
        for
          _      <- conn.handleIncomingJson(pubDiagsErrors)
          result <- callQueue.take.timeout(testTimeout)
        yield
          result._1 shouldBe "file:///workspace/Foo.scala"
          result._2 should have size 2
          result._2.head.severity shouldBe Some(DiagnosticSeverity.Error)
          result._2.head.message shouldBe "type mismatch: expected Int, found String"
          result._2(1).severity shouldBe Some(DiagnosticSeverity.Warning)
          result._2(1).message shouldBe "unused import"
      }
    yield succeed).timeout(testTimeout).unsafeRunSync()

  it should "pass an empty list when diagnostics are cleared" in
    (for
      conn <- makeConnection()
      _ <- withIncomingProcessor(conn) { callQueue =>
        for
          _      <- conn.handleIncomingJson(pubDiagsEmpty)
          result <- callQueue.take.timeout(testTimeout)
        yield
          result._1 shouldBe "file:///workspace/Foo.scala"
          result._2 shouldBe empty
      }
    yield succeed).timeout(testTimeout).unsafeRunSync()

  it should "ignore unknown notifications and continue processing later diagnostics" in
    (for
      conn <- makeConnection()
      _ <- withIncomingProcessor(conn) { callQueue =>
        for
          _      <- conn.handleIncomingJson(windowLogNotif)
          _      <- conn.handleIncomingJson(pubDiagsEmpty)
          result <- callQueue.take.timeout(testTimeout)
        yield result._2 shouldBe empty
      }
    yield succeed).timeout(testTimeout).unsafeRunSync()

  it should "preserve notification order" in
    (for
      conn <- makeConnection()
      _ <- withIncomingProcessor(conn) { callQueue =>
        for
          _      <- conn.handleIncomingJson(pubDiagsErrors)
          _      <- conn.handleIncomingJson(pubDiagsEmpty)
          first  <- callQueue.take.timeout(testTimeout)
          second <- callQueue.take.timeout(testTimeout)
        yield
          first._2 should have size 2
          second._2 shouldBe empty
      }
    yield succeed).timeout(testTimeout).unsafeRunSync()
