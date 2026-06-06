package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.serenity.lsp.client.LspConnection
import com.serenity.lsp.config.LanguageId
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterEach, Ignore}
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

@Ignore
class LspConnectionStreamIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspConnectionStreamIntegrationSpec"))
  private val testTimeout = 5.seconds

  override protected def beforeEach(): Unit =
    logger.info("[test] starting LspConnectionStreamIntegrationSpec case").unsafeRunSync()

  override protected def afterEach(): Unit =
    logger.info("[test] finished LspConnectionStreamIntegrationSpec case").unsafeRunSync()

  private def loadFixture(name: String): Json =
    val stream = getClass.getClassLoader.getResourceAsStream(s"lsp/fixtures/$name")
    require(stream != null, s"Fixture not found on classpath: lsp/fixtures/$name")
    parse(new String(stream.readAllBytes())).fold(
      err => throw RuntimeException(s"Bad JSON in fixture $name: $err"),
      identity
    )

  private val initResult = loadFixture("initialize_result.json")

  private def connectionResource(): Resource[IO, (MockLspServer, LspConnection)] =
    for
      server <- MockLspServer.resource(Map("initialize" -> initResult), logger)
      conn   <- LspConnection.connect(LanguageId.Scala, server.clientIn, server.clientOut, "file:///workspace", logger)
    yield (server, conn)

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
