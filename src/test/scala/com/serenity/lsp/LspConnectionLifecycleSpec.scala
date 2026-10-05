package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.lsp.client.{LspConnection, LspMethod, WorkspaceRootUri}
import com.serenity.lsp.config.LanguageId
import com.serenity.testkit.VirtualTime.runVirtual
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** What a connection does when its server stops reading or dies (#1847): it must say so, fail whatever was waiting on
  * the server, and refuse further messages immediately instead of queueing them for nobody.
  */
class LspConnectionLifecycleSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspConnectionLifecycleSpec"))
  private val testTimeout = 5.seconds

  private val didChange = LspMethod("textDocument/didChange")

  "LspConnection" should "fail a pending request and report termination when its server dies mid-request" in {
    val outcome = MockLspServer
      .resource(
        Map("initialize" -> Json.obj("capabilities" -> Json.obj())),
        logger,
        closeOnMethods = Set("textDocument/hover")
      )
      .flatMap(server =>
        LspConnection
          .connect(LanguageId.Scala, server.clientIn, server.clientOut, WorkspaceRootUri("file:///workspace"), logger)
          .map(server -> _)
      )
      .use { (server, connection) =>
        for
          _          <- server.drainReceived(2)
          hover      <- connection.sendRequest(LspMethod("textDocument/hover"), Json.obj()).attempt.start
          _          <- connection.terminated
          result     <- hover.joinWithNever
          afterDeath <- (1 to 300).toList.traverse(_ => connection.sendNotification(didChange, Json.obj()).attempt)
        yield (result, afterDeath)
      }
      .timeout(testTimeout)
      .unsafeRunSync()

    val (hoverResult, notificationsAfterDeath) = outcome
    hoverResult.left.toOption.map(_.getMessage).getOrElse("") should include("LSP connection closed")
    notificationsAfterDeath.forall(_.isLeft) shouldBe true
  }

  it should "fail a notification instead of waiting when its server has stopped reading" in {
    val results = runVirtual(
      LspConnection.create(LanguageId.Scala, logger).flatMap { connection =>
        (1 to 300).toList
          .traverse(_ => connection.sendNotification(didChange, Json.obj()).attempt)
          .timeout(testTimeout)
      }
    )

    results.take(256).forall(_.isRight) shouldBe true
    results.drop(256).map(_.left.toOption.map(_.getClass)).distinct shouldBe List(
      Some(classOf[LspConnection.LspOutgoingQueueFull])
    )
  }
