package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.IO
import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{LspPosition, LspProgress, LspRange, LspTextEdit}
import com.serenity.testkit.VirtualTime.runVirtual
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What `LspManager` does with the server traffic that is not a reply to its own requests (#1847): a server may only
  * edit documents this editor has open, and the work it reports follows it to the application and ends with it.
  */
class LspManagerServerRequestSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  private val change = LspTextEdit(LspRange(LspPosition(0, 0), LspPosition(0, 6)), "class")

  private def applyEditRequest(id: Long, documentUri: String): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> id.asJson,
      "method"  -> "workspace/applyEdit".asJson,
      "params" -> Json.obj(
        "edit" -> Json.obj(
          "changes" -> Json.obj(
            documentUri -> Json.arr(
              Json.obj(
                "newText" -> change.newText.asJson,
                "range" -> Json.obj(
                  "start" -> Json.obj("line" -> 0.asJson, "character" -> 0.asJson),
                  "end"   -> Json.obj("line" -> 0.asJson, "character" -> 6.asJson)
                )
              )
            )
          )
        )
      )
    )

  private def applied(reply: Json): Option[Boolean] =
    reply.hcursor.downField("result").downField("applied").as[Boolean].toOption

  "LspManager" should "apply a server's edit to a document it has open and say so" in
    runVirtual(
      harness().use { manager =>
        for
          _      <- open(manager)
          _      <- manager.connection.handleIncomingJson(applyEditRequest(31, uri))
          reply  <- takeMessage(manager.connection)
          events <- manager.events.get
          _      <- manager.stop
        yield
          applied(reply) shouldBe Some(true)
          reply.hcursor.downField("id").as[Long].toOption shouldBe Some(31L)
          events should contain(LspEvent.LspWorkspaceEditRequested(Map(uri -> List(change))))
      }
    )

  it should "refuse an edit to a document the editor has not opened" in
    runVirtual(
      harness().use { manager =>
        for
          _      <- open(manager)
          _      <- manager.connection.handleIncomingJson(applyEditRequest(32, "file:///workspace/Unopened.scala"))
          reply  <- takeMessage(manager.connection)
          events <- manager.events.get
          _      <- manager.stop
        yield
          applied(reply) shouldBe Some(false)
          reply.hcursor.downField("result").downField("failureReason").as[String].toOption.getOrElse("") should include(
            "Unopened.scala"
          )
          events.collect { case edit: LspEvent.LspWorkspaceEditRequested => edit } shouldBe Nil
      }
    )

  it should "pass the work a server reports to the application" in
    runVirtual(
      harness().use { manager =>
        val progress = Json.obj(
          "jsonrpc" -> "2.0".asJson,
          "method"  -> "$/progress".asJson,
          "params" -> Json.obj(
            "token" -> "index".asJson,
            "value" -> Json.obj("kind" -> "begin".asJson, "title" -> "Indexing".asJson)
          )
        )
        for
          _      <- open(manager)
          _      <- manager.connection.handleIncomingJson(progress)
          _      <- IO.sleep(1.second)
          events <- manager.events.get
          _      <- manager.stop
        yield events should contain(
          LspEvent.LspProgressReceived(LanguageId.Scala, "index", LspProgress.Begin("Indexing", None, None))
        )
      }
    )
