package com.serenity.lsp

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.client.{LspFramer, LspProtocol}
import com.serenity.lsp.model.DiagnosticSeverity
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets

class LspProtocolSpec extends AnyFlatSpec with Matchers:

  "LspFramer.encode" should "produce a valid Content-Length framed message" in {
    val json  = Json.obj("method" -> "ping".asJson)
    val bytes = LspFramer.encode(json)
    val str   = new String(bytes, StandardCharsets.UTF_8)

    str should startWith("Content-Length: ")
    str should include("\r\n\r\n")
    val sepIdx   = str.indexOf("\r\n\r\n")
    val header   = str.substring(0, sepIdx)
    val body     = str.substring(sepIdx + 4)
    val declared = header.stripPrefix("Content-Length: ").trim.toInt
    body.getBytes(StandardCharsets.UTF_8).length shouldBe declared
    body should include("\"ping\"")
  }

  "LspFramer.decode" should "round-trip a single message" in {
    val original = Json.obj("jsonrpc" -> "2.0".asJson, "method" -> "test".asJson)
    val bytes    = LspFramer.encode(original)
    val result   = fs2.Stream.chunk(fs2.Chunk.array(bytes))
      .through(LspFramer.decode)
      .compile.toList.unsafeRunSync()

    result shouldBe List(original)
  }

  it should "decode multiple messages from a single byte stream" in {
    val msg1 = Json.obj("id" -> 1.asJson, "result" -> "ok".asJson)
    val msg2 = Json.obj("id" -> 2.asJson, "result" -> "done".asJson)
    val bytes = LspFramer.encode(msg1) ++ LspFramer.encode(msg2)
    val result = fs2.Stream.chunk(fs2.Chunk.array(bytes))
      .through(LspFramer.decode)
      .compile.toList.unsafeRunSync()

    result shouldBe List(msg1, msg2)
  }

  "LspProtocol" should "identify responses and notifications correctly" in {
    val response     = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "result" -> Json.obj())
    val notification = Json.obj("jsonrpc" -> "2.0".asJson, "method" -> "initialized".asJson, "params" -> Json.obj())
    val request      = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 2.asJson, "method" -> "test".asJson, "params" -> Json.obj())

    LspProtocol.isResponse(response)       shouldBe true
    LspProtocol.isNotification(response)   shouldBe false
    LspProtocol.isNotification(notification) shouldBe true
    LspProtocol.isResponse(notification)   shouldBe false
    LspProtocol.isResponse(request)        shouldBe false
    LspProtocol.isNotification(request)    shouldBe false
  }

  it should "parse publishDiagnostics notifications" in {
    val diagJson = Json.obj(
      "method" -> "textDocument/publishDiagnostics".asJson,
      "params" -> Json.obj(
        "uri" -> "file:///foo/Bar.scala".asJson,
        "diagnostics" -> Json.arr(
          Json.obj(
            "range" -> Json.obj(
              "start" -> Json.obj("line" -> 5.asJson, "character" -> 2.asJson),
              "end"   -> Json.obj("line" -> 5.asJson, "character" -> 10.asJson)
            ),
            "severity" -> 1.asJson,
            "message"  -> "type mismatch".asJson,
            "source"   -> "metals".asJson
          )
        )
      )
    )

    val result = LspProtocol.parseDiagnostics(diagJson)
    result shouldBe defined
    val (uri, diags) = result.get
    uri   shouldBe "file:///foo/Bar.scala"
    diags should have size 1
    diags.head.message          shouldBe "type mismatch"
    diags.head.severity         shouldBe Some(DiagnosticSeverity.Error)
    diags.head.range.start.line shouldBe 5
    diags.head.source           shouldBe Some("metals")
  }

  it should "parse empty diagnostics list" in {
    val json = Json.obj(
      "method" -> "textDocument/publishDiagnostics".asJson,
      "params" -> Json.obj(
        "uri"         -> "file:///foo/Clean.scala".asJson,
        "diagnostics" -> Json.arr()
      )
    )
    val result = LspProtocol.parseDiagnostics(json)
    result shouldBe Some(("file:///foo/Clean.scala", Nil))
  }

  it should "build initialize params with processId and rootUri" in {
    val params = LspProtocol.initializeParams(12345, "file:///workspace")
    params.hcursor.downField("processId").as[Int].toOption shouldBe Some(12345)
    params.hcursor.downField("rootUri").as[String].toOption shouldBe Some("file:///workspace")
  }

  it should "build didOpen params with correct structure" in {
    val params = LspProtocol.didOpenParams("file:///foo/Bar.scala", "scala", 1, "object Bar")
    val td = params.hcursor.downField("textDocument")
    td.downField("uri").as[String].toOption     shouldBe Some("file:///foo/Bar.scala")
    td.downField("languageId").as[String].toOption shouldBe Some("scala")
    td.downField("version").as[Int].toOption    shouldBe Some(1)
    td.downField("text").as[String].toOption    shouldBe Some("object Bar")
  }
