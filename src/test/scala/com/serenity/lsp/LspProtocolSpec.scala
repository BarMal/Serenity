package com.serenity.lsp

import java.nio.charset.StandardCharsets

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.client.{LspFramer, LspProtocol}
import com.serenity.lsp.model.DiagnosticSeverity
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
    val result = fs2.Stream
      .chunk(fs2.Chunk.array(bytes))
      .through(LspFramer.decode)
      .compile
      .toList
      .unsafeRunSync()

    result shouldBe List(original)
  }

  it should "decode multiple messages from a single byte stream" in {
    val msg1  = Json.obj("id" -> 1.asJson, "result" -> "ok".asJson)
    val msg2  = Json.obj("id" -> 2.asJson, "result" -> "done".asJson)
    val bytes = LspFramer.encode(msg1) ++ LspFramer.encode(msg2)
    val result = fs2.Stream
      .chunk(fs2.Chunk.array(bytes))
      .through(LspFramer.decode)
      .compile
      .toList
      .unsafeRunSync()

    result shouldBe List(msg1, msg2)
  }

  it should "decode multi-byte UTF-8 messages split across byte chunks" in {
    val msg1  = Json.obj("id" -> 1.asJson, "result" -> "héllo".asJson)
    val msg2  = Json.obj("id" -> 2.asJson, "result" -> "done".asJson)
    val bytes = LspFramer.encode(msg1) ++ LspFramer.encode(msg2)
    val result = fs2.Stream
      .emits(bytes.toSeq)
      .covary[IO]
      .through(LspFramer.decode)
      .compile
      .toList
      .unsafeRunSync()

    result shouldBe List(msg1, msg2)
  }

  "LspProtocol" should "identify responses and notifications correctly" in {
    val response     = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "result" -> Json.obj())
    val notification = Json.obj("jsonrpc" -> "2.0".asJson, "method" -> "initialized".asJson, "params" -> Json.obj())
    val request =
      Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 2.asJson, "method" -> "test".asJson, "params" -> Json.obj())

    LspProtocol.isResponse(response) shouldBe true
    LspProtocol.isNotification(response) shouldBe false
    LspProtocol.isNotification(notification) shouldBe true
    LspProtocol.isResponse(notification) shouldBe false
    LspProtocol.isResponse(request) shouldBe false
    LspProtocol.isNotification(request) shouldBe false
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
    uri shouldBe "file:///foo/Bar.scala"
    diags should have size 1
    diags.head.message shouldBe "type mismatch"
    diags.head.severity shouldBe Some(DiagnosticSeverity.Error)
    diags.head.range.start.line shouldBe 5
    diags.head.source shouldBe Some("metals")
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
    val textDocumentCapabilities = params.hcursor.downField("capabilities").downField("textDocument")
    textDocumentCapabilities.downField("hover").succeeded shouldBe true
    textDocumentCapabilities.downField("definition").succeeded shouldBe true
    textDocumentCapabilities.downField("completion").succeeded shouldBe true
  }

  it should "build didOpen params with correct structure" in {
    val params = LspProtocol.didOpenParams("file:///foo/Bar.scala", "scala", 1, "object Bar")
    val td     = params.hcursor.downField("textDocument")
    td.downField("uri").as[String].toOption shouldBe Some("file:///foo/Bar.scala")
    td.downField("languageId").as[String].toOption shouldBe Some("scala")
    td.downField("version").as[Int].toOption shouldBe Some(1)
    td.downField("text").as[String].toOption shouldBe Some("object Bar")
  }

  it should "build full-text didChange params with the document version" in {
    val params = LspProtocol.didChangeParams("file:///foo/Bar.scala", 2, "object Updated")
    val td     = params.hcursor.downField("textDocument")

    td.downField("uri").as[String].toOption shouldBe Some("file:///foo/Bar.scala")
    td.downField("version").as[Int].toOption shouldBe Some(2)
    params.hcursor.downField("contentChanges").downArray.downField("text").as[String].toOption shouldBe
      Some("object Updated")
  }

  it should "build hover, definition, and completion params from document positions" in {
    val hover      = LspProtocol.hoverParams("file:///foo/Bar.scala", line = 7, character = 4)
    val definition = LspProtocol.definitionParams("file:///foo/Bar.scala", line = 8, character = 2)
    val completion = LspProtocol.completionParams("file:///foo/Bar.scala", line = 9, character = 6)

    hover.hcursor.downField("textDocument").downField("uri").as[String].toOption shouldBe Some("file:///foo/Bar.scala")
    hover.hcursor.downField("position").downField("line").as[Int].toOption shouldBe Some(7)
    hover.hcursor.downField("position").downField("character").as[Int].toOption shouldBe Some(4)

    definition.hcursor.downField("position").downField("line").as[Int].toOption shouldBe Some(8)
    definition.hcursor.downField("position").downField("character").as[Int].toOption shouldBe Some(2)

    completion.hcursor.downField("position").downField("line").as[Int].toOption shouldBe Some(9)
    completion.hcursor.downField("position").downField("character").as[Int].toOption shouldBe Some(6)
  }

  it should "parse hover text from markup content responses" in {
    val response = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> 2.asJson,
      "result" -> Json.obj(
        "contents" -> Json.obj(
          "kind"  -> "markdown".asJson,
          "value" -> "```scala\nmap[B](f: A => B): List[B]\n```".asJson
        )
      )
    )

    LspProtocol.parseHoverText(response) shouldBe Some("```scala\nmap[B](f: A => B): List[B]\n```")
  }

  it should "parse hover text from marked string arrays" in {
    val response = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> 3.asJson,
      "result" -> Json.obj(
        "contents" -> Json.arr(
          "List.map".asJson,
          Json.obj("language" -> "scala".asJson, "value" -> "def map[B](f: A => B): List[B]".asJson)
        )
      )
    )

    LspProtocol.parseHoverText(response) shouldBe Some("List.map\n\ndef map[B](f: A => B): List[B]")
  }

  it should "parse the first definition location from object or array responses" in {
    val location = Json.obj(
      "uri" -> "file:///foo/Bar.scala".asJson,
      "range" -> Json.obj(
        "start" -> Json.obj("line" -> 7.asJson, "character" -> 4.asJson),
        "end"   -> Json.obj("line" -> 7.asJson, "character" -> 10.asJson)
      )
    )

    val objectResponse = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 4.asJson, "result" -> location)
    val arrayResponse  = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 5.asJson, "result" -> Json.arr(location))

    LspProtocol.parseDefinitionLocation(objectResponse).map(_.uri) shouldBe Some("file:///foo/Bar.scala")
    LspProtocol.parseDefinitionLocation(arrayResponse).map(_.range.start.line) shouldBe Some(7)
    LspProtocol.parseDefinitionLocation(arrayResponse).map(_.range.start.character) shouldBe Some(4)
  }

  it should "parse completion candidates from completion lists, arrays, and empty results" in {
    val candidates = Json.arr(
      Json.obj("label" -> "map".asJson),
      Json.obj("label" -> "mapValues".asJson)
    )
    val completionList = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> 6.asJson,
      "result"  -> Json.obj("isIncomplete" -> false.asJson, "items" -> candidates)
    )
    val arrayResponse = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> 7.asJson,
      "result"  -> candidates
    )
    val emptyResponse = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> 8.asJson,
      "result"  -> Json.arr()
    )

    LspProtocol.parseCompletionItems(completionList) shouldBe Some(List("map", "mapValues"))
    LspProtocol.parseCompletionItems(arrayResponse) shouldBe Some(List("map", "mapValues"))
    LspProtocol.parseCompletionItems(emptyResponse) shouldBe Some(Nil)
  }
