package com.serenity.lsp

import java.nio.charset.StandardCharsets

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.client.{
  DocumentUri,
  JsonRpcMessage,
  LspFramer,
  LspMethod,
  LspProtocol,
  RequestId,
  WorkspaceRootUri
}
import com.serenity.lsp.model.{DiagnosticSeverity, LspPosition, LspRange, SemanticToken, SemanticTokensLegend}
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LspProtocolSpec extends AnyFlatSpec with Matchers:

  private def decode(bytes: Array[Byte]): List[Json] =
    fs2.Stream
      .chunk(fs2.Chunk.array(bytes))
      .through(LspFramer.decode)
      .compile
      .toList
      .unsafeRunSync()

  private def decodeFailure(bytes: Array[Byte]): Throwable =
    fs2.Stream
      .chunk(fs2.Chunk.array(bytes))
      .through(LspFramer.decode)
      .compile
      .toList
      .attempt
      .unsafeRunSync()
      .left
      .getOrElse(fail("Expected frame decoding to fail"))

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

  it should "decode a frame when its header and body split at every byte boundary" in {
    val message = Json.obj("id" -> 1.asJson, "result" -> "héllo".asJson)
    val bytes   = LspFramer.encode(message)

    (0 to bytes.length).foreach { splitAt =>
      val result = (fs2.Stream.chunk(fs2.Chunk.array(bytes.take(splitAt))) ++
        fs2.Stream.chunk(fs2.Chunk.array(bytes.drop(splitAt))))
        .through(LspFramer.decode)
        .compile
        .toList
        .unsafeRunSync()

      result shouldBe List(message)
    }
  }

  it should "reject missing, negative, invalid, and oversized Content-Length values" in {
    val invalidFrames = List(
      "Content-Type: application/vscode-jsonrpc\r\n\r\n{}"     -> "missing Content-Length",
      "Content-Length 1\r\n\r\n{}"                             -> "Malformed LSP header",
      "Content-Length: -1\r\n\r\n{}"                           -> "Invalid Content-Length",
      "Content-Length: nope\r\n\r\n{}"                         -> "Invalid Content-Length",
      s"Content-Length: ${LspFramer.MaxBodyBytes + 1}\r\n\r\n" -> "body exceeds"
    )

    invalidFrames.foreach {
      case (frame, expectedMessage) =>
        val failure = decodeFailure(frame.getBytes(StandardCharsets.UTF_8))
        failure.getMessage should include(expectedMessage)
    }
  }

  it should "reject headers that exceed the configured bound" in {
    val failure = decodeFailure(Array.fill(LspFramer.MaxHeaderBytes + 4)('x'.toByte))

    failure.getMessage should include("header exceeds")
  }

  it should "report EOF during an incomplete header or body" in {
    val incompleteHeader = decodeFailure("Content-Len".getBytes(StandardCharsets.UTF_8))
    val incompleteBody   = decodeFailure("Content-Length: 10\r\n\r\n{}".getBytes(StandardCharsets.UTF_8))

    incompleteHeader.getMessage should include("incomplete header")
    incompleteBody.getMessage should include("truncated body")
  }

  "LspProtocol.classify" should "classify responses, error responses, notifications, and unrecognized messages" in {
    val response = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "result" -> Json.obj("ok" -> true.asJson))
    val errorResponse = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> 9.asJson,
      "error"   -> Json.obj("code" -> (-32601).asJson, "message" -> "Method not found".asJson)
    )
    val notification = Json.obj("jsonrpc" -> "2.0".asJson, "method" -> "initialized".asJson, "params" -> Json.obj())
    val request =
      Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 2.asJson, "method" -> "test".asJson, "params" -> Json.obj())

    LspProtocol.classify(response) shouldBe JsonRpcMessage.Response(RequestId(1), Json.obj("ok" -> true.asJson))
    LspProtocol.classify(errorResponse) shouldBe JsonRpcMessage.ResponseError(RequestId(9), -32601, "Method not found")
    LspProtocol.classify(notification) shouldBe JsonRpcMessage.Notification(LspMethod("initialized"), Json.obj())
    LspProtocol.classify(request) shouldBe a[JsonRpcMessage.Malformed]
  }

  it should "surface a JSON-RPC error response instead of silently treating it as an absent result" in {
    val errorResponse = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> 4.asJson,
      "error"   -> Json.obj("code" -> (-32602).asJson, "message" -> "Invalid params".asJson)
    )

    LspProtocol.classify(errorResponse) match
      case JsonRpcMessage.ResponseError(id, code, message) =>
        id shouldBe RequestId(4)
        code shouldBe -32602
        message shouldBe "Invalid params"
      case other => fail(s"Expected a ResponseError, got $other")
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
    uri shouldBe DocumentUri("file:///foo/Bar.scala")
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
    result shouldBe Some((DocumentUri("file:///foo/Clean.scala"), Nil))
  }

  it should "build initialize params with processId and rootUri" in {
    val params = LspProtocol.initializeParams(12345, WorkspaceRootUri("file:///workspace"))
    params.hcursor.downField("processId").as[Int].toOption shouldBe Some(12345)
    params.hcursor.downField("rootUri").as[String].toOption shouldBe Some("file:///workspace")
    val textDocumentCapabilities = params.hcursor.downField("capabilities").downField("textDocument")
    textDocumentCapabilities.downField("hover").succeeded shouldBe true
    textDocumentCapabilities.downField("definition").succeeded shouldBe true
    textDocumentCapabilities.downField("completion").succeeded shouldBe true
  }

  it should "build didOpen params with correct structure" in {
    val params = LspProtocol.didOpenParams(DocumentUri("file:///foo/Bar.scala"), "scala", 1, "object Bar")
    val td     = params.hcursor.downField("textDocument")
    td.downField("uri").as[String].toOption shouldBe Some("file:///foo/Bar.scala")
    td.downField("languageId").as[String].toOption shouldBe Some("scala")
    td.downField("version").as[Int].toOption shouldBe Some(1)
    td.downField("text").as[String].toOption shouldBe Some("object Bar")
  }

  it should "build full-text didChange params with the document version" in {
    val params = LspProtocol.didChangeParams(DocumentUri("file:///foo/Bar.scala"), 2, "object Updated")
    val td     = params.hcursor.downField("textDocument")

    td.downField("uri").as[String].toOption shouldBe Some("file:///foo/Bar.scala")
    td.downField("version").as[Int].toOption shouldBe Some(2)
    params.hcursor.downField("contentChanges").downArray.downField("text").as[String].toOption shouldBe
      Some("object Updated")
  }

  it should "build hover, definition, and completion params from document positions" in {
    val hover      = LspProtocol.hoverParams(DocumentUri("file:///foo/Bar.scala"), line = 7, character = 4)
    val definition = LspProtocol.definitionParams(DocumentUri("file:///foo/Bar.scala"), line = 8, character = 2)
    val completion = LspProtocol.completionParams(DocumentUri("file:///foo/Bar.scala"), line = 9, character = 6)

    hover.hcursor.downField("textDocument").downField("uri").as[String].toOption shouldBe Some("file:///foo/Bar.scala")
    hover.hcursor.downField("position").downField("line").as[Int].toOption shouldBe Some(7)
    hover.hcursor.downField("position").downField("character").as[Int].toOption shouldBe Some(4)

    definition.hcursor.downField("position").downField("line").as[Int].toOption shouldBe Some(8)
    definition.hcursor.downField("position").downField("character").as[Int].toOption shouldBe Some(2)

    completion.hcursor.downField("position").downField("line").as[Int].toOption shouldBe Some(9)
    completion.hcursor.downField("position").downField("character").as[Int].toOption shouldBe Some(6)
  }

  it should "parse hover text from markup content responses" in {
    // These are already-unwrapped `result` values -- as `LspConnection.handleIncomingJson`/`classify` hands them to
    // callers, not the raw top-level response envelope.
    val result = Json.obj(
      "contents" -> Json.obj(
        "kind"  -> "markdown".asJson,
        "value" -> "```scala\nmap[B](f: A => B): List[B]\n```".asJson
      )
    )

    LspProtocol.parseHoverText(result) shouldBe Some("```scala\nmap[B](f: A => B): List[B]\n```")
  }

  it should "parse hover text from marked string arrays" in {
    val result = Json.obj(
      "contents" -> Json.arr(
        "List.map".asJson,
        Json.obj("language" -> "scala".asJson, "value" -> "def map[B](f: A => B): List[B]".asJson)
      )
    )

    LspProtocol.parseHoverText(result) shouldBe Some("List.map\n\ndef map[B](f: A => B): List[B]")
  }

  it should "parse the first definition location from object or array results" in {
    val location = Json.obj(
      "uri" -> "file:///foo/Bar.scala".asJson,
      "range" -> Json.obj(
        "start" -> Json.obj("line" -> 7.asJson, "character" -> 4.asJson),
        "end"   -> Json.obj("line" -> 7.asJson, "character" -> 10.asJson)
      )
    )

    val objectResult = location
    val arrayResult  = Json.arr(location)

    LspProtocol.parseDefinitionLocation(objectResult).map(_.uri) shouldBe Some(DocumentUri("file:///foo/Bar.scala"))
    LspProtocol.parseDefinitionLocation(arrayResult).map(_.range.start.line) shouldBe Some(7)
    LspProtocol.parseDefinitionLocation(arrayResult).map(_.range.start.character) shouldBe Some(4)
  }

  it should "parse completion candidates from completion lists, arrays, and empty results" in {
    val candidates = Json.arr(
      Json.obj("label" -> "map".asJson),
      Json.obj("label" -> "mapValues".asJson)
    )
    val completionList = Json.obj("isIncomplete" -> false.asJson, "items" -> candidates)
    val arrayResult    = candidates
    val emptyResult    = Json.arr()

    LspProtocol.parseCompletionItems(completionList) shouldBe Some(List("map", "mapValues"))
    LspProtocol.parseCompletionItems(arrayResult) shouldBe Some(List("map", "mapValues"))
    LspProtocol.parseCompletionItems(emptyResult) shouldBe Some(Nil)
  }

  it should "parse a range's 0-based start and end positions, including an all-zero range" in {
    val json = Json.obj(
      "range" -> Json.obj(
        "start" -> Json.obj("line" -> 0.asJson, "character" -> 0.asJson),
        "end"   -> Json.obj("line" -> 3.asJson, "character" -> 12.asJson)
      )
    )

    LspProtocol.parseRange(json.hcursor) shouldBe Some(LspRange(LspPosition(0, 0), LspPosition(3, 12)))
  }

  it should "return None when the range is missing or a coordinate field is absent" in {
    val noRange = Json.obj("uri" -> "file:///foo/Bar.scala".asJson)
    LspProtocol.parseRange(noRange.hcursor) shouldBe None

    val missingEndCharacter = Json.obj(
      "range" -> Json.obj(
        "start" -> Json.obj("line" -> 1.asJson, "character" -> 2.asJson),
        "end"   -> Json.obj("line" -> 1.asJson)
      )
    )
    LspProtocol.parseRange(missingEndCharacter.hcursor) shouldBe None
  }

  // ── SemanticTokens (issue #859 / #1177) ──────────────────────────────────────

  it should "declare a semanticTokens client capability with a full-request legend" in {
    val params  = LspProtocol.initializeParams(12345, WorkspaceRootUri("file:///workspace"))
    val semTok  = params.hcursor.downField("capabilities").downField("textDocument").downField("semanticTokens")
    semTok.downField("requests").downField("full").as[Boolean].toOption shouldBe Some(true)
    semTok.downField("tokenTypes").as[List[String]].toOption shouldBe Some(LspProtocol.ClientSemanticTokenTypes)
    semTok.downField("tokenModifiers").as[List[String]].toOption shouldBe Some(LspProtocol.ClientSemanticTokenModifiers)
  }

  it should "build semanticTokens/full params from the document uri" in {
    val params = LspProtocol.semanticTokensParams(DocumentUri("file:///foo/Bar.scala"))
    params.hcursor.downField("textDocument").downField("uri").as[String].toOption shouldBe Some("file:///foo/Bar.scala")
  }

  it should "parse the server's semantic tokens legend from its initialize result" in {
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

    LspProtocol.parseSemanticTokensLegend(initializeResult) shouldBe
      Some(SemanticTokensLegend(List("keyword", "string"), List("readonly")))
    LspProtocol.supportsSemanticTokens(initializeResult) shouldBe true
  }

  it should "report no semantic tokens support when the server omits the capability" in {
    val initializeResult = Json.obj("capabilities" -> Json.obj("hoverProvider" -> true.asJson))

    LspProtocol.parseSemanticTokensLegend(initializeResult) shouldBe None
    LspProtocol.supportsSemanticTokens(initializeResult) shouldBe false
  }

  it should "decode delta-encoded semantic tokens data into absolute positions using the legend" in {
    val legend = SemanticTokensLegend(
      tokenTypes = List("keyword", "string", "comment"),
      tokenModifiers = List("declaration", "readonly")
    )
    // Token 1: line 0, char 0, length 3, type "keyword" (index 0), modifiers none (bitset 0)
    // Token 2: same line (deltaLine 0), so char is relative to token 1's start: +4 -> char 4, length 5, type
    //   "string" (index 1), modifiers {declaration, readonly} (bits 0 and 1 set -> 0b11 = 3)
    // Token 3: next line (deltaLine 2 -> line 2), char is absolute on the new line: 1, length 10, type "comment"
    //   (index 2), modifiers none
    val data = List(
      0, 0, 3, 0, 0,
      0, 4, 5, 1, 3,
      2, 1, 10, 2, 0
    )
    val result = Json.obj("data" -> data.map(_.asJson).asJson)

    LspProtocol.parseSemanticTokens(result, legend) shouldBe Some(
      List(
        SemanticToken(line = 0, startCharacter = 0, length = 3, tokenType = "keyword", tokenModifiers = Set.empty),
        SemanticToken(
          line = 0,
          startCharacter = 4,
          length = 5,
          tokenType = "string",
          tokenModifiers = Set("declaration", "readonly")
        ),
        SemanticToken(line = 2, startCharacter = 1, length = 10, tokenType = "comment", tokenModifiers = Set.empty)
      )
    )
  }

  it should "decode an empty semantic tokens data array as no tokens" in {
    val legend = SemanticTokensLegend(List("keyword"), Nil)
    val result = Json.obj("data" -> Json.arr())

    LspProtocol.parseSemanticTokens(result, legend) shouldBe Some(Nil)
  }

  it should "return None when a semantic tokens result has no data field" in {
    val legend = SemanticTokensLegend(List("keyword"), Nil)
    LspProtocol.parseSemanticTokens(Json.obj(), legend) shouldBe None
  }

  it should "skip a token whose type index falls outside the legend rather than fail the whole decode" in {
    val legend = SemanticTokensLegend(List("keyword"), Nil)
    // First token has an out-of-range type index (5); second is valid.
    val data   = List(0, 0, 3, 5, 0, 0, 4, 2, 0, 0)
    val result = Json.obj("data" -> data.map(_.asJson).asJson)

    LspProtocol.parseSemanticTokens(result, legend) shouldBe Some(
      List(SemanticToken(line = 0, startCharacter = 4, length = 2, tokenType = "keyword", tokenModifiers = Set.empty))
    )
  }
