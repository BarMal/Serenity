package com.serenity.lsp

import java.nio.charset.StandardCharsets

import scala.util.Random

import com.serenity.lsp.client.LspFramer
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LspFramerSpec extends AnyFlatSpec with Matchers with EitherValues:

  private def message(id: Int, text: String): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id.asJson, "result" -> text.asJson)

  private def run(chunks: Seq[Array[Byte]]): FramerRuns.Outcome = FramerRuns.pure(chunks)

  private def feedAll(chunks: Seq[Array[Byte]]): List[Json] =
    run(chunks).value._1

  private def splitAt(bytes: Array[Byte], cuts: Seq[Int]): List[Array[Byte]] =
    val boundaries = (0 +: cuts.sorted :+ bytes.length).distinct
    boundaries.zip(boundaries.drop(1)).toList.map { case (from, until) => bytes.slice(from, until) }

  "LspFramer.FrameDecoder" should "decode concatenated frames identically for arbitrary chunk boundaries" in {
    val messages = List(message(1, "héllo"), message(2, "日本語 payload"), message(3, ""), message(4, "x" * 300))
    val bytes    = messages.flatMap(LspFramer.encode).toArray
    val random   = new Random(1850L)

    (1 to 300).foreach { _ =>
      val cuts = List.fill(random.nextInt(12))(random.nextInt(bytes.length + 1))
      feedAll(splitAt(bytes, cuts)) shouldBe messages
    }
  }

  it should "decode three frames split at every pair of byte boundaries" in {
    val messages = List(message(1, "a"), message(2, "é"), message(3, "c"))
    val bytes    = messages.flatMap(LspFramer.encode).toArray

    (0 to bytes.length).foreach { first =>
      (first to bytes.length).foreach(second => feedAll(splitAt(bytes, List(first, second))) shouldBe messages)
    }
  }

  it should "decode every frame carried by a single chunk" in {
    val messages = (1 to 50).toList.map(id => message(id, s"payload-$id"))
    val bytes    = messages.flatMap(LspFramer.encode).toArray

    feedAll(List(bytes)) shouldBe messages
  }

  it should "accept lower-case header names and a Content-Type header in any order" in {
    val body    = """{"id":7}"""
    val length  = body.getBytes(StandardCharsets.UTF_8).length
    val framing = s"Content-Type: application/vscode-jsonrpc; charset=utf-8\r\ncontent-length: $length\r\n\r\n$body"

    feedAll(List(framing.getBytes(StandardCharsets.UTF_8))) shouldBe List(Json.obj("id" -> 7.asJson))
  }

  it should "scan for the header terminator once per frame rather than once per byte" in {
    val messages = (1 to 100).toList.map(id => message(id, s"payload-$id"))
    val bytes    = messages.flatMap(LspFramer.encode).toArray

    val (decoded, decoder) = run(List(bytes)).value

    decoded shouldBe messages
    decoder.headerScans should be <= messages.length + 1
  }

  it should "scan once per chunk while a header is incomplete and not at all while a body is pending" in {
    val bytes = LspFramer.encode(message(1, "x" * 4096))

    val (decoded, decoder) = run(bytes.grouped(7).toList).value

    decoded shouldBe List(message(1, "x" * 4096))
    val headerLength = bytes.indexOfSlice("\r\n\r\n".getBytes(StandardCharsets.US_ASCII)) + 4
    decoder.headerScans should be <= (headerLength / 7 + 1)
  }

  it should "decode a 1 MB frame delivered in 8 KB chunks with a single header scan" in {
    val large = message(1, "y" * (1024 * 1024))
    val bytes = LspFramer.encode(large)

    val (decoded, decoder) = run(bytes.grouped(8 * 1024).toList).value

    decoded shouldBe List(large)
    decoder.headerScans shouldBe 1
  }

  it should "report invalid JSON bodies with the offending text" in {
    val framing = "Content-Length: 5\r\n\r\n{nope".getBytes(StandardCharsets.UTF_8)
    val failure = run(List(framing)).left.value

    failure.getMessage should include("JSON parse error")
    failure.getMessage should include("{nope")
  }

  "LspFramer.encode" should "frame the compact JSON body with its UTF-8 byte length" in {
    val json     = message(3, "héllo 日本")
    val body     = json.noSpaces.getBytes(StandardCharsets.UTF_8)
    val expected = s"Content-Length: ${body.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8) ++ body

    LspFramer.encode(json).toList shouldBe expected.toList
  }
