package com.serenity.lsp

import java.nio.charset.StandardCharsets

import com.serenity.lsp.client.LspFramer
import com.serenity.lsp.client.LspFramer.FrameError
import io.circe.Json
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Decoding does not depend on where the transport cut the bytes, and malformed input is a `FrameError`, whatever the
  * cuts, never anything else.
  */
class LspFramerPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with EitherValues:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val text: Gen[String] =
    Gen
      .listOf(Gen.oneOf(Gen.alphaNumChar.map(_.toString), Gen.oneOf("é", "日本語", "😀", " ", "\"", "\\", "\r\n\r\n")))
      .map(_.mkString)

  private val message: Gen[Json] =
    for
      id      <- Gen.choose(0, 100000)
      payload <- text
    yield Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id.asJson, "result" -> payload.asJson)

  private val messages: Gen[List[Json]] = Gen.listOf(message)

  private def split(bytes: Array[Byte], cuts: List[Int]): List[Array[Byte]] =
    val boundaries = (0 +: cuts.map(_.abs % (bytes.length + 1)).sorted :+ bytes.length).distinct
    boundaries.zip(boundaries.drop(1)).map((from, until) => bytes.slice(from, until))

  private val cuts: Gen[List[Int]] = Gen.listOf(Gen.posNum[Int])

  private def ascii(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)

  property("any chunking of a valid stream decodes to the messages that were framed") {
    forAll(messages, cuts) { (framed, cut) =>
      val chunks = split(framed.flatMap(LspFramer.encode).toArray, cut)

      FramerRuns.pure(chunks).value._1 shouldBe framed
      FramerRuns.piped(chunks).value shouldBe framed
    }
  }

  property("feeding byte by byte decodes the same messages") {
    forAll(Gen.resize(5, messages)) { framed =>
      val chunks = framed.flatMap(LspFramer.encode).map(Array(_))

      FramerRuns.pure(chunks).value._1 shouldBe framed
    }
  }

  property("empty chunks anywhere change nothing") {
    forAll(messages, cuts) { (framed, cut) =>
      val chunks = split(framed.flatMap(LspFramer.encode).toArray, cut).flatMap(chunk => List(Array.empty[Byte], chunk))

      FramerRuns.pure(chunks).value._1 shouldBe framed
    }
  }

  private val malformedHeaders: Gen[(String, String)] = Gen.oneOf(
    "Content-Type: application/vscode-jsonrpc\r\n\r\n{}"     -> "missing Content-Length",
    "Content-Length 1\r\n\r\n{}"                             -> "Malformed LSP header",
    ": 2\r\n\r\n{}"                                          -> "Malformed LSP header",
    "Content-Length: -1\r\n\r\n{}"                           -> "Invalid Content-Length",
    "Content-Length: nope\r\n\r\n{}"                         -> "Invalid Content-Length",
    "Content-Length: \r\n\r\n{}"                             -> "Invalid Content-Length",
    "Content-Length: 2\r\nContent-Length: 2\r\n\r\n{}"       -> "multiple Content-Length",
    s"Content-Length: ${LspFramer.MaxBodyBytes + 1}\r\n\r\n" -> "body exceeds",
    "Content-Length: 99999999999999999999\r\n\r\n"           -> "body exceeds",
    "Content-Length: 5\r\n\r\n{nope"                         -> "JSON parse error"
  )

  property("every malformed header is a FrameError naming the problem, for any chunking") {
    forAll(malformedHeaders, cuts) {
      case ((frame, expected), cut) =>
        val chunks = split(ascii(frame), cut)

        val failure = FramerRuns.pure(chunks).left.value
        failure.getMessage should include(expected)
        FramerRuns.piped(chunks).left.value shouldBe a[FrameError]
    }
  }

  property("a header that never ends is a FrameError once it passes the bound, for any chunking") {
    forAll(Gen.choose(LspFramer.MaxHeaderBytes + 4, LspFramer.MaxHeaderBytes + 64), cuts) { (length, cut) =>
      val chunks = split(Array.fill(length)('x'.toByte), cut)

      FramerRuns.pure(chunks).left.value.getMessage should include("header exceeds")
    }
  }

  /** Cutting `lost` bytes off the end of a framed stream is an error unless it lands exactly on a frame boundary, which
    * includes cutting everything: a stream of whole frames, or of none, is complete.
    */
  private def checkCutShort(framed: List[Json], lost: Int, cut: List[Int]): Unit =
    val frames     = framed.map(LspFramer.encode)
    val bytes      = frames.flatten.toArray
    val boundaries = frames.scanLeft(0)(_ + _.length)
    val kept       = bytes.length - lost
    val chunks     = split(bytes.take(kept), cut)

    if boundaries.contains(kept) then FramerRuns.pure(chunks).value._1 shouldBe framed.take(boundaries.indexOf(kept))
    else FramerRuns.pure(chunks).left.value.getMessage should startWith("Truncated LSP frame at EOF")

    FramerRuns.pure(chunks).map(_._1).left.map(_.getMessage) shouldBe FramerRuns.legacy(chunks)

  property("a stream cut short is a FrameError unless the cut lands on a frame boundary") {
    forAll(Gen.nonEmptyListOf(message), Gen.posNum[Int], cuts) { (framed, drop, cut) =>
      checkCutShort(framed, 1 + (drop % framed.flatMap(LspFramer.encode).length), cut)
    }
  }

  property("cutting a stream off on each frame boundary leaves the whole frames before it") {
    forAll(Gen.nonEmptyListOf(message), Gen.posNum[Int], cuts) { (framed, pick, cut) =>
      val frames = framed.map(LspFramer.encode)
      val keep   = pick % (frames.size + 1)
      checkCutShort(framed, frames.drop(keep).map(_.length).sum, cut)
    }
  }

  // Found by a property run on master: one message, and a cut of every byte.
  property("a stream cut off entirely is empty, not truncated") {
    val only = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 22570.asJson, "result" -> " ".asJson)

    checkCutShort(List(only), LspFramer.encode(only).length, Nil)
  }

  private val mutated: Gen[List[Array[Byte]]] =
    for
      framed <- messages
      bytes = framed.flatMap(LspFramer.encode).toArray
      damage <- Gen.oneOf(
        Gen.const(bytes),
        Gen.choose(0, 8).map(n => bytes.dropRight(n)),
        Gen.listOfN(3, Gen.choose(0, 255)).map(junk => bytes ++ junk.map(_.toByte)),
        if bytes.isEmpty then Gen.const(bytes)
        else
          for
            at   <- Gen.choose(0, bytes.length - 1)
            byte <- Gen.choose(0, 255)
          yield bytes.updated(at, byte.toByte)
      )
      chunks <- cuts.map(split(damage, _))
    yield chunks

  property("the pure decoder agrees with the mutable one it replaced on intact and damaged streams") {
    forAll(mutated) { chunks =>
      FramerRuns.pure(chunks).map(_._1).left.map(_.getMessage) shouldBe FramerRuns.legacy(chunks)
    }
  }
