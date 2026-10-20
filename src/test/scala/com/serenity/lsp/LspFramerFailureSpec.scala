package com.serenity.lsp

import java.nio.charset.StandardCharsets

import cats.effect.unsafe.implicits.global
import com.serenity.lsp.client.LspFramer
import com.serenity.lsp.client.LspFramer.FrameError
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every way a frame stream can be malformed is a `FrameError`, JSON that does not parse included. */
class LspFramerFailureSpec extends AnyFlatSpec with Matchers:

  private def failure(text: String): Throwable =
    fs2.Stream
      .chunk(fs2.Chunk.array(text.getBytes(StandardCharsets.UTF_8)))
      .through(LspFramer.decode)
      .compile
      .toList
      .attempt
      .unsafeRunSync()
      .left
      .getOrElse(fail("Expected frame decoding to fail"))

  "LspFramer.decode" should "report an unparseable JSON body as a FrameError naming the body" in {
    val error = failure("Content-Length: 5\r\n\r\n{nope")

    error shouldBe a[FrameError]
    error.getMessage should include("JSON parse error")
    error.getMessage should include("{nope")
  }
