package com.serenity

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.FileManager
import com.serenity.rope.Balance
import com.serenity.session.SessionBuffer
import com.serenity.state.models.{Buffer, BufferId, Document}
import com.serenity.text.LineEnding
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Whether opening a file and saving it back returns the bytes it arrived with. A CRLF-delimited file that comes back
  * LF-delimited has been rewritten on every line by an operation the user asked nothing of.
  */
class LineEndingRoundTripSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def withTempFile(name: String, bytes: Array[Byte])(check: Path => Unit): Unit =
    val directory = Files.createTempDirectory("serenity-line-endings")
    val path      = directory.resolve(name)
    Files.write(path, bytes)
    try check(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory)

  "Opening and saving a file" should "preserve CRLF line endings" in {
    val original = "first\r\nsecond\r\nthird\r\n"
    withTempFile("crlf.txt", original.getBytes(StandardCharsets.UTF_8)) { path =>
      val fileManager = new FileManager()
      val roundTripped = (for
        buffer <- fileManager.loadFile(path, BufferId(1))
        _      <- fileManager.saveBuffer(buffer, path)
        bytes  <- IO.blocking(Files.readAllBytes(path))
      yield new String(bytes, StandardCharsets.UTF_8)).unsafeRunSync()

      roundTripped.shouldBe(original)
    }
  }

  it should "preserve LF line endings" in {
    val original = "first\nsecond\nthird\n"
    withTempFile("lf.txt", original.getBytes(StandardCharsets.UTF_8)) { path =>
      val fileManager = new FileManager()
      val roundTripped = (for
        buffer <- fileManager.loadFile(path, BufferId(2))
        _      <- fileManager.saveBuffer(buffer, path)
        bytes  <- IO.blocking(Files.readAllBytes(path))
      yield new String(bytes, StandardCharsets.UTF_8)).unsafeRunSync()

      roundTripped.shouldBe(original)
    }
  }

  it should "keep a CRLF file's endings across an edit, not just an untouched round trip" in {
    val original = "first\r\nsecond\r\n"
    withTempFile("edited.txt", original.getBytes(StandardCharsets.UTF_8)) { path =>
      val fileManager = new FileManager()
      val roundTripped = (for
        buffer <- fileManager.loadFile(path, BufferId(3))
        edited = buffer.copy(document =
          buffer.document.copy(content = com.serenity.rope.Rope("first\nsecond\nthird\n"))
        )
        _     <- fileManager.saveBuffer(edited, path)
        bytes <- IO.blocking(Files.readAllBytes(path))
      yield new String(bytes, StandardCharsets.UTF_8)).unsafeRunSync()

      roundTripped.shouldBe("first\r\nsecond\r\nthird\r\n")
    }
  }

  "Detection" should "read a file with no terminator at all as the platform-neutral default" in
    LineEnding.detect("no newline here").shouldBe(LineEnding.Lf)

  it should "let the majority win a mixed file, since there is no ending that reproduces it" in {
    LineEnding.detect("a\r\nb\r\nc\n").shouldBe(LineEnding.Crlf)
    LineEnding.detect("a\r\nb\nc\n").shouldBe(LineEnding.Lf)
  }

  it should "not mistake a lone CR inside CRLF for a separate LF" in
    LineEnding.detect("a\r\nb\r\n").shouldBe(LineEnding.Crlf)

  "A session round trip" should "carry the line ending, so restore does not rewrite the file" in {
    val buffer = Buffer(
      id = BufferId(4),
      document = Document(
        content = com.serenity.rope.Rope("a\nb\n"),
        filePath = Some(java.nio.file.Paths.get("notes.txt")),
        lineEnding = LineEnding.Crlf
      )
    )
    val restored = SessionBuffer.toBuffer(SessionBuffer.fromBuffer(buffer))
    restored.document.lineEnding.shouldBe(LineEnding.Crlf)
  }
