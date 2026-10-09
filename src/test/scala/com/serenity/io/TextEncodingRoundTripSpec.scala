package com.serenity.io

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import _root_.io.circe.syntax.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.rope.{Balance, Rope}
import com.serenity.session.SessionBuffer
import com.serenity.session.given
import com.serenity.state.models.{Buffer, BufferId, Document}
import com.serenity.text.TextEncoding
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1627 (and the BOM half of #1964): opening a file and saving it back must return the bytes it arrived with,
  * whatever its encoding, and a file that is not text must not be opened as text at all.
  */
class TextEncodingRoundTripSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val fileManager = new FileManager()

  private def withTempFile(name: String, bytes: Array[Byte])(check: Path => Unit): Unit =
    val directory = TestTemp.directory("serenity-encodings")
    val path      = directory.resolve(name)
    Files.write(path, bytes)
    try check(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory)

  private def openAndSave(path: Path, edit: Buffer => Buffer = identity): (Buffer, Array[Byte]) =
    (for
      buffer <- fileManager.loadFile(path, BufferId(1))
      _      <- fileManager.saveBuffer(edit(buffer), path)
      bytes  <- IO.blocking(Files.readAllBytes(path))
    yield buffer -> bytes).unsafeRunSync()

  private def editedTo(content: String)(buffer: Buffer): Buffer =
    buffer.copy(document = buffer.document.withContent(Rope(content)))

  "A Latin-1 file" should "open as the characters it encodes and save back byte-identically" in {
    val original = "café naïve\nÀ bientôt\n".getBytes(StandardCharsets.ISO_8859_1)
    withTempFile("latin1.txt", original) { path =>
      val (opened, saved) = openAndSave(path)
      opened.document.content.collect() shouldBe "café naïve\nÀ bientôt\n"
      saved shouldBe original
    }
  }

  it should "keep a Windows-1252-only character such as the euro sign" in {
    val original = Array[Byte](0x80.toByte, '5'.toByte, '\n'.toByte)
    withTempFile("euro.txt", original) { path =>
      val (opened, saved) = openAndSave(path)
      opened.document.content.collect() shouldBe "€5\n"
      opened.document.encoding shouldBe TextEncoding.Windows1252
      saved shouldBe original
    }
  }

  it should "fall back to ISO-8859-1 for bytes Windows-1252 leaves unmapped" in {
    val original = Array[Byte]('a'.toByte, 0x81.toByte, 0x8d.toByte, 'b'.toByte)
    withTempFile("unmapped.txt", original) { path =>
      val (opened, saved) = openAndSave(path)
      opened.document.encoding shouldBe TextEncoding.Iso88591
      saved shouldBe original
    }
  }

  it should "save as UTF-8 once it holds a character Latin-1 cannot represent, rather than lose it" in {
    val original = "café\n".getBytes(StandardCharsets.ISO_8859_1)
    withTempFile("unencodable.txt", original) { path =>
      val saved = (for
        buffer <- fileManager.loadFile(path, BufferId(2))
        saved  <- fileManager.saveBuffer(editedTo("café 🙂\n")(buffer), path)
      yield saved).unsafeRunSync()

      Files.readAllBytes(path) shouldBe "café 🙂\n".getBytes(StandardCharsets.UTF_8)
      (saved.document.encoding, saved.document.hasBom) shouldBe ((TextEncoding.Utf8, false))
    }
  }

  "A UTF-8 file with a BOM" should "not show the BOM as content, and restore it on save" in {
    val original = TextEncoding.Utf8.byteOrderMark.toArray ++ "héllo\nworld\n".getBytes(StandardCharsets.UTF_8)
    withTempFile("bom.txt", original) { path =>
      val (opened, saved) = openAndSave(path)
      opened.document.content.collect() shouldBe "héllo\nworld\n"
      opened.document.hasBom shouldBe true
      saved shouldBe original
    }
  }

  it should "keep its BOM across an edit" in {
    val original = TextEncoding.Utf8.byteOrderMark.toArray ++ "first\n".getBytes(StandardCharsets.UTF_8)
    withTempFile("bom-edited.txt", original) { path =>
      val (_, saved) = openAndSave(path, editedTo("first\nsecond\n"))
      saved shouldBe TextEncoding.Utf8.byteOrderMark.toArray ++ "first\nsecond\n".getBytes(StandardCharsets.UTF_8)
    }
  }

  "A UTF-16 file with a BOM" should "open as text rather than as binary, and save back byte-identically" in {
    val original = TextEncoding.Utf16Le.byteOrderMark.toArray ++ "hi ✓\r\n".getBytes(StandardCharsets.UTF_16LE)
    withTempFile("utf16.txt", original) { path =>
      val (opened, saved) = openAndSave(path)
      opened.document.content.collect() shouldBe "hi ✓\n"
      opened.document.encoding shouldBe TextEncoding.Utf16Le
      saved shouldBe original
    }
  }

  "A binary file" should "be refused with an explicit error instead of opened as text" in {
    val original = Array[Byte]('P'.toByte, 'K'.toByte, 3, 4, 0, 0, 'x'.toByte)
    withTempFile("archive.txt", original) { path =>
      val result = fileManager.loadFile(path, BufferId(3)).attempt.unsafeRunSync()
      result shouldBe Left(FileManagerError.BinaryContent(path))
    }
  }

  "A plain UTF-8 file" should "open and save back unchanged, recorded as UTF-8 without a BOM" in {
    val original = "héllo ✓\nplain\n".getBytes(StandardCharsets.UTF_8)
    withTempFile("plain.txt", original) { path =>
      val (opened, saved) = openAndSave(path)
      opened.document.content.collect() shouldBe "héllo ✓\nplain\n"
      opened.document.encoding shouldBe TextEncoding.Utf8
      opened.document.hasBom shouldBe false
      saved shouldBe original
    }
  }

  "A session round trip" should "carry the encoding and BOM, so a restored buffer saves the same bytes" in {
    val buffer = Buffer(
      id = BufferId(4),
      document = Document(
        content = Rope("café\n"),
        filePath = Some(java.nio.file.Paths.get("notes.txt")),
        encoding = TextEncoding.Windows1252,
        hasBom = false
      )
    )
    val bomBuffer = buffer.copy(document = buffer.document.copy(encoding = TextEncoding.Utf16Be, hasBom = true))

    val restored    = SessionBuffer.toBuffer(SessionBuffer.fromBuffer(buffer))
    val restoredBom = SessionBuffer.toBuffer(SessionBuffer.fromBuffer(bomBuffer))

    (restored.document.encoding, restored.document.hasBom) shouldBe ((TextEncoding.Windows1252, false))
    (restoredBom.document.encoding, restoredBom.document.hasBom) shouldBe ((TextEncoding.Utf16Be, true))
  }

  it should "restore a session written before encodings were recorded as UTF-8 without a BOM" in {
    val buffer = Buffer(id = BufferId(5), document = Document(content = Rope("text\n")))
    val legacyJson =
      SessionBuffer.fromBuffer(buffer).asJson.mapObject(_.remove("encoding").remove("hasBom"))
    val restored = legacyJson.as[SessionBuffer].map(SessionBuffer.toBuffer(_))

    restored.map(restoredBuffer => (restoredBuffer.document.encoding, restoredBuffer.document.hasBom)) shouldBe
      Right((TextEncoding.Utf8, false))
  }
end TextEncodingRoundTripSpec
