package com.serenity.io

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets

import com.serenity.rope.{Balance, Rope, RopeChunks}
import com.serenity.text.{LineEnding, LineEndingCounts, TextEncoding}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1910, #1867: a file is decoded straight into rope leaves and written back by streaming them through the encoder,
  * byte-exactly, without a whole-document `String` in between.
  */
class ChunkedTextCodecSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val chunk = Balance.default.leafChunkSize

  private def bytesOf(text: String, encoding: TextEncoding, withBom: Boolean): Array[Byte] =
    (if withBom then encoding.byteOrderMark.toArray else Array.emptyByteArray) ++ text.getBytes(encoding.charset)

  private def roundTrip(original: Array[Byte]): (DecodedRope, Array[Byte]) =
    val decoded = TextFileCodec.decodeRope(original).getOrElse(fail("expected text"))
    val saved = TextFileCodec.encodeRopeOrFallBackToUtf8(
      decoded.content,
      decoded.lineEndings.dominant,
      decoded.encoding,
      decoded.hasBom
    )
    (decoded, saved.bytes)

  private val samples: List[(String, String)] = List(
    "LF"                   -> "alpha\nbeta\n\ngamma",
    "CRLF"                 -> "alpha\r\nbeta\r\n\r\ngamma\r\n",
    "CR"                   -> "alpha\rbeta\rgamma\r",
    "text with astral"     -> "café 🙂\nnaïve\n",
    "no terminator at all" -> "single line",
    "empty"                -> ""
  )

  private val encodings: List[(String, TextEncoding, Boolean)] = List(
    ("UTF-8", TextEncoding.Utf8, false),
    ("UTF-8 with BOM", TextEncoding.Utf8, true),
    ("UTF-16LE with BOM", TextEncoding.Utf16Le, true),
    ("UTF-16BE with BOM", TextEncoding.Utf16Be, true)
  )

  for
    (encodingName, encoding, withBom) <- encodings
    (sampleName, sample)              <- samples
    if !(sample.isEmpty && !withBom)
  do
    s"A $encodingName file with $sampleName" should "save back byte-identically and decode to normalised content" in {
      val original     = bytesOf(sample, encoding, withBom)
      val (decoded, _) = roundTrip(original)
      decoded.content.collect() shouldBe Rope(sample).collect()
      decoded.lineEndings shouldBe LineEndingCounts.of(sample)
      decoded.encoding shouldBe encoding
      decoded.hasBom shouldBe withBom
      roundTrip(original)._2.toSeq shouldBe original.toSeq
    }

  "A Latin-1 file" should "round-trip byte-identically" in {
    val original         = "café\r\nnaïve\r\n".getBytes(StandardCharsets.ISO_8859_1)
    val (decoded, saved) = roundTrip(original)
    decoded.encoding shouldBe TextEncoding.Windows1252
    saved.toSeq shouldBe original.toSeq
  }

  "A mixed file" should "count every terminator and save with the dominant one" in {
    val original         = "a\r\nb\r\nc\nd\re".getBytes(StandardCharsets.UTF_8)
    val (decoded, saved) = roundTrip(original)
    decoded.lineEndings shouldBe LineEndingCounts(lf = 1, crlf = 2, cr = 1)
    decoded.content.collect() shouldBe "a\nb\nc\nd\ne"
    new String(saved, StandardCharsets.UTF_8) shouldBe "a\r\nb\r\nc\r\nd\r\ne"
  }

  "A CRLF split by a chunk boundary" should "still be one terminator" in {
    val text    = "x" * (chunk - 1) + "\r\n" + "y" * 10
    val decoded = TextFileCodec.decodeRope(text.getBytes(StandardCharsets.UTF_8)).getOrElse(fail("expected text"))
    decoded.lineEndings shouldBe LineEndingCounts(lf = 0, crlf = 1, cr = 0)
    decoded.content.collect() shouldBe "x" * (chunk - 1) + "\n" + "y" * 10
  }

  "A surrogate pair split by a chunk boundary" should "survive load and save" in {
    val text             = "x" * (chunk - 1) + "🙂" + "tail"
    val original         = bytesOf(text, TextEncoding.Utf8, withBom = false)
    val (decoded, saved) = roundTrip(original)
    decoded.content.collect() shouldBe text
    saved.toSeq shouldBe original.toSeq
  }

  private val smile = "🙂"

  "A rope whose leaf boundary falls inside a surrogate pair" should "encode that pair intact" in {
    val rope  = com.serenity.rope.RopeBuilder.fromLeaves(List("ab" + smile.take(1), smile.drop(1) + "cd"))
    val saved = TextFileCodec.encodeRopeOrFallBackToUtf8(rope, LineEnding.Lf, TextEncoding.Utf8, hasBom = false)
    new String(saved.bytes, StandardCharsets.UTF_8) shouldBe "ab🙂cd"
  }

  "Bytes invalid in the chosen encoding" should "not decode as it" in {
    val invalid = Array[Byte](0x61, 0xc3.toByte)
    TextFileCodec.decodeRopeAs(invalid, TextEncoding.Utf8) shouldBe None
  }

  "Content the file's encoding cannot represent" should "be written as UTF-8 instead" in {
    val saved = TextFileCodec.encodeRopeOrFallBackToUtf8(
      Rope("café 🙂\n"),
      LineEnding.Lf,
      TextEncoding.Iso88591,
      hasBom = false
    )
    saved.encoding shouldBe TextEncoding.Utf8
    new String(saved.bytes, StandardCharsets.UTF_8) shouldBe "café 🙂\n"
  }

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported => Some(bean)
    case _                                                                              => None

  private def allocatedBy(work: () => Any): Option[Long] =
    allocationBean.map { bean =>
      val id    = Thread.currentThread().threadId()
      val start = bean.getThreadAllocatedBytes(id)
      work()
      bean.getThreadAllocatedBytes(id) - start
    }

  private val megabytes = 8
  private val sample =
    ("The quick brown fox jumps over the lazy dog, café naïve.\r\n" * 1000).getBytes(StandardCharsets.UTF_8)
  private val large = Array.tabulate(megabytes * 1024 * 1024 / sample.length)(_ => sample).flatten

  "Loading a file" should "allocate a bounded number of bytes per input byte" in {
    allocatedBy(() => TextFileCodec.decodeRope(large)) match
      case Some(bytes) =>
        withClue(s"${bytes.toDouble / large.length} bytes per byte: ")(bytes should be < large.length.toLong * 10)
      case None => info("per-thread allocation counter unsupported -- skipping")
  }

  "Saving a document" should "allocate a bounded number of bytes per output byte" in {
    val decoded = TextFileCodec.decodeRope(large).getOrElse(fail("expected text"))
    allocatedBy(() =>
      TextFileCodec.encodeRopeOrFallBackToUtf8(decoded.content, LineEnding.Crlf, TextEncoding.Utf8, hasBom = false)
    ) match
      case Some(bytes) =>
        withClue(s"${bytes.toDouble / large.length} bytes per byte: ")(bytes should be < large.length.toLong * 6)
      case None => info("per-thread allocation counter unsupported -- skipping")
  }

  "LineEnding.detect" should "not allocate per character" in {
    val text = new String(large, StandardCharsets.UTF_8)
    allocatedBy(() => LineEnding.detect(text)) match
      case Some(bytes) => bytes should be < 64L * 1024
      case None        => info("per-thread allocation counter unsupported -- skipping")
  }

  "The rope of a decoded file" should "hold the same leaves the encoder writes" in {
    val decoded = TextFileCodec.decodeRope(large).getOrElse(fail("expected text"))
    RopeChunks.of(decoded.content).mkString shouldBe decoded.content.collect()
  }
