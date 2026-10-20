package com.serenity.io

import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.{ByteBuffer, CharBuffer}

import scala.util.Try

import com.serenity.rope.{Balance, Rope, RopeChunks}
import com.serenity.text.{LineEnding, TextEncoding}

/** A text file's content with the BOM stripped, plus what saving needs to reproduce its bytes. */
final case class DecodedText(content: String, encoding: TextEncoding, hasBom: Boolean)

/** The bytes a save writes, with the encoding and BOM they were actually written in. */
final case class EncodedText(bytes: Array[Byte], encoding: TextEncoding, hasBom: Boolean)

/** Lossless decoding and encoding of text files (#1627). Every decode is strict: a byte sequence the encoding cannot
  * represent moves on to the next candidate instead of becoming U+FFFD, since a replacement character saved back is the
  * original byte destroyed.
  */
object TextFileCodec:

  /** A NUL this early is taken as binary content. Only applied without a UTF-16 BOM, where NULs are ordinary. */
  val BinarySniffLength: Int = 8192

  /** Tried in order on a file with no BOM. Windows-1252 follows UTF-8 because it is what most "Latin-1" text really is;
    * it leaves five bytes unmapped, so [[LastResortEncoding]] catches what it rejects.
    */
  val FallbackOrder: List[TextEncoding] = List(TextEncoding.Utf8, TextEncoding.Windows1252)

  /** Maps every byte to a character, so decoding with it always succeeds and always round-trips. */
  val LastResortEncoding: TextEncoding = TextEncoding.Iso88591

  /** `None` when `bytes` look binary. */
  def decode(bytes: Array[Byte]): Option[DecodedText] =
    decodeWithByteOrderMark(bytes).orElse(Option.unless(looksBinary(bytes))(decodeWithoutByteOrderMark(bytes)))

  /** `bytes` read as `encoding` because the user chose it, not detected -- `None` when they are not valid in it. A
    * leading byte order mark of that encoding is stripped and remembered.
    */
  def decodeAs(bytes: Array[Byte], encoding: TextEncoding): Option[DecodedText] =
    val hasBom = encoding.byteOrderMark.nonEmpty && bytes.startsWith(encoding.byteOrderMark.toArray)
    strictDecode(if hasBom then bytes.drop(encoding.byteOrderMark.length) else bytes, encoding)
      .map(DecodedText(_, encoding, hasBom))

  /** The policy for content its file's encoding cannot represent (#1627): write it as UTF-8 without a BOM rather than
    * lose the characters, and report the switch in the result. Refusing the save instead would look like a save that
    * happened, because failed saves are not shown to the user yet (#1717); once they are, a prompt can replace this.
    */
  def encodeOrFallBackToUtf8(content: String, encoding: TextEncoding, hasBom: Boolean): EncodedText =
    encode(content, encoding, hasBom)
      .map(EncodedText(_, encoding, hasBom))
      .getOrElse(EncodedText(content.getBytes(StandardCharsets.UTF_8), TextEncoding.Utf8, hasBom = false))

  /** `None` when `content` holds a character `encoding` cannot represent. */
  def encode(content: String, encoding: TextEncoding, hasBom: Boolean): Option[Array[Byte]] =
    Try(
      encoding.charset
        .newEncoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .encode(CharBuffer.wrap(content))
    ).toOption.map { buffer =>
      val body = Array.tabulate(buffer.remaining())(index => buffer.get(buffer.position() + index))
      (if hasBom then encoding.byteOrderMark.toArray else Array.emptyByteArray) ++ body
    }

  /** [[decode]], straight into a rope with the terminators counted on the way, so the content is never a `String`. */
  def decodeRope(bytes: Array[Byte])(using Balance): Option[DecodedRope] =
    decodeRopeWithByteOrderMark(bytes).orElse(
      if looksBinary(bytes) then None else decodeRopeWithoutByteOrderMark(bytes)
    )

  /** [[decodeAs]], straight into a rope. */
  def decodeRopeAs(bytes: Array[Byte], encoding: TextEncoding)(using Balance): Option[DecodedRope] =
    val hasBom = encoding.byteOrderMark.nonEmpty && bytes.startsWith(encoding.byteOrderMark.toArray)
    ropeAs(bytes, encoding, hasBom)

  /** [[encodeOrFallBackToUtf8]] for editor content held as a rope, written with `lineEnding` for each `\n`. The text is
    * passed through the encoder a leaf at a time rather than joined, replaced and converted as whole strings.
    */
  def encodeRopeOrFallBackToUtf8(
    content: Rope,
    lineEnding: LineEnding,
    encoding: TextEncoding,
    hasBom: Boolean
  ): EncodedText =
    encodeRope(content, lineEnding, encoding, hasBom)
      .map(EncodedText(_, encoding, hasBom))
      .getOrElse(
        EncodedText(
          encodeRope(content, lineEnding, TextEncoding.Utf8, hasBom = false)
            .getOrElse(lineEnding.applyTo(content.collect()).getBytes(StandardCharsets.UTF_8)),
          TextEncoding.Utf8,
          hasBom = false
        )
      )

  private def encodeRope(
    content: Rope,
    lineEnding: LineEnding,
    encoding: TextEncoding,
    hasBom: Boolean
  ): Option[Array[Byte]] =
    ChunkedTextCodec.encode(
      RopeChunks.of(content),
      lineEnding.sequence,
      encoding,
      if hasBom then encoding.byteOrderMark.toArray else Array.emptyByteArray
    )

  private def ropeAs(bytes: Array[Byte], encoding: TextEncoding, hasBom: Boolean)(using Balance): Option[DecodedRope] =
    ChunkedTextCodec
      .decode(bytes, if hasBom then encoding.byteOrderMark.length else 0, encoding)
      .map((content, endings) => DecodedRope(content, endings, encoding, hasBom))

  private def decodeRopeWithByteOrderMark(bytes: Array[Byte])(using Balance): Option[DecodedRope] =
    TextEncoding.values
      .find(encoding => encoding.byteOrderMark.nonEmpty && bytes.startsWith(encoding.byteOrderMark.toArray))
      .flatMap(encoding => ropeAs(bytes, encoding, hasBom = true))

  private def decodeRopeWithoutByteOrderMark(bytes: Array[Byte])(using Balance): Option[DecodedRope] =
    (FallbackOrder :+ LastResortEncoding).iterator
      .flatMap(encoding => ropeAs(bytes, encoding, hasBom = false))
      .nextOption()

  private def decodeWithByteOrderMark(bytes: Array[Byte]): Option[DecodedText] =
    TextEncoding.values
      .find(encoding => encoding.byteOrderMark.nonEmpty && bytes.startsWith(encoding.byteOrderMark.toArray))
      .flatMap(encoding => decodeAfterByteOrderMark(bytes, encoding))

  private def decodeAfterByteOrderMark(bytes: Array[Byte], encoding: TextEncoding): Option[DecodedText] =
    strictDecode(bytes.drop(encoding.byteOrderMark.length), encoding).map(DecodedText(_, encoding, hasBom = true))

  private def decodeWithoutByteOrderMark(bytes: Array[Byte]): DecodedText =
    FallbackOrder.iterator
      .flatMap(encoding => strictDecode(bytes, encoding).map(DecodedText(_, encoding, hasBom = false)))
      .nextOption()
      .getOrElse(DecodedText(new String(bytes, LastResortEncoding.charset), LastResortEncoding, hasBom = false))

  private def looksBinary(bytes: Array[Byte]): Boolean =
    bytes.iterator.take(BinarySniffLength).contains(0.toByte)

  private def strictDecode(bytes: Array[Byte], encoding: TextEncoding): Option[String] =
    Try(
      encoding.charset
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .decode(ByteBuffer.wrap(bytes))
        .toString
    ).toOption
