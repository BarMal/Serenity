package com.serenity.io

import java.nio.charset.{CharsetDecoder, CharsetEncoder, CodingErrorAction}
import java.nio.{ByteBuffer, CharBuffer}

import scala.annotation.tailrec

import com.serenity.rope.{Balance, Rope, RopeBuilder}
import com.serenity.text.{LineEndingCounts, TextEncoding}

/** Text decoded straight into a rope, with the terminators it arrived with counted on the way. */
final case class DecodedRope(
    content: Rope,
    lineEndings: LineEndingCounts,
    encoding: TextEncoding,
    hasBom: Boolean
)

/** Strict, chunk-at-a-time decoding and encoding (#1910, #1867): the file's characters pass through one rope leaf's
  * worth of buffer at a time, so neither direction holds a second copy of the document as a `String`.
  */
private[io] object ChunkedTextCodec:

  /** `bytes` from `offset` decoded as `encoding`, with CRLF and CR turned into LF as they go; `None` when a byte
    * sequence is invalid in `encoding`.
    */
  def decode(bytes: Array[Byte], offset: Int, encoding: TextEncoding)(using
    balance: Balance
  ): Option[(Rope, LineEndingCounts)] =
    val input   = ByteBuffer.wrap(bytes, offset, bytes.length - offset)
    val decoder = strictDecoder(encoding)
    val output  = CharBuffer.allocate(balance.leafChunkSize)

    @tailrec
    def fill(leaves: Vector[String], seen: Terminators, flushing: Boolean): Option[(Vector[String], Terminators)] =
      val _               = output.clear()
      val result          = if flushing then decoder.flush(output) else decoder.decode(input, output, true)
      val _               = output.flip()
      val (leaf, counted) = normalised(output, seen)
      val grown           = if leaf.isEmpty then leaves else leaves :+ leaf
      if result.isError then None
      else if result.isOverflow then fill(grown, counted, flushing)
      else if flushing then Some((grown, counted))
      else fill(grown, counted, flushing = true)

    fill(Vector.empty, Terminators.none, flushing = false).map((leaves, seen) =>
      (RopeBuilder.fromLeaves(leaves), seen.counts)
    )

  /** `chunks` encoded as `encoding`, each `\n` written as `lineEnding` after `bom`; `None` when a character is
    * unmappable in `encoding`. The bytes are assembled once, at the end, from chunk-sized pieces.
    */
  def encode(
    chunks: Iterator[String],
    lineEnding: String,
    encoding: TextEncoding,
    bom: Array[Byte]
  ): Option[Array[Byte]] =
    val encoder = strictEncoder(encoding)
    val output  = ByteBuffer.allocate(OutputChunkBytes)

    @tailrec
    def drain(input: CharBuffer, endOfInput: Boolean, pieces: Vector[Array[Byte]]): Option[Vector[Array[Byte]]] =
      val _      = output.clear()
      val result = encoder.encode(input, output, endOfInput)
      val _      = output.flip()
      val grown  = pieces :+ bytesOf(output)
      if result.isError then None
      else if result.isOverflow then drain(input, endOfInput, grown)
      else Some(grown)

    @tailrec
    def run(carry: String, pieces: Vector[Array[Byte]]): Option[Vector[Array[Byte]]] =
      if chunks.hasNext then
        val input = CharBuffer.wrap(carry + withLineEnding(chunks.next(), lineEnding))
        drain(input, endOfInput = false, pieces) match
          case None        => None
          case Some(grown) => run(input.toString, grown)
      else
        val input = CharBuffer.wrap(carry)
        drain(input, endOfInput = true, pieces).flatMap(finish(encoder, output, _))

    run("", Vector(bom)).map(pieces => concatenated(pieces))

  private val OutputChunkBytes = 64 * 1024

  private def withLineEnding(chunk: String, lineEnding: String): String =
    if lineEnding == "\n" then chunk else chunk.replace("\n", lineEnding)

  private def finish(
    encoder: CharsetEncoder,
    output: ByteBuffer,
    pieces: Vector[Array[Byte]]
  ): Option[Vector[Array[Byte]]] =
    @tailrec
    def flush(gathered: Vector[Array[Byte]]): Option[Vector[Array[Byte]]] =
      val _      = output.clear()
      val result = encoder.flush(output)
      val _      = output.flip()
      val grown  = gathered :+ bytesOf(output)
      if result.isOverflow then flush(grown) else Some(grown)
    flush(pieces)

  private def bytesOf(buffer: ByteBuffer): Array[Byte] =
    val piece = new Array[Byte](buffer.remaining())
    val _     = buffer.get(piece)
    piece

  private def concatenated(pieces: Vector[Array[Byte]]): Array[Byte] =
    val joined = new Array[Byte](pieces.foldLeft(0)((total, piece) => total + piece.length))
    val _ = pieces.foldLeft(0) { (at, piece) =>
      System.arraycopy(piece, 0, joined, at, piece.length)
      at + piece.length
    }
    joined

  private def strictDecoder(encoding: TextEncoding): CharsetDecoder =
    encoding.charset
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)

  private def strictEncoder(encoding: TextEncoding): CharsetEncoder =
    encoding.charset
      .newEncoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)

  /** Terminators counted so far; `endsInCr` is true while the last character seen was a CR counted as a lone CR, which
    * a following LF in the next chunk turns into a CRLF.
    */
  final private case class Terminators(lf: Int, crlf: Int, cr: Int, endsInCr: Boolean):
    def counts: LineEndingCounts = LineEndingCounts(lf, crlf, cr)

  private object Terminators:
    val none: Terminators = Terminators(0, 0, 0, endsInCr = false)

  /** The decoded `chunk` with every terminator as `\n`, and the counts including it. */
  private def normalised(chunk: CharBuffer, before: Terminators): (String, Terminators) =
    val length = chunk.remaining()
    val source = new Array[Char](length)
    val _      = chunk.get(source)
    val target = new Array[Char](length)

    @tailrec
    def loop(from: Int, to: Int, seen: Terminators): (Int, Terminators) =
      if from >= length then (to, seen)
      else
        source(from) match
          case '\n' if from == 0 && before.endsInCr =>
            loop(1, to, seen.copy(cr = seen.cr - 1, crlf = seen.crlf + 1))
          case '\n' =>
            target(to) = '\n'
            loop(from + 1, to + 1, seen.copy(lf = seen.lf + 1))
          case '\r' =>
            target(to) = '\n'
            if from + 1 < length && source(from + 1) == '\n' then
              loop(from + 2, to + 1, seen.copy(crlf = seen.crlf + 1))
            else loop(from + 1, to + 1, seen.copy(cr = seen.cr + 1))
          case other =>
            target(to) = other
            loop(from + 1, to + 1, seen)

    val (written, counted) = loop(0, 0, before.copy(endsInCr = false))
    val endsInCr           = if length == 0 then before.endsInCr else source(length - 1) == '\r'
    (new String(target, 0, written), counted.copy(endsInCr = endsInCr))
