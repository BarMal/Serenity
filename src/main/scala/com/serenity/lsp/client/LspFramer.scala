package com.serenity.lsp.client

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import scala.annotation.tailrec

import cats.effect.IO
import cats.syntax.all.*
import fs2.{Chunk, Pull, Stream}
import io.circe.{Json, Printer}

object LspFramer:

  private val HeaderSepBytes = Array('\r'.toByte, '\n'.toByte, '\r'.toByte, '\n'.toByte)
  private val LengthKey      = "Content-Length"

  private[lsp] val MaxHeaderBytes = 16 * 1024
  private[lsp] val MaxBodyBytes   = 16 * 1024 * 1024

  final private[lsp] case class FrameError(message: String) extends RuntimeException(message)

  def encode(json: Json): Array[Byte] =
    val body        = Printer.noSpaces.printToByteBuffer(json, StandardCharsets.UTF_8)
    val headerBytes = s"$LengthKey: ${body.remaining}\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
    val framed      = new Array[Byte](headerBytes.length + body.remaining)
    System.arraycopy(headerBytes, 0, framed, 0, headerBytes.length)
    body.get(framed, headerBytes.length, body.remaining)
    framed

  def decode: fs2.Pipe[IO, Byte, Json] =
    in => pull(in.chunks, FrameDecoder.empty).stream

  private def pull(chunks: Stream[IO, Chunk[Byte]], decoder: FrameDecoder): Pull[IO, Json, Unit] =
    chunks.pull.uncons1.flatMap {
      case Some((chunk, rest)) =>
        decoder.feed(chunk) match
          case Left(error)           => Pull.raiseError[IO](error)
          case Right((frames, next)) => Pull.output(Chunk.from(frames)) >> pull(rest, next)
      case None =>
        decoder.finish match
          case Left(error) => Pull.raiseError[IO](error)
          case Right(_)    => Pull.done
    }

  /** A run of received bytes, sharing the array of the chunk it came from. Chunks are immutable by fs2's contract. */
  final private[lsp] case class Fragment(values: Array[Byte], offset: Int, length: Int)

  /** The state of decoding one stream: the bytes of the frame being read so far, and where it has got to. Feeding a
    * chunk returns the frames it completed with the next state; the state fed is never changed.
    *
    * A header is only joined from its fragments once its terminator has arrived, and a body only once it is complete,
    * so bytes are neither copied nor searched again as chunks arrive. Within one chunk the state is carried in the
    * loop, and a body that lies wholly inside a chunk is parsed from that chunk without a copy.
    *
    * @param received
    *   the fragments of the current frame so far, newest first
    * @param held
    *   how many bytes `received` holds
    * @param bodyLength
    *   the Content-Length of the frame whose header has been read, or [[FrameDecoder.NoBody]] while the header is still
    *   being received
    * @param matched
    *   how many bytes of the header terminator end the bytes received so far, 0 to 3
    * @param headerScans
    *   how many times a chunk has been searched for the header terminator
    */
  final private[lsp] case class FrameDecoder(
      received: List[Fragment],
      held: Int,
      bodyLength: Int,
      matched: Int,
      headerScans: Int
  ):

    def feed(chunk: Chunk[Byte]): Either[FrameError, (List[Json], FrameDecoder)] =
      if chunk.isEmpty then Right((Nil, this))
      else
        val slice = chunk.toArraySlice
        FrameDecoder.step(this, slice.values, slice.offset, slice.offset + slice.length, Nil)

    def finish: Either[FrameError, Unit] =
      if bodyLength != FrameDecoder.NoBody then
        Left(
          FrameError(
            s"Truncated LSP frame at EOF: truncated body, expected $bodyLength bytes but received $held"
          )
        )
      else if held != 0 then Left(FrameError("Truncated LSP frame at EOF: incomplete header"))
      else Right(())

  private[lsp] object FrameDecoder:
    /** A frame's body is never negative, so this says no header has been read yet without boxing an `Option`. */
    val NoBody: Int = -1

    val empty: FrameDecoder = FrameDecoder(Nil, 0, NoBody, 0, 0)

    /** Consumes `values(from until until)`, one frame at a time. */
    @tailrec
    def step(
      decoder: FrameDecoder,
      values: Array[Byte],
      from: Int,
      until: Int,
      frames: List[Json]
    ): Either[FrameError, (List[Json], FrameDecoder)] =
      if decoder.bodyLength == NoBody then
        if from == until then Right((frames.reverse, decoder))
        else
          val scans = decoder.headerScans + 1
          val found = terminatorEnd(values, from, until, decoder.matched)
          if found >= 0 then
            val received     = decoder.held + (found + 1 - from)
            val headerLength = received - HeaderSepBytes.length
            if headerLength > MaxHeaderBytes then Left(headerTooLarge)
            else
              val header = headerText(decoder, values, from, found + 1 - from, headerLength)
              parseBodyLength(header) match
                case Left(error) => Left(error)
                case Right(length) =>
                  step(FrameDecoder(Nil, 0, length, 0, scans), values, found + 1, until, frames)
          else
            val held = decoder.held + (until - from)
            if held > MaxHeaderBytes + HeaderSepBytes.length - 1 then Left(headerTooLarge)
            else
              Right(
                (
                  frames.reverse,
                  FrameDecoder(
                    Fragment(values, from, until - from) :: decoder.received,
                    held,
                    NoBody,
                    -1 - found,
                    scans
                  )
                )
              )
      else
        val missing   = decoder.bodyLength - decoder.held
        val available = until - from
        if available < missing then
          val received =
            if available == 0 then decoder.received else Fragment(values, from, available) :: decoder.received
          Right((frames.reverse, decoder.copy(received = received, held = decoder.held + available, matched = 0)))
        else
          val parsed =
            if decoder.received.isEmpty then parseJson(values, from, missing)
            else
              val body = joined(decoder, values, from, missing)
              parseJson(body, 0, body.length)
          parsed match
            case Left(error) => Left(error)
            case Right(json) =>
              step(FrameDecoder(Nil, 0, NoBody, 0, decoder.headerScans), values, from + missing, until, json :: frames)

    /** The index of the last byte of the first header terminator at or after `from`, found by walking the terminator's
      * four states with `matched` bytes of it already seen. Where there is none, `-1 - m` for the `m` bytes of it that
      * the bytes searched end with.
      */
    @tailrec
    private def terminatorEnd(values: Array[Byte], index: Int, until: Int, matched: Int): Int =
      if index == until then -1 - matched
      else
        val next = values(index) match
          case '\r' => if matched == 2 then 3 else 1
          case '\n' => if matched == 1 then 2 else if matched == 3 then 4 else 0
          case _    => 0
        if next == HeaderSepBytes.length then index else terminatorEnd(values, index + 1, until, next)

    private def headerTooLarge: FrameError = FrameError(s"LSP frame header exceeds $MaxHeaderBytes bytes")

    private def headerText(decoder: FrameDecoder, values: Array[Byte], from: Int, take: Int, length: Int): String =
      if decoder.received.isEmpty then new String(values, from, length, StandardCharsets.US_ASCII)
      else new String(joined(decoder, values, from, take), 0, length, StandardCharsets.US_ASCII)

    /** The fragments received, then `take` bytes of `values` from `from`, as one array. Filled once here, before
      * anything else sees it.
      */
    private def joined(decoder: FrameDecoder, values: Array[Byte], from: Int, take: Int): Array[Byte] =
      val out = new Array[Byte](decoder.held + take)
      val _ = decoder.received.foldLeft(decoder.held) { (end, fragment) =>
        System.arraycopy(fragment.values, fragment.offset, out, end - fragment.length, fragment.length)
        end - fragment.length
      }
      System.arraycopy(values, from, out, decoder.held, take)
      out

    private def parseBodyLength(header: String): Either[FrameError, Int] =
      val lines = splitLines(header)
      lines.find(_.indexOf(':') <= 0) match
        case Some(line) => Left(FrameError(s"Malformed LSP header: $line"))
        case None =>
          lines.flatMap { line =>
            val separator = line.indexOf(':')
            Option.when(line.substring(0, separator).trim.equalsIgnoreCase(LengthKey))(
              line.substring(separator + 1).trim
            )
          } match
            case Nil          => Left(FrameError("Malformed LSP header: missing Content-Length"))
            case value :: Nil => bodyLengthOf(value)
            case _            => Left(FrameError("Malformed LSP header: multiple Content-Length values"))

    @tailrec
    private def splitLines(text: String, from: Int = 0, lines: List[String] = Nil): List[String] =
      text.indexOf("\r\n", from) match
        case -1  => (text.substring(from) :: lines).reverse
        case end => splitLines(text, end + 2, text.substring(from, end) :: lines)

    private def bodyLengthOf(value: String): Either[FrameError, Int] =
      if value.isEmpty || !value.forall(char => char >= '0' && char <= '9') then
        Left(FrameError(s"Invalid Content-Length: $value"))
      else
        val significant = value.dropWhile(_ == '0')
        if significant.length > MaxBodyBytes.toString.length || (significant.nonEmpty && significant.toInt > MaxBodyBytes)
        then Left(FrameError(s"LSP frame body exceeds $MaxBodyBytes bytes"))
        else Right(if significant.isEmpty then 0 else significant.toInt)

    private def parseJson(values: Array[Byte], offset: Int, length: Int): Either[FrameError, Json] =
      io.circe.jawn.parseByteBuffer(ByteBuffer.wrap(values, offset, length)).leftMap { failure =>
        val text = new String(values, offset, length, StandardCharsets.UTF_8)
        FrameError(s"JSON parse error: $failure in: $text")
      }

end LspFramer
