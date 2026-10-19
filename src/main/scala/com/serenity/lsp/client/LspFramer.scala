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

  /** The state of decoding one stream: the bytes received and not yet consumed, and what is known of the frame they
    * start. Feeding a chunk returns the frames it completed with the next state; the state fed is never changed.
    *
    * Bytes are queued chunk by chunk and joined only when a header has to be searched or a body is complete, so a large
    * body that arrives in many chunks is copied once, not once per chunk.
    *
    * @param bodyLength
    *   the Content-Length of the frame whose header has been read, until that body is complete
    * @param headerScanFrom
    *   where the header terminator search resumes, so a byte is not searched twice
    * @param headerScans
    *   how many terminator searches have run
    */
  final private[lsp] case class FrameDecoder(
      buffered: Chunk.Queue[Byte],
      bodyLength: Option[Int],
      headerScanFrom: Int,
      headerScans: Int
  ):

    def feed(chunk: Chunk[Byte]): Either[FrameError, (List[Json], FrameDecoder)] =
      if chunk.isEmpty then Right((Nil, this))
      else FrameDecoder.drain(copy(buffered = buffered :+ chunk), Nil)

    def finish: Either[FrameError, Unit] =
      bodyLength match
        case Some(length) =>
          Left(
            FrameError(
              s"Truncated LSP frame at EOF: truncated body, expected $length bytes but received ${buffered.size}"
            )
          )
        case None if buffered.size != 0 => Left(FrameError("Truncated LSP frame at EOF: incomplete header"))
        case None                       => Right(())

  private[lsp] object FrameDecoder:
    val empty: FrameDecoder = FrameDecoder(Chunk.Queue.empty, None, 0, 0)

    @tailrec
    def drain(decoder: FrameDecoder, frames: List[Json]): Either[FrameError, (List[Json], FrameDecoder)] =
      decoder.bodyLength match
        case None =>
          readHeader(decoder) match
            case Left(error)                            => Left(error)
            case Right(next) if next.bodyLength.isEmpty => Right((frames.reverse, next))
            case Right(next)                            => drain(next, frames)
        case Some(length) if decoder.buffered.size < length => Right((frames.reverse, decoder))
        case Some(length) =>
          val joined = joinedBytes(decoder.buffered)
          parseJson(joined.take(length)) match
            case Left(error) => Left(error)
            case Right(json) =>
              drain(decoder.copy(buffered = queueOf(joined.drop(length)), bodyLength = None), json :: frames)

    /** The state after the header: awaiting its body when the header was complete, else still awaiting the header. */
    private def readHeader(decoder: FrameDecoder): Either[FrameError, FrameDecoder] =
      if decoder.buffered.size < HeaderSepBytes.length then Right(decoder)
      else
        val joined = joinedBytes(decoder.buffered)
        val scans  = decoder.headerScans + 1
        headerEnd(joined, decoder.headerScanFrom) match
          case None if joined.size > MaxHeaderBytes + HeaderSepBytes.length - 1 => Left(headerTooLarge)
          case None =>
            Right(
              FrameDecoder(
                queueOf(joined),
                None,
                math.max(0, joined.size - HeaderSepBytes.length + 1),
                scans
              )
            )
          case Some(end) if end > MaxHeaderBytes => Left(headerTooLarge)
          case Some(end) =>
            parseBodyLength(joined.take(end)).map(length =>
              FrameDecoder(queueOf(joined.drop(end + HeaderSepBytes.length)), Some(length), 0, scans)
            )

    private def headerTooLarge: FrameError = FrameError(s"LSP frame header exceeds $MaxHeaderBytes bytes")

    private def joinedBytes(queue: Chunk.Queue[Byte]): Chunk[Byte] =
      queue.chunks.toList match
        case Nil           => Chunk.empty
        case single :: Nil => single
        case _             => queue.compact

    private def queueOf(chunk: Chunk[Byte]): Chunk.Queue[Byte] =
      if chunk.isEmpty then Chunk.Queue.empty else Chunk.Queue(chunk)

    private def headerEnd(bytes: Chunk[Byte], from: Int): Option[Int] =
      val slice = bytes.toArraySlice
      val last  = slice.length - HeaderSepBytes.length

      @tailrec
      def search(index: Int): Option[Int] =
        if index > last then None
        else if (0 until HeaderSepBytes.length).forall(i => slice.values(slice.offset + index + i) == HeaderSepBytes(i))
        then Some(index)
        else search(index + 1)

      search(from)

    private def parseBodyLength(header: Chunk[Byte]): Either[FrameError, Int] =
      val slice = header.toArraySlice
      val text  = new String(slice.values, slice.offset, slice.length, StandardCharsets.US_ASCII)
      text
        .split("\\r\\n", -1)
        .toList
        .traverse { line =>
          val separator = line.indexOf(':')
          Either.cond(
            separator > 0,
            line.substring(0, separator).trim -> line.substring(separator + 1).trim,
            FrameError(s"Malformed LSP header: $line")
          )
        }
        .flatMap { fields =>
          fields.collect { case (name, value) if name.equalsIgnoreCase(LengthKey) => value } match
            case Nil                              => Left(FrameError("Malformed LSP header: missing Content-Length"))
            case value :: Nil if !isDigits(value) => Left(FrameError(s"Invalid Content-Length: $value"))
            case value :: Nil if BigInt(value) > MaxBodyBytes =>
              Left(FrameError(s"LSP frame body exceeds $MaxBodyBytes bytes"))
            case value :: Nil => Right(BigInt(value).toInt)
            case _            => Left(FrameError("Malformed LSP header: multiple Content-Length values"))
        }

    private def isDigits(value: String): Boolean =
      value.nonEmpty && value.forall(char => char >= '0' && char <= '9')

    private def parseJson(body: Chunk[Byte]): Either[FrameError, Json] =
      val slice = body.toArraySlice
      io.circe.jawn.parseByteBuffer(ByteBuffer.wrap(slice.values, slice.offset, slice.length)).leftMap { failure =>
        val text = new String(slice.values, slice.offset, slice.length, StandardCharsets.UTF_8)
        FrameError(s"JSON parse error: $failure in: $text")
      }

end LspFramer
