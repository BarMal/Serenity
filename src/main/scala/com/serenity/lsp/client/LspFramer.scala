package com.serenity.lsp.client

import java.nio.charset.StandardCharsets

import scala.collection.mutable.ListBuffer

import cats.effect.IO
import fs2.{Chunk, Stream}
import io.circe.Json

object LspFramer:

  private val HeaderSepBytes = Array('\r'.toByte, '\n'.toByte, '\r'.toByte, '\n'.toByte)
  private val LengthKey      = "Content-Length"

  private[lsp] val MaxHeaderBytes = 16 * 1024
  private[lsp] val MaxBodyBytes   = 16 * 1024 * 1024

  final private[lsp] case class FrameError(message: String) extends RuntimeException(message)

  private enum ParseState:
    case Header
    case Body(length: Int)

  def encode(json: Json): Array[Byte] =
    val body        = json.noSpaces.getBytes(StandardCharsets.UTF_8)
    val header      = s"$LengthKey: ${body.length}\r\n\r\n"
    val headerBytes = header.getBytes(StandardCharsets.UTF_8)
    headerBytes ++ body

  def decode: fs2.Pipe[IO, Byte, Json] =
    in => decodeStream(in)

  private def decodeStream(stream: Stream[IO, Byte]): Stream[IO, Json] =
    stream.through(messageFrames).through(parseJson)

  private def messageFrames: fs2.Pipe[IO, Byte, String] =
    in =>
      Stream.eval(IO(new FrameDecoder)).flatMap { decoder =>
        in.chunks.evalMap(chunk => IO.delay(decoder.feed(chunk))).flatMap(Stream.emits) ++
          Stream.eval(IO.delay(decoder.finish())).flatMap(Stream.emits)
      }

  // This bounded buffer is local to one decode stream and avoids copying accumulated frames per chunk.
  // scalafix:off DisableSyntax
  final private class FrameDecoder:

    private var buffer         = new Array[Byte](1024)
    private var offset         = 0
    private var size           = 0
    private var headerScanFrom = 0
    private var state          = ParseState.Header

    def feed(chunk: Chunk[Byte]): List[String] =
      val frames = ListBuffer.empty[String]
      chunk.foreach { byte =>
        append(byte)
        drain(frames)
      }
      frames.toList

    def finish(): List[String] =
      state match
        case ParseState.Header if size == 0 => Nil
        case ParseState.Header =>
          throw FrameError("Truncated LSP frame at EOF: incomplete header")
        case ParseState.Body(length) =>
          throw FrameError(s"Truncated LSP frame at EOF: truncated body, expected $length bytes but received $size")

    private def append(byte: Byte): Unit =
      state match
        case ParseState.Header if size >= MaxHeaderBytes + HeaderSepBytes.length =>
          throw FrameError(s"LSP frame header exceeds $MaxHeaderBytes bytes")
        case _ =>
          ensureCapacity(size + 1)
          buffer(offset + size) = byte
          size += 1

    private def drain(frames: ListBuffer[String]): Unit =
      var canContinue = true
      while canContinue do
        state match
          case ParseState.Header =>
            findHeaderEnd() match
              case None =>
                if size > MaxHeaderBytes + HeaderSepBytes.length - 1 then
                  throw FrameError(s"LSP frame header exceeds $MaxHeaderBytes bytes")
                canContinue = false
              case Some(headerEnd) =>
                if headerEnd > MaxHeaderBytes then throw FrameError(s"LSP frame header exceeds $MaxHeaderBytes bytes")
                val bodyLength = parseBodyLength(headerEnd)
                discard(headerEnd + HeaderSepBytes.length)
                state = ParseState.Body(bodyLength)
          case ParseState.Body(length) =>
            if size < length then canContinue = false
            else
              frames += new String(buffer, offset, length, StandardCharsets.UTF_8)
              discard(length)
              state = ParseState.Header

    private def findHeaderEnd(): Option[Int] =
      var index = headerScanFrom
      while index <= size - HeaderSepBytes.length do
        if HeaderSepBytes.indices.forall(offsetInSeparator =>
              buffer(offset + index + offsetInSeparator) == HeaderSepBytes(offsetInSeparator)
            )
        then return Some(index)
        index += 1
      headerScanFrom = math.max(0, size - HeaderSepBytes.length + 1)
      None

    private def parseBodyLength(headerEnd: Int): Int =
      val header = new String(buffer, offset, headerEnd, StandardCharsets.US_ASCII)
      val fields = header.split("\\r\\n", -1).toList.map { line =>
        val separator = line.indexOf(':')
        if separator <= 0 then throw FrameError(s"Malformed LSP header: $line")
        line.substring(0, separator).trim -> line.substring(separator + 1).trim
      }
      val lengths = fields.collect { case (name, value) if name.equalsIgnoreCase(LengthKey) => value }

      lengths match
        case Nil => throw FrameError("Malformed LSP header: missing Content-Length")
        case value :: Nil =>
          if value.isEmpty || !value.forall(char => char >= '0' && char <= '9') then
            throw FrameError(s"Invalid Content-Length: $value")
          val length = BigInt(value)
          if length > MaxBodyBytes then throw FrameError(s"LSP frame body exceeds $MaxBodyBytes bytes")
          length.toInt
        case _ => throw FrameError("Malformed LSP header: multiple Content-Length values")

    private def discard(count: Int): Unit =
      offset += count
      size -= count
      headerScanFrom = 0
      if size == 0 then offset = 0

    private def ensureCapacity(required: Int): Unit =
      if offset + required <= buffer.length then ()
      else if required <= buffer.length then
        System.arraycopy(buffer, offset, buffer, 0, size)
        offset = 0
      else
        val capacity = math.max(required, buffer.length * 2)
        val expanded = new Array[Byte](capacity)
        System.arraycopy(buffer, offset, expanded, 0, size)
        buffer = expanded
        offset = 0

  // scalafix:on DisableSyntax

  private def parseJson: fs2.Pipe[IO, String, Json] =
    _.evalMap(s =>
      IO.fromEither(io.circe.parser.parse(s).left.map(e => new RuntimeException(s"JSON parse error: $e in: $s")))
    )
