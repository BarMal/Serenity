package com.serenity.lsp

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import scala.collection.mutable.ListBuffer

import com.serenity.lsp.client.LspFramer
import fs2.Chunk
import io.circe.Json

/** The mutable decoder `LspFramer` used before it was made pure, kept verbatim as the baseline the differential
  * property test and the ABBA benchmark compare the pure decoder against.
  */
object LegacyFraming:
  private val HeaderSepBytes = Array('\r'.toByte, '\n'.toByte, '\r'.toByte, '\n'.toByte)
  private val LengthKey      = "Content-Length"

  /** Incremental frame decoder for one stream. Each chunk is appended in bulk; the header terminator is searched from
    * the previous scan position, and a pending body is waited for without rescanning.
    */
  // This bounded buffer is local to one decode stream and avoids copying accumulated frames per chunk.
  // scalafix:off DisableSyntax
  final class LegacyFrameDecoder:

    private val NoBody = -1

    private var buffer         = new Array[Byte](1024)
    private var offset         = 0
    private var size           = 0
    private var headerScanFrom = 0
    private var bodyLength     = NoBody
    private var scans          = 0

    def headerScans: Int = scans

    def feed(chunk: Chunk[Byte]): List[Json] =
      val frames = ListBuffer.empty[Json]
      if chunk.nonEmpty then
        ensureCapacity(size + chunk.size)
        chunk.copyToArray(buffer, offset + size)
        size += chunk.size
        drain(frames)
      frames.toList

    def finish(): List[Json] =
      if bodyLength != NoBody then
        throw LspFramer.FrameError(
          s"Truncated LSP frame at EOF: truncated body, expected $bodyLength bytes but received $size"
        )
      else if size != 0 then throw LspFramer.FrameError("Truncated LSP frame at EOF: incomplete header")
      else Nil

    private def drain(frames: ListBuffer[Json]): Unit =
      var canContinue = true
      while canContinue do
        if bodyLength == NoBody then
          findHeaderEnd() match
            case None =>
              if size > LspFramer.MaxHeaderBytes + HeaderSepBytes.length - 1 then
                throw LspFramer.FrameError(s"LSP frame header exceeds $LspFramer.MaxHeaderBytes bytes")
              canContinue = false
            case Some(headerEnd) =>
              if headerEnd > LspFramer.MaxHeaderBytes then
                throw LspFramer.FrameError(s"LSP frame header exceeds $LspFramer.MaxHeaderBytes bytes")
              val length = parseBodyLength(headerEnd)
              discard(headerEnd + HeaderSepBytes.length)
              bodyLength = length
        else if size < bodyLength then canContinue = false
        else
          frames += parseJson(bodyLength)
          discard(bodyLength)
          bodyLength = NoBody

    private def findHeaderEnd(): Option[Int] =
      if size < HeaderSepBytes.length then None
      else
        scans += 1
        val last  = size - HeaderSepBytes.length
        var index = headerScanFrom
        var found = -1
        while found < 0 && index <= last do
          if buffer(offset + index) == HeaderSepBytes(0) && buffer(offset + index + 1) == HeaderSepBytes(1) &&
              buffer(offset + index + 2) == HeaderSepBytes(2) && buffer(offset + index + 3) == HeaderSepBytes(3)
          then found = index
          index += 1
        if found >= 0 then Some(found)
        else
          headerScanFrom = math.max(0, size - HeaderSepBytes.length + 1)
          None

    private def parseBodyLength(headerEnd: Int): Int =
      val header = new String(buffer, offset, headerEnd, StandardCharsets.US_ASCII)
      val fields = header.split("\\r\\n", -1).toList.map { line =>
        val separator = line.indexOf(':')
        if separator <= 0 then throw LspFramer.FrameError(s"Malformed LSP header: $line")
        line.substring(0, separator).trim -> line.substring(separator + 1).trim
      }
      val lengths = fields.collect { case (name, value) if name.equalsIgnoreCase(LengthKey) => value }

      lengths match
        case Nil => throw LspFramer.FrameError("Malformed LSP header: missing Content-Length")
        case value :: Nil =>
          if value.isEmpty || !value.forall(char => char >= '0' && char <= '9') then
            throw LspFramer.FrameError(s"Invalid Content-Length: $value")
          val length = BigInt(value)
          if length > LspFramer.MaxBodyBytes then
            throw LspFramer.FrameError(s"LSP frame body exceeds $LspFramer.MaxBodyBytes bytes")
          length.toInt
        case _ => throw LspFramer.FrameError("Malformed LSP header: multiple Content-Length values")

    private def parseJson(length: Int): Json =
      io.circe.jawn.parseByteBuffer(ByteBuffer.wrap(buffer, offset, length)) match
        case Right(json) => json
        case Left(failure) =>
          val text = new String(buffer, offset, length, StandardCharsets.UTF_8)
          throw new RuntimeException(s"JSON parse error: $failure in: $text")

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
end LegacyFraming
