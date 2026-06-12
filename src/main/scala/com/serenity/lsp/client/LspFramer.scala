package com.serenity.lsp.client

import java.nio.charset.StandardCharsets

import cats.effect.IO
import fs2.{Chunk, Stream}
import io.circe.Json

object LspFramer:

  private val HeaderSep      = "\r\n\r\n"
  private val LengthKey      = "Content-Length: "
  private val HeaderSepBytes = HeaderSep.getBytes(StandardCharsets.UTF_8).toVector

  def encode(json: Json): Array[Byte] =
    val body        = json.noSpaces.getBytes(StandardCharsets.UTF_8)
    val header      = s"$LengthKey${body.length}$HeaderSep"
    val headerBytes = header.getBytes(StandardCharsets.UTF_8)
    headerBytes ++ body

  def decode: fs2.Pipe[IO, Byte, Json] =
    in => decodeStream(in)

  private def decodeStream(stream: Stream[IO, Byte]): Stream[IO, Json] =
    stream.through(messageFrames).through(parseJson)

  private def messageFrames: fs2.Pipe[IO, Byte, String] =
    in =>
      in.scanChunks(Vector.empty[Byte]) { (buffer, chunk) =>
        val accumulated           = buffer ++ chunk.toList
        val (remaining, messages) = extractMessages(accumulated)
        (remaining, Chunk.from(messages))
      }

  private def extractMessages(buffer: Vector[Byte]): (Vector[Byte], List[String]) =
    val sepIdx = buffer.sliding(HeaderSepBytes.length).indexWhere(_ == HeaderSepBytes)
    if sepIdx < 0 then (buffer, Nil)
    else
      val header   = new String(buffer.take(sepIdx).toArray, StandardCharsets.UTF_8)
      val afterSep = buffer.drop(sepIdx + HeaderSepBytes.length)
      val lengthOpt = header.linesIterator
        .find(_.startsWith(LengthKey))
        .flatMap(_.stripPrefix(LengthKey).trim.toIntOption)

      lengthOpt match
        case Some(length) =>
          if afterSep.length >= length then
            val body                       = new String(afterSep.take(length).toArray, StandardCharsets.UTF_8)
            val remaining                  = afterSep.drop(length)
            val (restBuffer, restMessages) = extractMessages(remaining)
            (restBuffer, body :: restMessages)
          else (buffer, Nil)
        case None =>
          (buffer, Nil)

  private def parseJson: fs2.Pipe[IO, String, Json] =
    _.evalMap(s =>
      IO.fromEither(io.circe.parser.parse(s).left.map(e => new RuntimeException(s"JSON parse error: $e in: $s")))
    )
