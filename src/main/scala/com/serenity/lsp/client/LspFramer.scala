package com.serenity.lsp.client

import java.nio.charset.StandardCharsets

import cats.effect.IO
import fs2.{Chunk, Stream}
import io.circe.Json

object LspFramer:

  private val HeaderSep = "\r\n\r\n"
  private val LengthKey = "Content-Length: "

  def encode(json: Json): Array[Byte] =
    val body        = json.noSpaces.getBytes(StandardCharsets.UTF_8)
    val header      = s"$LengthKey${body.length}$HeaderSep"
    val headerBytes = header.getBytes(StandardCharsets.UTF_8)
    headerBytes ++ body

  def decode: fs2.Pipe[IO, Byte, Json] =
    in => decodeStream(in)

  private def decodeStream(stream: Stream[IO, Byte]): Stream[IO, Json] =
    stream.through(fs2.text.utf8.decode).through(messageFrames).through(parseJson)

  private def messageFrames: fs2.Pipe[IO, String, String] =
    in =>
      in.scanChunks("") { (buffer, chunk) =>
        val accumulated           = buffer + chunk.toList.mkString
        val (remaining, messages) = extractMessages(accumulated)
        (remaining, Chunk.from(messages))
      }

  private def extractMessages(buffer: String): (String, List[String]) =
    val sepIdx = buffer.indexOf(HeaderSep)
    if sepIdx < 0 then (buffer, Nil)
    else
      val header   = buffer.substring(0, sepIdx)
      val afterSep = buffer.substring(sepIdx + HeaderSep.length)
      val lengthOpt = header.linesIterator
        .find(_.startsWith(LengthKey))
        .map(_.stripPrefix(LengthKey).trim.toInt)

      lengthOpt match
        case Some(length) =>
          val bodyBytes = afterSep.getBytes(StandardCharsets.UTF_8)
          if bodyBytes.length >= length then
            val body                       = new String(bodyBytes.take(length), StandardCharsets.UTF_8)
            val remaining                  = new String(bodyBytes.drop(length), StandardCharsets.UTF_8)
            val (restBuffer, restMessages) = extractMessages(remaining)
            (restBuffer, body :: restMessages)
          else (buffer, Nil)
        case None =>
          (buffer, Nil)

  private def parseJson: fs2.Pipe[IO, String, Json] =
    _.evalMap(s =>
      IO.fromEither(io.circe.parser.parse(s).left.map(e => new RuntimeException(s"JSON parse error: $e in: $s")))
    )
