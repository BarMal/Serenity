package com.serenity.lsp.client

import cats.effect.IO
import fs2.{Chunk, Stream}
import io.circe.Json

import java.nio.charset.StandardCharsets

object LspFramer:

  private val HeaderSep  = "\r\n\r\n"
  private val LengthKey  = "Content-Length: "

  def encode(json: Json): Array[Byte] =
    val body    = json.noSpaces.getBytes(StandardCharsets.UTF_8)
    val header  = s"$LengthKey${body.length}$HeaderSep"
    val headerBytes = header.getBytes(StandardCharsets.UTF_8)
    headerBytes ++ body

  def decode: fs2.Pipe[IO, Byte, Json] =
    in => decodeStream(in)

  private def decodeStream(stream: Stream[IO, Byte]): Stream[IO, Json] =
    stream.through(fs2.text.utf8.decode).through(messageFrames).through(parseJson)

  private def messageFrames: fs2.Pipe[IO, String, String] =
    in =>
      in.scanChunks("") { (buffer, chunk) =>
        val accumulated = buffer + chunk.toList.mkString
        val messages    = scala.collection.mutable.ListBuffer.empty[String]
        var remaining   = accumulated

        var found = true
        while found do
          val sepIdx = remaining.indexOf(HeaderSep)
          if sepIdx < 0 then found = false
          else
            val header   = remaining.substring(0, sepIdx)
            val afterSep = remaining.substring(sepIdx + HeaderSep.length)
            val lengthOpt = header.linesIterator
              .find(_.startsWith(LengthKey))
              .map(_.stripPrefix(LengthKey).trim.toInt)
            lengthOpt match
              case Some(length) if afterSep.getBytes(StandardCharsets.UTF_8).length >= length =>
                val bodyBytes = afterSep.getBytes(StandardCharsets.UTF_8)
                val body      = new String(bodyBytes.take(length), StandardCharsets.UTF_8)
                messages += body
                remaining = new String(bodyBytes.drop(length), StandardCharsets.UTF_8)
              case _ =>
                found = false

        (remaining, Chunk.from(messages.toList))
      }

  private def parseJson: fs2.Pipe[IO, String, Json] =
    _.evalMap(s => IO.fromEither(io.circe.parser.parse(s).left.map(e => new RuntimeException(s"JSON parse error: $e in: $s"))))
