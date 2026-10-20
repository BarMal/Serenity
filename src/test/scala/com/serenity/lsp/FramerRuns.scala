package com.serenity.lsp

import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.lsp.client.LspFramer
import com.serenity.lsp.client.LspFramer.{FrameDecoder, FrameError}
import fs2.Chunk
import io.circe.Json

/** Drives the decoders over a list of chunks the way a stream would, for the specs and the benchmark. */
object FramerRuns:

  type Outcome = Either[FrameError, (List[Json], FrameDecoder)]

  /** Feeds every chunk in turn, then signals the end of the stream. */
  def pure(chunks: Seq[Array[Byte]]): Outcome =
    chunks
      .foldLeft[Outcome](Right((Nil, FrameDecoder.empty))) {
        case (decoded, bytes) =>
          decoded
            .flatMap((frames, decoder) => decoder.feed(Chunk.array(bytes)).map((more, next) => (frames ::: more, next)))
      }
      .flatMap((frames, decoder) => decoder.finish.as((frames, decoder)))

  /** The same run through the public `fs2.Pipe`, failing with whatever the stream failed with. */
  def piped(chunks: Seq[Array[Byte]]): Either[Throwable, List[Json]] =
    fs2.Stream
      .emits(chunks)
      .map(Chunk.array(_))
      .flatMap(fs2.Stream.chunk)
      .through(LspFramer.decode)
      .compile
      .toList
      .attempt
      .unsafeRunSync()

  /** The mutable decoder this one replaced, run the same way; its failure is the message of what it threw. */
  def legacy(chunks: Seq[Array[Byte]]): Either[String, List[Json]] =
    val decoder = LegacyFraming.LegacyFrameDecoder()
    scala.util
      .Try {
        val frames = chunks.toList.flatMap(bytes => decoder.feed(Chunk.array(bytes)))
        decoder.finish()
        frames
      }
      .toEither
      .left
      .map(_.getMessage)
