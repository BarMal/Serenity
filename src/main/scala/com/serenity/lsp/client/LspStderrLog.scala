package com.serenity.lsp.client

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption, StandardOpenOption}
import java.time.Instant

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.lsp.config.LanguageId
import fs2.io.readInputStream
import org.typelevel.log4cats.Logger

/** Where a language server's stderr goes. Servers such as metals, rust-analyzer and pyright log there continuously, and
  * a pipe nobody reads fills at the OS buffer size and blocks the server mid-write, so every request after that times
  * out.
  *
  * The output is kept in `lsp-<language>.log` under the log directory, which is where a user looks when a server
  * misbehaves. It is bounded: once the live file would pass the cap it becomes `lsp-<language>.log.1`, replacing the
  * previous one, so a server never occupies more than twice the cap on disk. Successive server starts append to the
  * same file, each introduced by a header line, so what a crashed server said survives its restart.
  */
private[lsp] object LspStderrLog:

  val DefaultMaxBytes: Long = 512L * 1024

  private val ReadChunkBytes = 8192

  def defaultDirectory: Path = Paths.get(System.getProperty("user.home"), ".serenity", "logs")

  def pathFor(directory: Path, languageId: LanguageId): Path =
    directory.resolve(s"lsp-${languageId.id}.log")

  def rotatedPathFor(directory: Path, languageId: LanguageId): Path =
    directory.resolve(s"lsp-${languageId.id}.log.1")

  /** A `None` output means the log could not be opened or written: the stream is still read and discarded, because an
    * unread pipe is the failure this exists to prevent.
    */
  final private case class Segment(output: Option[OutputStream], written: Long)

  /** Reads `stderr` to its end into the language's log, whatever becomes of the log. */
  def drain(
    stderr: InputStream,
    languageId: LanguageId,
    directory: Path,
    maxBytes: Long,
    logger: Logger[IO]
  ): IO[Unit] =
    val live    = pathFor(directory, languageId)
    val rotated = rotatedPathFor(directory, languageId)
    val header  = s"--- ${Instant.now()} ${languageId.displayName} language server started ---\n"

    def open: IO[Segment] =
      IO.blocking {
        Files.createDirectories(directory)
        val existing = if Files.exists(live) then Files.size(live) else 0L
        Segment(Some(Files.newOutputStream(live, StandardOpenOption.CREATE, StandardOpenOption.APPEND)), existing)
      }.handleErrorWith(error =>
        logger
          .warn(error)(s"[LSP] ${languageId.id} stderr is not being logged: cannot open $live")
          .as(
            Segment(None, 0L)
          )
      )

    def close(segment: Segment): IO[Unit] =
      segment.output.traverse_(output => IO.blocking(output.close()).attempt.void)

    def rotate(segment: Segment): IO[Segment] =
      close(segment) >>
        IO.blocking(Files.move(live, rotated, StandardCopyOption.REPLACE_EXISTING)).attempt >>
        IO.blocking(Files.deleteIfExists(live)).attempt >>
      open

    def append(segment: Segment, bytes: Array[Byte]): IO[Segment] =
      segment.output match
        case None => IO.pure(segment)
        case Some(_) =>
          val overflows = segment.written > 0 && segment.written + bytes.length > maxBytes
          (if overflows then rotate(segment) else IO.pure(segment)).flatMap(write(_, bytes))

    def write(segment: Segment, bytes: Array[Byte]): IO[Segment] =
      segment.output.fold(IO.pure(segment)) { output =>
        IO.blocking {
          output.write(bytes)
          output.flush()
          segment.copy(written = segment.written + bytes.length)
        }.handleErrorWith(error =>
          logger.warn(error)(s"[LSP] ${languageId.id} stderr log write failed; the rest is discarded") >>
            close(segment).as(Segment(None, 0L))
        )
      }

    val started = open.flatMap(append(_, header.getBytes(StandardCharsets.UTF_8))).flatMap(Ref.of[IO, Segment])

    Resource.make(started)(_.get.flatMap(close)).use { current =>
      readInputStream(IO.pure(stderr), ReadChunkBytes, closeAfterUse = false).chunks
        .evalMap(chunk => current.get.flatMap(append(_, chunk.toArray)).flatMap(current.set))
        .compile
        .drain
    }
