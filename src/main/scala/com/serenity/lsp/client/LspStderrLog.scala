package com.serenity.lsp.client

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption, StandardOpenOption}
import java.time.Instant

import scala.annotation.tailrec

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.lsp.config.LanguageId
import fs2.Stream
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
private[serenity] object LspStderrLog:

  val DefaultMaxBytes: Long = 512L * 1024

  /** Upper bound of one write. A server's stderr arrives in whatever sizes it writes (often one line at a time), and a
    * write per line is a file-system operation per line, which is slow enough on some platforms to stall the server on
    * a full pipe. Whatever has already arrived is written together instead.
    */
  private[lsp] val BatchBytes = 64 * 1024

  /** The file operations the log needs, so that their number can be observed. */
  private[lsp] trait Storage:
    def size(path: Path): Long
    def open(path: Path): OutputStream
    def replace(from: Path, to: Path): Unit
    def delete(path: Path): Unit

  private[lsp] object FileStorage extends Storage:
    def size(path: Path): Long = if Files.exists(path) then Files.size(path) else 0L

    def open(path: Path): OutputStream =
      Files.createDirectories(path.getParent)
      Files.newOutputStream(path, StandardOpenOption.CREATE, StandardOpenOption.APPEND)

    def replace(from: Path, to: Path): Unit =
      Files.move(from, to, StandardCopyOption.REPLACE_EXISTING): Unit

    def delete(path: Path): Unit = Files.deleteIfExists(path): Unit

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
    logger: Logger[IO],
    storage: Storage = FileStorage
  ): IO[Unit] =
    val live    = pathFor(directory, languageId)
    val rotated = rotatedPathFor(directory, languageId)
    val header  = s"--- ${Instant.now()} ${languageId.displayName} language server started ---\n"

    def open: IO[Segment] =
      IO.blocking {
        val existing = storage.size(live)
        Segment(Some(storage.open(live)), existing)
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
        IO.blocking(storage.replace(live, rotated)).attempt >>
        IO.blocking(storage.delete(live)).attempt >>
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
          segment.copy(written = segment.written + bytes.length)
        }.handleErrorWith(error =>
          logger.warn(error)(s"[LSP] ${languageId.id} stderr log write failed; the rest is discarded") >>
            close(segment).as(Segment(None, 0L))
        )
      }

    val started = open.flatMap(append(_, header.getBytes(StandardCharsets.UTF_8))).flatMap(Ref.of[IO, Segment])

    Resource.make(started)(_.get.flatMap(close)).use { current =>
      Stream
        .repeatEval(IO.blocking(readBatch(stderr)))
        .unNoneTerminate
        .evalMap(batch => current.get.flatMap(append(_, batch)).flatMap(current.set))
        .compile
        .drain
    }

  /** Blocks for the first bytes, then takes whatever else is already buffered without waiting for more. */
  private def readBatch(stderr: InputStream): Option[Array[Byte]] =
    val buffer = new Array[Byte](BatchBytes)

    @tailrec def fill(filled: Int): Int =
      if filled < buffer.length && stderr.available() > 0 then
        stderr.read(buffer, filled, buffer.length - filled) match
          case -1   => filled
          case read => fill(filled + read)
      else filled

    stderr.read(buffer) match
      case -1   => None
      case read => Some(java.util.Arrays.copyOf(buffer, fill(read)))
