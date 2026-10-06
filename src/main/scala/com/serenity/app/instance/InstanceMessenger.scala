package com.serenity.app.instance

import java.io.{BufferedInputStream, ByteArrayOutputStream}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{Channels, ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.DurationInt
import scala.util.Try

import _root_.io.circe.Json
import cats.effect.std.Queue
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.Stream
import org.typelevel.log4cats.Logger

enum Delivery:
  case Delivered

  /** Nothing is listening yet, or any more: worth trying again shortly. */
  case Unreachable
  case Failed

/** A second launch hands the running instance the files it was asked to open over a Unix-domain socket in the config
  * directory (#2023). One JSON request line per connection, answered with an acknowledgement once it is queued.
  *
  * The request ends at its newline rather than at the sender's half-close: a half-closed AF_UNIX stream is not reliably
  * seen as end-of-input by its peer on Windows, which left the listener reading until its timeout.
  */
object InstanceMessenger:

  private val Acknowledgement   = "ok"
  private val RequestTerminator = '\n'
  private val MaxRequestBytes   = 1 << 20
  private val ConnectionTimeout = 2.seconds

  /** Only the lock holder may call this: it replaces whatever is at `socketPath`, which can only be a dead instance's.
    */
  def serve(socketPath: Path, logger: Logger[IO]): Resource[IO, Stream[IO, List[Path]]] =
    for
      _        <- Resource.make(deleteSocket(socketPath))(_ => deleteSocket(socketPath))
      server   <- Resource.fromAutoCloseable(IO.blocking(ServerSocketChannel.open(StandardProtocolFamily.UNIX)))
      _        <- Resource.eval(IO.blocking(server.bind(UnixDomainSocketAddress.of(socketPath))))
      requests <- Resource.eval(Queue.unbounded[IO, List[Path]])
      _        <- acceptOne(server, requests, logger).foreverM.background
    yield Stream.fromQueueUnterminated(requests)

  def forward(socketPath: Path, paths: List[Path]): IO[Delivery] =
    Resource
      .fromAutoCloseable(IO.blocking(SocketChannel.open(StandardProtocolFamily.UNIX)))
      .use { channel =>
        IO.interruptible(channel.connect(UnixDomainSocketAddress.of(socketPath))).attempt.flatMap {
          case Left(_) => IO.pure(Delivery.Unreachable)
          case Right(_) =>
            IO.interruptible(exchange(channel, encode(paths)))
              .map(acknowledged => if acknowledged then Delivery.Delivered else Delivery.Failed)
        }
      }
      .timeout(ConnectionTimeout)
      .handleError(_ => Delivery.Failed)

  private[instance] def encode(paths: List[Path]): String =
    Json.obj("open" -> Json.arr(paths.map(path => Json.fromString(path.toString))*)).noSpaces + RequestTerminator

  private[instance] def decode(request: String): Option[List[Path]] =
    _root_.io.circe.parser
      .parse(request)
      .flatMap(_.hcursor.get[List[String]]("open"))
      .toOption
      .flatMap(_.traverse(path => Try(Paths.get(path)).toOption))

  private def deleteSocket(socketPath: Path): IO[Unit] =
    IO.blocking(Files.deleteIfExists(socketPath)).void

  /** A misbehaving client costs only its own connection; the listener carries on. The accept sits outside the
    * `Resource` because a resource's acquire is uncancelable, which would leave shutdown waiting for the next client.
    */
  private def acceptOne(server: ServerSocketChannel, requests: Queue[IO, List[Path]], logger: Logger[IO]): IO[Unit] =
    IO.interruptible(server.accept()).flatMap { connection =>
      Resource.fromAutoCloseable(IO.pure(connection)).use(serveConnection(_, requests, logger))
    }

  private def serveConnection(
    connection: SocketChannel,
    requests: Queue[IO, List[Path]],
    logger: Logger[IO]
  ): IO[Unit] =
    IO.interruptible(readRequest(connection))
      .timeout(ConnectionTimeout)
      .flatMap {
        case Some(paths) => requests.offer(paths) >> IO.interruptible(acknowledge(connection))
        case None        => logger.warn("[INSTANCE] Ignored a malformed request on the instance socket")
      }
      .handleErrorWith(error => logger.warn(error)("[INSTANCE] Could not read a request from another launch"))

  private def readRequest(connection: SocketChannel): Option[List[Path]] =
    decode(readLine(new BufferedInputStream(Channels.newInputStream(connection))))

  private def readLine(input: BufferedInputStream): String =
    val line = new ByteArrayOutputStream()
    Iterator
      .continually(input.read())
      .takeWhile(byte => byte != -1 && byte != RequestTerminator.toInt && line.size < MaxRequestBytes)
      .foreach(byte => line.write(byte))
    line.toString(StandardCharsets.UTF_8)

  private def acknowledge(connection: SocketChannel): Unit =
    Channels.newOutputStream(connection).write(Acknowledgement.getBytes(StandardCharsets.UTF_8))

  private def exchange(channel: SocketChannel, request: String): Boolean =
    Channels.newOutputStream(channel).write(request.getBytes(StandardCharsets.UTF_8))
    val reply = Channels.newInputStream(channel).readNBytes(Acknowledgement.length)
    new String(reply, StandardCharsets.UTF_8) == Acknowledgement
