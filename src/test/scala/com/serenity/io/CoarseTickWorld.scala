package com.serenity.io

import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}

/** A clock and a stat over a filesystem whose timestamp tick is [100 s, 102 s): every stat of any file reports an mtime
  * of 100 s, so an external same-size rewrite of `raced` leaves its stamp unchanged.
  *
  * The clock starts inside the tick (101.9 s). [[settle]] is the external rewrite landing and time passing the end of
  * the tick (102.01 s). It fires at most once, and fires early if the code under test reads the clock after it has
  * stat-ed `raced` -- which is how a clock sampled after the stat is caught.
  */
final class CoarseTickWorld(raced: Path, rewrite: Array[Byte] => Array[Byte]):
  private val second      = 1_000_000_000L
  private val insideTick  = 101L * second + 900_000_000L
  private val pastTickEnd = 102L * second + 10_000_000L
  private val tickMtime   = FileTime.from(100L * second, TimeUnit.NANOSECONDS)
  private val now         = Ref.unsafe[IO, Long](insideTick)
  private val statted     = Ref.unsafe[IO, Boolean](false)
  private val settledOnce = Ref.unsafe[IO, Boolean](false)
  private val racedPath   = raced.toAbsolutePath.normalize

  val settle: IO[Unit] =
    settledOnce.getAndSet(true).flatMap { alreadySettled =>
      IO.unlessA(alreadySettled)(
        IO.blocking(Files.write(raced, rewrite(Files.readAllBytes(raced)))) >> now.set(pastTickEnd)
      )
    }

  val clock: IO[FiniteDuration] =
    statted.get.flatMap(IO.whenA(_)(settle)) >> now.get.map(FiniteDuration(_, TimeUnit.NANOSECONDS))

  val attributes: FileStamp.Attributes = path =>
    val real = Files.readAttributes(path, classOf[BasicFileAttributes])
    if path.toAbsolutePath.normalize == racedPath then statted.set(true).unsafeRunSync()
    new BasicFileAttributes:
      def lastModifiedTime: FileTime = tickMtime
      def lastAccessTime: FileTime   = real.lastAccessTime
      def creationTime: FileTime     = real.creationTime
      def isRegularFile: Boolean     = real.isRegularFile
      def isDirectory: Boolean       = real.isDirectory
      def isSymbolicLink: Boolean    = real.isSymbolicLink
      def isOther: Boolean           = real.isOther
      def size: Long                 = real.size
      def fileKey: Object            = real.fileKey
