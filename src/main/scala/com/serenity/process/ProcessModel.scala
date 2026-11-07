package com.serenity.process

import java.io.{InputStream, OutputStream}
import java.time.Instant

import scala.concurrent.duration.*

import cats.effect.IO

opaque type ProcessOwner = String

object ProcessOwner:
  def apply(name: String): ProcessOwner = name

  extension (owner: ProcessOwner) def name: String = owner

/** How long a stop may take: `grace` after asking a process to exit, then `forceWait` after killing it outright. */
final case class StopPolicy(grace: FiniteDuration, forceWait: FiniteDuration)

object StopPolicy:
  val Default: StopPolicy = StopPolicy(2.seconds, 1.second)

final case class ProcessSpec(command: List[String], owner: ProcessOwner, stop: StopPolicy = StopPolicy.Default)

final case class ProcessResult(exitCode: Int, output: String)

final case class ProcessInfo(
    owner: ProcessOwner,
    pid: Long,
    command: List[String],
    startedAt: Instant,
    detached: Boolean
)

/** What the service can do to a process or one of its descendants. Errors from a stop request are the caller's to
  * ignore: a process that is already gone is not a failure.
  */
trait ProcessControl:
  def pid: Long
  def isAlive: IO[Boolean]
  def destroy: IO[Unit]
  def destroyForcibly: IO[Unit]

trait RunningProcess extends ProcessControl:
  def descendants: IO[List[ProcessControl]]
  def exitCode: IO[Int]
  def readStdout: IO[Array[Byte]]
  def discardStderr: IO[Unit]
  def stdin: OutputStream
  def stdout: InputStream
  def stderr: InputStream

/** The streams of an owned process, as a caller of `spawn` may use them. */
final case class ManagedProcess(
    pid: Long,
    stdin: OutputStream,
    stdout: InputStream,
    stderr: InputStream,
    exitCode: IO[Int]
)
