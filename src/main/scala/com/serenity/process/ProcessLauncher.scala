package com.serenity.process

import java.io.{InputStream, OutputStream}
import java.util.concurrent.CompletableFuture

import scala.jdk.CollectionConverters.*

import cats.effect.IO

/** The one seam to the operating system: starting a process. Specs substitute a fake. */
trait ProcessLauncher:
  def start(spec: ProcessSpec): IO[RunningProcess]

object ProcessLauncher:

  object system extends ProcessLauncher:

    def start(spec: ProcessSpec): IO[RunningProcess] =
      IO.blocking(new JdkProcess(new ProcessBuilder(spec.command*).start()))

  final private class JdkControl(handle: ProcessHandle) extends ProcessControl:
    def pid: Long                 = handle.pid
    def isAlive: IO[Boolean]      = IO(handle.isAlive)
    def destroy: IO[Unit]         = IO(handle.destroy()).void
    def destroyForcibly: IO[Unit] = IO(handle.destroyForcibly()).void

  final private class JdkProcess(process: Process) extends RunningProcess:
    private val control = new JdkControl(process.toHandle)

    def pid: Long                 = control.pid
    def isAlive: IO[Boolean]      = control.isAlive
    def destroy: IO[Unit]         = control.destroy
    def destroyForcibly: IO[Unit] = control.destroyForcibly

    def descendants: IO[List[ProcessControl]] =
      IO(process.descendants().iterator().asScala.map(new JdkControl(_)).toList)

    def exitCode: IO[Int] =
      IO.fromCompletableFuture(IO(process.onExit(): CompletableFuture[Process])).map(_.exitValue)

    def readStdout: IO[Array[Byte]] = IO.blocking(process.getInputStream.readAllBytes())

    def discardStderr: IO[Unit] =
      IO.blocking(process.getErrorStream.transferTo(OutputStream.nullOutputStream())).void.attempt.void

    def stdin: OutputStream = process.getOutputStream
    def stdout: InputStream = process.getInputStream
    def stderr: InputStream = process.getErrorStream
