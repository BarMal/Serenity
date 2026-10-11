package com.serenity.ui.tui

import java.nio.charset.StandardCharsets

import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*
import org.jline.utils.NonBlockingReader

/** Terminal input for a `TerminalInputHandler` pump that parks on a fiber-level queue instead of a blocked thread, so
  * the pump runs under `TestControl` virtual time. Passed as the handler's `readSource`; the pump's read-gate counts
  * the same enter/return transitions it counts for a real blocking read, so the liveness guard sees what it sees in
  * production: a read with nothing queued is parked, one with input queued returns at once.
  */
final class ParkingInput private (chars: Queue[IO, Int]):

  val readSource: TerminalInputHandler.ReadGate => IO[Int] = gate =>
    IO(gate.transitions.incrementAndGet()) >> chars.take.flatTap(_ => IO(gate.transitions.incrementAndGet()))

  def feed(input: Array[Byte]): IO[Unit] =
    new String(input, StandardCharsets.UTF_8).toList.traverse_(c => chars.offer(c.toInt))

  def feedEof: IO[Unit] = chars.offer(NonBlockingReader.EOF)

object ParkingInput:
  def create: IO[ParkingInput] = Queue.unbounded[IO, Int].map(new ParkingInput(_))
