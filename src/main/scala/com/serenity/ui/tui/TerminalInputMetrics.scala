package com.serenity.ui.tui

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

/** Input-pipeline counters for diagnosing dropped-keystroke reports (e.g. the kitty prose-typing drop, where every
  * bare-modifier edge arrives as its own ESC-prefixed CSI-u sequence). Three points along [[TerminalInputHandler]]'s
  * path are counted:
  *   - `charsRead`: UTF-16 code units the read loop pulled off the terminal reader
  *   - `tokensDecoded`: [[TerminalInputDecoder.DecodedToken]]s the pure decoder produced from those bytes
  *   - `keysQueued`: [[TerminalInputHandler.QueuedInput.Key]] items actually offered to the downstream event queue
  *
  * A gap between `charsRead` and `keysQueued` beyond what escape sequences explain localises a loss to the
  * decode/handler layer; parity there points the investigation downstream (state/render). Kept off the hot path by
  * default via [[Disabled]] -- production wires an [[Enabled]] built with the session logger, tests take the no-op.
  */
sealed trait TerminalInputMetrics:
  def recordCharsRead(count: Int): IO[Unit]
  def recordTokens(count: Int): IO[Unit]
  def recordKeysQueued(count: Int): IO[Unit]

  /** Logged at INFO on handler shutdown so every TUI session leaves one line; the running progress line (see
    * [[TerminalInputMetrics.Enabled]]) is DEBUG so it only appears when a debugging session opts in.
    */
  def logSummary(context: String): IO[Unit]

object TerminalInputMetrics:

  /** Emit a running DEBUG line once every this many characters read, so a long live session shows drift accumulating
    * rather than only a single figure at the end.
    */
  val LogEvery: Long = 500L

  case object Disabled extends TerminalInputMetrics:
    def recordCharsRead(count: Int): IO[Unit]  = IO.unit
    def recordTokens(count: Int): IO[Unit]     = IO.unit
    def recordKeysQueued(count: Int): IO[Unit] = IO.unit
    def logSummary(context: String): IO[Unit]  = IO.unit

  final class Enabled private[TerminalInputMetrics] (
      logger: Logger[IO],
      charsRead: Ref[IO, Long],
      tokensDecoded: Ref[IO, Long],
      keysQueued: Ref[IO, Long]
  ) extends TerminalInputMetrics:

    def recordCharsRead(count: Int): IO[Unit] =
      charsRead.updateAndGet(_ + count).flatMap { total =>
        IO.whenA(total % LogEvery == 0L)(summaryLine("progress").flatMap(logger.debug(_)))
      }

    def recordTokens(count: Int): IO[Unit]     = tokensDecoded.update(_ + count)
    def recordKeysQueued(count: Int): IO[Unit] = keysQueued.update(_ + count)

    def logSummary(context: String): IO[Unit] = summaryLine(context).flatMap(logger.info(_))

    private def summaryLine(context: String): IO[String] =
      (charsRead.get, tokensDecoded.get, keysQueued.get).mapN { (read, tokens, keys) =>
        s"[TUI-INPUT $context] charsRead=$read tokensDecoded=$tokens keysQueued=$keys"
      }

  def create(logger: Logger[IO]): IO[TerminalInputMetrics] =
    for
      charsRead     <- Ref.of[IO, Long](0L)
      tokensDecoded <- Ref.of[IO, Long](0L)
      keysQueued    <- Ref.of[IO, Long](0L)
    yield new Enabled(logger, charsRead, tokensDecoded, keysQueued)
