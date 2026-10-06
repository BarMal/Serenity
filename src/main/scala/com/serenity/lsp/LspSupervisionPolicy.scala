package com.serenity.lsp

import scala.concurrent.duration.*

private[lsp] enum LspRestartDecision:
  case RestartAfter(delay: FiniteDuration)
  case GiveUp(recentCrashes: Int)

/** How `LspManager` keeps language servers running and lets them go. A server that dies is restarted after an
  * exponentially growing delay until it has died more than `maxRestarts` times within `restartWindow`; a server left
  * serving no document is shut down once it has stayed that way for `idleShutdownGrace`, so closing and reopening a
  * file does not pay for a server restart.
  */
final private[lsp] case class LspSupervisionPolicy(
    maxRestarts: Int,
    restartWindow: FiniteDuration,
    initialBackoff: FiniteDuration,
    idleShutdownGrace: FiniteDuration,
    shutdownTimeout: FiniteDuration
):

  /** `crashTimes` includes the crash being decided on. */
  def afterCrash(crashTimes: List[FiniteDuration], now: FiniteDuration): LspRestartDecision =
    val recent = recentCrashes(crashTimes, now)
    if recent.size > maxRestarts then LspRestartDecision.GiveUp(recent.size)
    else LspRestartDecision.RestartAfter(backoff(recent.size))

  /** Whether a new connection may be started now: never while the server is given up on, and not before the backoff its
    * last crash earned has passed, so a demand arriving mid-backoff cannot cut it short.
    */
  def mayConnect(crashTimes: List[FiniteDuration], now: FiniteDuration): Boolean =
    afterCrash(crashTimes, now) match
      case LspRestartDecision.GiveUp(_)           => false
      case LspRestartDecision.RestartAfter(delay) => crashTimes.maxOption.forall(last => now - last >= delay)

  def recentCrashes(crashTimes: List[FiniteDuration], now: FiniteDuration): List[FiniteDuration] =
    crashTimes.filter(crashedAt => now - crashedAt < restartWindow)

  private def backoff(recentCrashCount: Int): FiniteDuration =
    initialBackoff * (1L << (recentCrashCount - 1).max(0).min(LspSupervisionPolicy.MaxBackoffDoublings))

private[lsp] object LspSupervisionPolicy:

  private val MaxBackoffDoublings = 16

  /** VS Code's language client allows five restarts in three minutes; three keeps a server that dies on every start
    * from flapping for as long before the user is told.
    */
  val Default: LspSupervisionPolicy = LspSupervisionPolicy(
    maxRestarts = 3,
    restartWindow = 3.minutes,
    initialBackoff = 1.second,
    idleShutdownGrace = 30.seconds,
    shutdownTimeout = 2.seconds
  )
