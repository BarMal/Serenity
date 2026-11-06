package com.serenity.ui.benchmark

import java.util.concurrent.locks.LockSupport

import scala.annotation.tailrec

/** What the resize scenario does about a window manager that decides the window's size (#2480). A tiling manager keeps
  * a window at its tile whatever it is asked for, so asking again and timing the frames that follow would time a resize
  * that never happened.
  */
object WindowSizing:

  enum Outcome:

    /** The window has the size it was asked for. */
    case Applied(size: WindowPx)

    /** The time was up and the window still had `size`. */
    case Kept(size: WindowPx)

  /** How long a window gets to take a size, and how often it is looked at. A guess, not a measurement: with no window
    * manager the size had been applied by the time the event queue was empty in every one of 108 resizes (#2480), so
    * the wait only has to be long enough to tell a manager that answers from one that never will.
    */
  val ApplyTimeoutMillis: Long = 250L
  val PollMillis: Long         = 5L

  /** Looks at the window's size until it is `wanted` or `timeoutMillis` have passed on `clock`, pausing `pollMillis`
    * between looks. The clock and the pause are parameters so that a spec waits for nothing.
    */
  def awaitApplied(
    read: () => WindowPx,
    wanted: WindowPx,
    timeoutMillis: Long,
    pollMillis: Long,
    clock: () => Long,
    pause: Long => Unit
  ): Outcome =
    val started = clock()
    @tailrec def look(): Outcome =
      val size = read()
      if size == wanted then Outcome.Applied(size)
      else if clock() - started >= timeoutMillis then Outcome.Kept(size)
      else
        pause(pollMillis)
        look()
    look()

  /** [[awaitApplied]] in real time, with the guessed timeout. */
  def await(read: () => WindowPx, wanted: WindowPx): Outcome =
    awaitApplied(
      read,
      wanted,
      ApplyTimeoutMillis,
      PollMillis,
      () => System.nanoTime() / 1_000_000L,
      millis => LockSupport.parkNanos(millis * 1_000_000L)
    )

  def keptReason(asked: WindowPx, kept: WindowPx): String =
    s"the window manager kept the window at ${kept.width}x${kept.height} when it was asked for ${asked.width}x${asked.height}"
