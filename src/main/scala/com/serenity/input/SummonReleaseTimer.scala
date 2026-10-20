package com.serenity.input

/** Settles a summon release no auto-repeat press followed. Restarting it replaces the pending lapse. */
trait SummonReleaseTimer:
  def restart(): Unit

object SummonReleaseTimer:

  /** One-shot, on the event dispatch thread like the key listeners. Lapses a little after the repeat window, so a
    * repeat's press that is already queued behind the release is handled first.
    */
  def swing(onLapse: () => Unit): SummonReleaseTimer =
    val timer = new javax.swing.Timer((SummonChordTracker.RepeatGapMillis + 8).toInt, _ => onLapse())
    timer.setRepeats(false)
    () => timer.restart()
