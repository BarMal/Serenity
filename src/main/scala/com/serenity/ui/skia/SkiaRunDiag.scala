package com.serenity.ui.skia

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

/** DIAGNOSTIC (branch claude/diag-frame-count-runs, not for merging): what each counted run of a Skia surface drew. */
object SkiaRunDiag:

  private[skia] val pending = new AtomicReference[String]("")
  private val log           = new ConcurrentLinkedQueue[String]()

  private[skia] def record(bounds: String): Unit =
    val _ = log.add(s"${pending.get()} $bounds")

  def drain(): List[String] =
    Iterator.continually(log.poll()).takeWhile(_ != null).toList
