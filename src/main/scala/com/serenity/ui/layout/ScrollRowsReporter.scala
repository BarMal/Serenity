package com.serenity.ui.layout

import java.util.concurrent.atomic.AtomicReference

import com.serenity.state.models.BufferId
import org.slf4j.LoggerFactory

/** Says a buffer's [[ScrollRowCounts]] when they are first known and again whenever they change (#2506). Off unless
  * `-Dserenity.diag.scrollRows=true`; while off, the counts are never measured, so the render path pays one test.
  */
final class ScrollRowsReporter(enabled: Boolean, sink: String => Unit):

  private val lastSaid = AtomicReference(Map.empty[BufferId, ScrollRowCounts])

  def report(buffer: BufferId, counts: => ScrollRowCounts): Unit =
    if enabled then
      val now    = counts
      val before = lastSaid.getAndUpdate(_.updated(buffer, now))
      if !before.get(buffer).contains(now) then sink(now.logLine(buffer))

object ScrollRowsReporter:

  val PropertyName: String = "serenity.diag.scrollRows"

  private val logger = LoggerFactory.getLogger(classOf[ScrollRowsReporter])

  def fromSystemProperty(): ScrollRowsReporter =
    ScrollRowsReporter(System.getProperty(PropertyName, "false") == "true", line => logger.info(line))
