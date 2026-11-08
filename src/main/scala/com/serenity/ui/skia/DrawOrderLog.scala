package com.serenity.ui.skia

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*

/** A scratch diagnostic for #2618, never to be merged: with `-Dserenity.skia.drawOrderLog=<file>` it records the
  * nanoTime, thread, kind and a per-kind counter of every publish, caret change, render callback and Skiko frame
  * bracket, and writes them to the file every 2 seconds and at exit.
  */
private[skia] object DrawOrderLog:

  private val path     = Option(System.getProperty("serenity.skia.drawOrderLog")).filter(_.trim.nonEmpty)
  private val events   = new ConcurrentLinkedQueue[String]()
  private val counters = new ConcurrentHashMap[String, AtomicInteger]()

  def note(kind: String): Unit =
    if path.isDefined then
      val sequence = counters.computeIfAbsent(kind, _ => new AtomicInteger(0)).incrementAndGet()
      val _        = events.add(s"${System.nanoTime()} ${Thread.currentThread.getName.replace(' ', '_')} $kind $sequence")

  private def flush(file: String): Unit =
    val lines = Iterator.continually(events.poll()).takeWhile(_ != null).toList
    if lines.nonEmpty then
      val _ = Files.write(
        Paths.get(file),
        lines.asJava,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )

  path.foreach { file =>
    val writer = new Thread(
      () =>
        while true do
          Thread.sleep(2000)
          flush(file)
      ,
      "draw-order-log"
    )
    writer.setDaemon(true)
    writer.start()
    Runtime.getRuntime.addShutdownHook(new Thread(() => flush(file)))
  }
