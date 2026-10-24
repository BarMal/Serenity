package com.serenity.ui.sdl

import scala.io.Source

/** The struct offsets and constants `ci/sdl3/layout.c` printed from the pinned SDL headers, read from
  * `src/test/resources/sdl3/layout.txt`.
  */
object SdlGolden:

  val entries: Map[String, Long] =
    val source = Source.fromResource("sdl3/layout.txt")
    try
      source
        .getLines()
        .filter(_.trim.nonEmpty)
        .map { line =>
          val Array(name, value) = line.trim.split(' ')
          name -> value.toLong
        }
        .toMap
    finally source.close()

  def apply(name: String): Long = entries.getOrElse(name, sys.error(s"no golden entry for $name"))
