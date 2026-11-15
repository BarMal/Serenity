package com.serenity.testkit

import com.serenity.rope.{Rope, RopeChunks}

/** A rope as its leaves, each with its length and its text written so that a log line can be pasted back into a test:
  * every UTF-16 unit outside printable ASCII, and the quote and the backslash, is a `\uXXXX` escape.
  *
  * A leaf boundary can fall inside a surrogate pair, which a terminal or a CI log prints as `?`, so a failing property
  * over leaf splits is not replayable from the plain text.
  */
object RopeShapeReport:

  def of(rope: Rope): String =
    val leaves = RopeChunks.of(rope).toList
    val shown  = leaves.map(leaf => s"""[${leaf.length} "${escaped(leaf)}"]""")
    s"${leaves.size} leaves: ${shown.mkString(" ")}"

  private def escaped(text: String): String =
    text.map(unit => if literal(unit) then unit.toString else f"\\u${unit.toInt}%04X").mkString

  private def literal(unit: Char): Boolean =
    unit >= ' ' && unit <= '~' && unit != '\\' && unit != '"'
