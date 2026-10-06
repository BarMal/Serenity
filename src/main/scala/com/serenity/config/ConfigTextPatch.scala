package com.serenity.config

import java.util.Locale

import scala.util.Try
import scala.util.matching.Regex

import com.typesafe.config.ConfigFactory

/** Edits the config text a user wrote in place, line by line, instead of writing the model out again.
  *
  * A re-render keeps the settings but loses everything else about the file: the user's comments, their ordering, the
  * spellings they chose and any value they left alone. Changing one setting should change one line.
  */
private[config] object ConfigTextPatch:

  /** One setting to change: every spelling its key can be written with, and its new text, or `None` to remove it. */
  final case class Change(key: String, spellings: Set[String], value: Option[String])

  /** The new text, and the settings whose existing line was replaced or removed (as opposed to added). */
  final case class Patched(text: String, replacedKeys: Set[String])

  private val assignment: Regex = """^(\s*"?([A-Za-z0-9_.\-]+)"?\s*[=:]\s*)(.*?)(\r?)$""".r

  final private case class Line(index: Int, prefix: String, lineEnd: String)

  def apply(existing: String, changes: List[Change]): Patched =
    val start = (existing.split("\n", -1).toVector, Set.empty[String], Vector.empty[String])
    val (lines, replaced, added) = changes.foldLeft(start) {
      case ((current, replacedKeys, addedLines), change) =>
        val found = completeAssignments(current, change.spellings)
        (found.lastOption, change.value) match
          case (Some(last), Some(value)) =>
            (current.updated(last.index, last.prefix + value + last.lineEnd), replacedKeys + change.key, addedLines)
          case (None, Some(value)) =>
            (current, replacedKeys, addedLines :+ s"${change.key} = $value")
          case (_, None) =>
            val gone = found.map(_.index).toSet
            val kept = current.zipWithIndex.collect { case (line, index) if !gone.contains(index) => line }
            (kept, if found.isEmpty then replacedKeys else replacedKeys + change.key, addedLines)
    }
    Patched(withAppended(lines, added, existing.contains("\r\n")), replaced)

  /** Last-wins overrides at the end of the text, for settings a line edit could not reach. */
  def appendOverrides(text: String, changes: List[Change]): String =
    withAppended(
      text.split("\n", -1).toVector,
      changes.flatMap(change => change.value.map(value => s"${change.key} = $value")).toVector,
      text.contains("\r\n")
    )

  private def withAppended(lines: Vector[String], added: Vector[String], windowsEnds: Boolean): String =
    val newline = if windowsEnds then "\r\n" else "\n"
    val body    = if lines.lastOption.contains("") then lines.dropRight(1) else lines
    val all     = body.map(_.stripSuffix("\r")) ++ added
    if all.isEmpty then "" else all.mkString("", newline, newline)

  /** Lines of the form `key = value` for one of `spellings` whose value is complete on that line. A value that opens a
    * block or list without closing it could not be replaced one line at a time.
    */
  private def completeAssignments(lines: Vector[String], spellings: Set[String]): List[Line] =
    val wanted = spellings.map(_.toLowerCase(Locale.ROOT))
    lines.zipWithIndex.toList.flatMap { (line, index) =>
      line match
        case assignment(prefix, key, value, lineEnd)
            if wanted
              .contains(key.toLowerCase(Locale.ROOT)) && Try(ConfigFactory.parseString(s"k = $value")).isSuccess =>
          List(Line(index, prefix, lineEnd))
        case _ => Nil
    }
