package com.serenity.docs

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import com.serenity.config.{HotkeyAction, HotkeyConfig, HotkeyTrigger, label}

/** The default-binding table in `docs/user/keyboard.md`, generated from [[HotkeyConfig]] so it cannot drift. Run
  * `sbt "Test/runMain com.serenity.docs.KeyboardReferenceTable"` to rewrite the file after a default changes;
  * `KeyboardReferenceDocSpec` fails while the file is stale.
  */
object KeyboardReferenceTable:

  val Document: Path = Paths.get("docs", "user", "keyboard.md")
  val StartMarker    = "<!-- keyboard-table:start -->"
  val EndMarker      = "<!-- keyboard-table:end -->"

  private val MacOs   = "Mac OS X"
  private val OtherOs = "Linux"

  def table: String =
    val mac   = HotkeyConfig.forOs(MacOs)
    val other = HotkeyConfig.forOs(OtherOs)
    val actionRows = HotkeyAction.values.toList.map { action =>
      row(
        action.configKey.replace('_', ' ').capitalize,
        action.configKey,
        labels(other.bindingsFor(action), OtherOs),
        labels(mac.bindingsFor(action), MacOs)
      )
    }
    val commandRows = other.commandBindings.keys.toList.sorted.map { id =>
      row(
        id.capitalize,
        id,
        labels(other.commandBindingsFor(id), OtherOs),
        labels(mac.commandBindingsFor(id), MacOs)
      )
    }
    ("| Action | Config key | Linux and Windows | macOS |" ::
      "| --- | --- | --- | --- |" ::
      actionRows ::: commandRows).mkString("\n")

  private def labels(triggers: List[HotkeyTrigger], osName: String): String =
    if triggers.isEmpty then "unbound" else triggers.map(trigger => s"`${trigger.label(osName)}`").mkString(", ")

  private def row(action: String, key: String, other: String, mac: String): String =
    s"| $action | `$key` | $other | $mac |"

  def spliced(document: String): Either[String, String] =
    val start = document.indexOf(StartMarker)
    val end   = document.indexOf(EndMarker)
    if start < 0 || end < start then Left(s"$Document must contain $StartMarker followed by $EndMarker")
    else Right(document.substring(0, start + StartMarker.length) + "\n" + table + "\n" + document.substring(end))

  def main(args: Array[String]): Unit =
    val current = new String(Files.readAllBytes(Document), StandardCharsets.UTF_8)
    spliced(current) match
      case Right(updated) => Files.write(Document, updated.getBytes(StandardCharsets.UTF_8)); ()
      case Left(problem)  => sys.error(problem)
