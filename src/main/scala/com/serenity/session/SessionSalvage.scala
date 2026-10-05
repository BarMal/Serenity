package com.serenity.session

import java.nio.file.{Path, Paths}

import scala.util.Try

import _root_.io.circe.{ACursor, Json}

enum UnreadableReason:
  case Corrupt
  case NewerVersion(schemaVersion: Int)

/** Unsaved text read leniently out of a session that could not be restored. */
final case class SalvagedText(label: String, text: String)

/** A session that could not be restored: where the untouched original now lives, and the plain-text copies of whatever
  * unsaved text could be read out of it.
  */
final case class UnreadableSession(reason: UnreadableReason, backup: Path, recoveredTexts: List[Path]):

  def describe: List[String] =
    val why = reason match
      case UnreadableReason.Corrupt =>
        "Your last session could not be restored because its file is damaged."
      case UnreadableReason.NewerVersion(version) =>
        s"Your last session was saved by a newer version of Serenity (session format $version), so this version " +
          "could not restore it."
    val recovered = recoveredTexts.headOption.flatMap(path => Option(path.getParent)).map { directory =>
      val documents = if recoveredTexts.size == 1 then "1 document" else s"${recoveredTexts.size} documents"
      s"Unsaved text from $documents was saved as plain text in $directory."
    }
    List(why, s"The session file was kept unchanged at $backup.") ++ recovered

  def summary: String = describe.mkString(" ")

object SessionSalvage:

  // A truncated write leaves JSON no parser accepts, but every buffer text written before the cut is still a complete
  // JSON string literal after its key.
  private val UnsavedContentLiteral = "\"unsavedContent\"\\s*:\\s*(\"(?:[^\"\\\\]|\\\\.)*\")".r

  private val UnsafeFileNameCharacters = "[^A-Za-z0-9._-]+".r

  private val MaxLabelLength = 60

  def reason(json: String): UnreadableReason =
    _root_.io.circe.parser
      .parse(json)
      .toOption
      .flatMap(_.hcursor.get[Int]("schemaVersion").toOption)
      .filter(_ > SessionState.CurrentSchemaVersion.value)
      .fold(UnreadableReason.Corrupt)(UnreadableReason.NewerVersion(_))

  def salvage(json: String): List[SalvagedText] =
    _root_.io.circe.parser.parse(json).fold(_ => scanned(json), parsed)

  def backupFileName(originalFileName: String, reason: UnreadableReason, epochMillis: Long): String =
    val kind = reason match
      case UnreadableReason.Corrupt         => "corrupt"
      case UnreadableReason.NewerVersion(_) => "newer"
    s"$originalFileName.$kind-$epochMillis"

  def recoveredFileName(index: Int, label: String): String =
    val safeLabel = UnsafeFileNameCharacters.replaceAllIn(label, "_").take(MaxLabelLength)
    f"${index + 1}%02d-$safeLabel.txt"

  /** A clean file-backed buffer's text is already on disk, so only edited or untitled text is worth keeping. */
  private def parsed(session: Json): List[SalvagedText] =
    session.hcursor.downField("buffers").values.toList.flatten.zipWithIndex.flatMap { (buffer, index) =>
      val cursor   = buffer.hcursor
      val filePath = cursor.get[Option[String]]("filePath").toOption.flatten
      val edited   = cursor.get[Boolean]("isDirty").getOrElse(true) || filePath.isEmpty
      unsavedText(cursor).filter(_ => edited).map(SalvagedText(label(filePath, index), _))
    }

  private def unsavedText(cursor: ACursor): Option[String] =
    cursor.get[Option[String]]("unsavedContent").toOption.flatten.filter(_.nonEmpty)

  private def label(filePath: Option[String], index: Int): String =
    filePath
      .flatMap(path => Try(Paths.get(path).getFileName).toOption.flatMap(Option(_)))
      .map(_.toString)
      .getOrElse(s"untitled-${index + 1}")

  private def scanned(json: String): List[SalvagedText] =
    UnsavedContentLiteral
      .findAllMatchIn(json)
      .flatMap(found => _root_.io.circe.parser.decode[String](found.group(1)).toOption)
      .filter(_.nonEmpty)
      .zipWithIndex
      .map((text, index) => SalvagedText(s"recovered-${index + 1}", text))
      .toList
