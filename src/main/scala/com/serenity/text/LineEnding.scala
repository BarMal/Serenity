package com.serenity.text

/** The line terminator a file arrived with.
  *
  * `Rope` normalises every terminator to `\n` on the way in, so that after a file is loaded nothing downstream can tell
  * what it was written with. That is the right call for editing -- wrapping, cursor arithmetic and search all get to
  * assume one terminator -- but it means the buffer alone cannot reproduce the file it came from. Without recording
  * this, saving rewrites every line of a CRLF file to LF, silently and on the first save.
  */
enum LineEnding(val sequence: String, val label: String):
  case Lf   extends LineEnding("\n", "LF")
  case Crlf extends LineEnding("\r\n", "CRLF")
  case Cr   extends LineEnding("\r", "CR")

  def configKey: String = label.toLowerCase

  /** Rewrite normalised (LF-only) editor content back into this terminator. */
  def applyTo(normalizedContent: String): String =
    this match
      case Lf => normalizedContent
      case _  => normalizedContent.replace("\n", sequence)

object LineEnding:

  val default: LineEnding = Lf

  def fromConfigKey(value: String): Option[LineEnding] =
    values.find(_.configKey.equalsIgnoreCase(value.trim))

  /** The terminator to treat `rawContent` as being written with, judged before any normalisation. See
    * [[LineEndingCounts.dominant]] for how a mixed file is settled.
    */
  def detect(rawContent: String): LineEnding =
    LineEndingCounts.of(rawContent).dominant
