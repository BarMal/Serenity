package com.serenity.text

/** The line terminator a file arrived with.
  *
  * `Rope` normalises every terminator to `\n` on the way in, so that after a file is loaded nothing downstream can tell
  * what it was written with. That is the right call for editing -- wrapping, cursor arithmetic and search all get to
  * assume one terminator -- but it means the buffer alone cannot reproduce the file it came from. Without recording
  * this, saving rewrites every line of a CRLF file to LF, silently and on the first save.
  */
enum LineEnding(val sequence: String):
  case Lf   extends LineEnding("\n")
  case Crlf extends LineEnding("\r\n")

  def configKey: String =
    this match
      case Lf   => "lf"
      case Crlf => "crlf"

  /** Rewrite normalised (LF-only) editor content back into this terminator. */
  def applyTo(normalizedContent: String): String =
    this match
      case Lf   => normalizedContent
      case Crlf => normalizedContent.replace("\n", Crlf.sequence)

object LineEnding:

  val default: LineEnding = Lf

  def fromConfigKey(value: String): Option[LineEnding] =
    values.find(_.configKey.equalsIgnoreCase(value.trim))

  /** The terminator to treat `rawContent` as being written with, judged before any normalisation.
    *
    * Mixed files are real -- a merge or a generator can leave both -- and they have no correct answer, so the majority
    * wins and a tie goes to the platform-neutral `Lf`. Whichever is chosen, saving then makes the file uniform, which
    * is a change but a coherent one; preserving the original mixture would mean tracking a terminator per line for a
    * file that is already inconsistent.
    */
  def detect(rawContent: String): LineEnding =
    val crlfCount = countCrlf(rawContent)
    val lfCount   = rawContent.count(_ == '\n') - crlfCount
    if crlfCount > lfCount then Crlf else Lf

  private def countCrlf(content: String): Int =
    content.sliding(2).count(_ == Crlf.sequence)
