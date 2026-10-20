package com.serenity.state.models

import java.nio.file.Path

/** How heavy a document is for the features that scale with its whole text. */
enum FileSize:
  case Ordinary
  case Large

  /** Past what language servers and spell checking can follow without stalling the editor. */
  case Huge

/** When a document counts as large (#1867), measured in characters because that is what a rope knows in O(1); for
  * mostly-ASCII text it is the file's size in bytes.
  *
  * A large document opens with a warning. A huge one also goes without the features that re-read the whole text on
  * every change: it is never announced to a language server (so it has no diagnostics, completions or semantic tokens)
  * and is not spell-checked. Editing, search and saving are unaffected. With `optimizations` off nothing is switched
  * off, however large the document, and only the warning remains.
  */
final case class LargeFilePolicy(
    warnAboveChars: Int,
    toolingOffAboveChars: Int,
    optimizations: Boolean = true
):

  def classify(chars: Int): FileSize =
    if optimizations && chars > toolingOffAboveChars then FileSize.Huge
    else if chars > warnAboveChars then FileSize.Large
    else FileSize.Ordinary

  def sizeOf(document: Document): FileSize = classify(document.content.weight)

  def allowsLanguageTooling(document: Document): Boolean = sizeOf(document) != FileSize.Huge

  def allowsSpellCheck(document: Document): Boolean = sizeOf(document) != FileSize.Huge

  /** What opening `document` from `path` should tell the user, if anything. */
  def openingNotice(path: Path, document: Document): Option[Notice] =
    val name = Option(path.getFileName).fold(path.toString)(_.toString)
    val size = LargeFilePolicy.millionsOfCharacters(document.content.weight)
    sizeOf(document) match
      case FileSize.Ordinary => None
      case FileSize.Large =>
        Some(Notice(NoticeLevel.Warning, s"$name is large ($size million characters): editing it may be slow."))
      case FileSize.Huge =>
        Some(
          Notice(
            NoticeLevel.Warning,
            s"$name is very large ($size million characters): language server features and spell check are off."
          )
        )

object LargeFilePolicy:

  val Default: LargeFilePolicy =
    LargeFilePolicy(warnAboveChars = 5000000, toolingOffAboveChars = 20000000, optimizations = true)

  private def millionsOfCharacters(chars: Int): String =
    val tenths = chars / 100000
    s"${tenths / 10}.${tenths % 10}"
