package com.serenity.state.models

/** Text Serenity put on the clipboard. A whole-line copy -- Copy or Cut with no selection -- pastes back as lines above
  * the caret line rather than at the caret, as VS Code and Sublime Text paste one.
  */
final case class ClipboardEntry(text: String, wholeLine: Boolean)

/** The most recent clipboard texts, newest first, at most [[ClipboardHistory.Capacity]] of them and none repeated. */
final case class ClipboardHistory(entries: List[ClipboardEntry]):

  /** An empty text is kept only as a whole-line copy, where it still pastes an empty line. */
  def recorded(entry: ClipboardEntry): ClipboardHistory =
    if entry.text.isEmpty && !entry.wholeLine then this
    else ClipboardHistory((entry :: entries.filterNot(_.text == entry.text)).take(ClipboardHistory.Capacity))

  /** Text read back from the system clipboard. Only text Serenity did not write itself starts an entry, so a whole-line
    * copy keeps its shape across the round trip.
    */
  def imported(text: String): ClipboardHistory =
    if entries.headOption.exists(_.text == text) then this else recorded(ClipboardEntry(text, wholeLine = false))

  /** How `text` pastes: as lines only while it is still the whole-line copy Serenity made last. */
  def entryFor(text: String): ClipboardEntry =
    entries.headOption.filter(_.text == text).getOrElse(ClipboardEntry(text, wholeLine = false))

object ClipboardHistory:

  val Capacity: Int = 20

  val empty: ClipboardHistory = ClipboardHistory(Nil)
