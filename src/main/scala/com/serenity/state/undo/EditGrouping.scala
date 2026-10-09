package com.serenity.state.undo

/** The kinds of edit whose consecutive runs share one undo step, as mainstream editors group them. */
enum EditKind:
  case Typing, DeletingBackward, DeletingForward

/** How a recorded edit relates to the run of edits before it (#1930). */
enum EditGrouping:

  /** One undo step on its own: paste, cut, replace, format, newline, block indent, and any edit made by a command. */
  case Standalone

  /** An edit of `kind` that continues the buffer's open run of the same kind and keeps going from where it left off.
    * `afterWord` is whitespace typed straight after a word: it opens a new step rather than joining the word's.
    */
  case Coalescing(kind: EditKind, afterWord: Boolean)

object EditGrouping:

  /** Characters typed at one cursor, with `previous` the character just before the insertion point. */
  def typing(inserted: String, previous: Option[Char]): EditGrouping =
    val isSeparator = inserted.nonEmpty && inserted.forall(_.isWhitespace) && previous.exists(!_.isWhitespace)
    Coalescing(EditKind.Typing, afterWord = isSeparator)

  def deleting(kind: EditKind): EditGrouping = Coalescing(kind, afterWord = false)
