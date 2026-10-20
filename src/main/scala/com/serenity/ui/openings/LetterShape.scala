package com.serenity.ui.openings

/** How the first letter of a paragraph set as an initial sits against the lines it spans. */
enum LetterShape:
  /** The top of the letter is level with the capitals of the first line: a drop cap. */
  case Drop

  /** The letter is one line taller than a drop cap over the same lines and rises one line above the capitals of the
    * first line. `growsRow` says whether the first row grows to make room for the part that rises or lets it reach into
    * the room above.
    */
  case Rising(growsRow: Boolean)

  def rises: Boolean = this match
    case Drop      => false
    case Rising(_) => true
