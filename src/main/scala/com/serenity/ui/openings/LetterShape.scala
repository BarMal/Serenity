package com.serenity.ui.openings

/** How the first letter of a paragraph set as an initial sits against the lines it spans. */
enum LetterShape:
  /** The top of the letter is level with the capitals of the first line: a drop cap. */
  case Drop

  /** The letter is one line taller than a drop cap over the same lines and rises one line above the capitals of the
    * first line; the first row grows to make room for the part that rises.
    */
  case Rising

  def rises: Boolean = this == Rising
