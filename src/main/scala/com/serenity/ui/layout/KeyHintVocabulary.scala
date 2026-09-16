package com.serenity.ui.layout

/** The one set of words every command-runner surface uses for its keys, so the palette and the settings surface read as
  * one thing: the arrows move, Enter does the selected row's action, Esc goes back (or closes at the root), and the
  * side arrows change an option in place.
  */
object KeyHintVocabulary:

  private val Move      = "↑↓ move"
  private val Change    = "←→ change"
  private val Separator = " • "

  def browse(enterAction: String, escapeAction: String, changesOptions: Boolean = false): String =
    (List(Move, s"Enter $enterAction", s"Esc $escapeAction") ++ Option.when(changesOptions)(Change))
      .mkString(Separator)

  val editing: String   = List("Type to edit", "Enter save", "Esc cancel").mkString(Separator)
  val recording: String = "Esc cancel"

  /** The transient footer: the same words as [[browse]] plus where the selection sits in the list. */
  def footer(enterAction: String, escapeAction: String, position: Int, count: Int): String =
    s"${browse(enterAction, escapeAction)}$Separator$position/${count.max(1)}"
