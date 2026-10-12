package com.serenity.state.models

/** What one source knows about how a document is indented. A source that cannot say a field leaves it `None`, so a
  * source behind it fills it in: a file indented with tabs says nothing about how wide a tab is.
  */
final case class IndentationHint(insertSpaces: Option[Boolean] = None, tabSize: Option[Int] = None):

  def isEmpty: Boolean = insertSpaces.isEmpty && tabSize.isEmpty

  /** The settings this hint spells out in full, which is what a user's choice always does. */
  def complete: Option[IndentationSettings] =
    for
      spaces <- insertSpaces
      size   <- tabSize
    yield IndentationSettings(size, spaces)

  def orElse(other: IndentationHint): IndentationHint =
    IndentationHint(insertSpaces.orElse(other.insertSpaces), tabSize.orElse(other.tabSize))

  def over(base: IndentationSettings): IndentationSettings =
    IndentationSettings(tabSize.getOrElse(base.tabSize), insertSpaces.getOrElse(base.insertSpaces)).normalized

object IndentationHint:

  val Empty: IndentationHint = IndentationHint()

  def of(settings: IndentationSettings): IndentationHint =
    IndentationHint(Some(settings.insertSpaces), Some(settings.tabSize))

/** The indentation sources for one document, most authoritative first: what the user chose for it, what the project's
  * configuration says about its path, and what its own text shows. Whatever none of them settles comes from the global
  * settings.
  */
final case class DocumentIndentation(
    chosen: IndentationHint = IndentationHint.Empty,
    configured: IndentationHint = IndentationHint.Empty,
    detected: IndentationHint = IndentationHint.Empty
):

  def effective(global: IndentationSettings): IndentationSettings =
    if chosen.isEmpty && configured.isEmpty && detected.isEmpty then global
    else chosen.orElse(configured).orElse(detected).over(global)

  /** `None` hands the choice back to the other sources. */
  def withChosen(settings: Option[IndentationSettings]): DocumentIndentation =
    copy(chosen = settings.fold(IndentationHint.Empty)(IndentationHint.of))
