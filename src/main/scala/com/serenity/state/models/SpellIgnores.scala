package com.serenity.state.models

import java.util.Locale

import com.serenity.lsp.client.DocumentUri

/** One occurrence of a word the writer chose to leave alone: where it stood, and what it said. */
final case class IgnoredOccurrence(line: Int, start: Int, word: String)

/** Misspellings the writer dismissed for this session (#1939). "Ignore all" is by word; "ignore once" is by occurrence,
  * and stops applying if the text at that place changes. Neither is persisted -- that is "Add to Dictionary", which
  * writes the word to the spell-check configuration.
  */
final case class SpellIgnores(
    words: Set[String] = Set.empty,
    occurrences: Map[DocumentUri, Set[IgnoredOccurrence]] = Map.empty
):

  def hides(uri: DocumentUri, occurrence: IgnoredOccurrence): Boolean =
    words.contains(SpellIgnores.key(occurrence.word)) || occurrences.getOrElse(uri, Set.empty).contains(occurrence)

  def withWord(word: String): SpellIgnores =
    copy(words = words + SpellIgnores.key(word))

  def withOccurrence(uri: DocumentUri, occurrence: IgnoredOccurrence): SpellIgnores =
    copy(occurrences = occurrences.updated(uri, occurrences.getOrElse(uri, Set.empty) + occurrence))

object SpellIgnores:

  val empty: SpellIgnores = SpellIgnores()

  private def key(word: String): String = word.toLowerCase(Locale.ROOT)
