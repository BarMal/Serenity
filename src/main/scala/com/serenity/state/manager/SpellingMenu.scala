package com.serenity.state.manager

import com.serenity.command.SpellingCommands
import com.serenity.spellcheck.Misspelling
import com.serenity.state.models.ContextMenuItem

/** What a menu offers for one misspelled word: the best suggestions to replace it with, then what to do about it. */
private[manager] object SpellingMenu:

  val MaxSuggestions: Int = 5

  def items(found: Misspelling, suggestions: List[String]): List[ContextMenuItem] =
    val replacements = suggestions.take(MaxSuggestions).zipWithIndex.map { (suggestion, index) =>
      ContextMenuItem(
        s"spell-replace-$index",
        suggestion,
        SpellingCommands.replace(found.line, found.start, found.end, found.word, suggestion)
      )
    }
    replacements ++ List(
      ContextMenuItem(
        "spell-add-to-dictionary",
        s"Add “${found.word}” to Dictionary",
        SpellingCommands.addToDictionary(found.word)
      ),
      ContextMenuItem(
        "spell-ignore-once",
        "Ignore Once",
        SpellingCommands.ignoreOnce(found.line, found.start, found.end, found.word)
      ),
      ContextMenuItem("spell-ignore-everywhere", "Ignore All", SpellingCommands.ignoreEverywhere(found.word))
    )
