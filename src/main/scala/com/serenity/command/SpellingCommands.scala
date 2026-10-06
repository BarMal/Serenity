package com.serenity.command

/** The commands a spelling menu offers for one flagged word. They are built per menu, each carrying the word and where
  * it stands, so they are not registered: the palette's own spelling commands act on the word at the cursor.
  */
object SpellingCommands:

  def replace(line: Int, start: Int, end: Int, misspelled: String, replacement: String): Command =
    spelling(
      "spell-replace",
      s"Replace $misspelled with $replacement.",
      SpellingIntent.Replace(line, start, end, misspelled, replacement)
    )

  def addToDictionary(word: String): Command =
    spelling(
      "spell-add-to-dictionary",
      s"Add $word to the custom spell-check dictionary.",
      SpellingIntent.AddToDictionary(word)
    )

  def ignoreOnce(line: Int, start: Int, end: Int, word: String): Command =
    spelling(
      "spell-ignore-once",
      s"Leave this $word alone for this session.",
      SpellingIntent.IgnoreOnce(line, start, end, word)
    )

  def ignoreEverywhere(word: String): Command =
    spelling(
      "spell-ignore-everywhere",
      s"Leave $word alone everywhere for this session.",
      SpellingIntent.IgnoreEverywhere(word)
    )

  private def spelling(name: String, description: String, intent: SpellingIntent): Command =
    Command.typed(
      name,
      description,
      CommandIntent.Spelling(intent),
      CommandCategory.Edit
    )
