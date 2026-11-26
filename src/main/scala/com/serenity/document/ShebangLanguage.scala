package com.serenity.document

import com.serenity.lsp.config.LanguageId

/** The language a script's first line names, for a file whose name does not: `#!/usr/bin/env python3` is Python. Only
  * the interpreters that are a [[LanguageId]] are known; a shell or make script has no language here and reads as plain
  * text, as it did before.
  */
object ShebangLanguage:

  private val Python     = raw"python(\d+(\.\d+)*)?".r
  private val Lua        = raw"lua(jit)?(\d+(\.\d+)*)?".r
  private val JavaScript = Set("node", "nodejs", "deno", "bun")

  def of(firstLine: String): Option[LanguageId] =
    val line = firstLine.stripTrailing
    Option
      .when(line.startsWith("#!"))(line.drop(2).trim.split("\\s+").toList.filter(_.nonEmpty))
      .flatMap(interpreter)

  private def interpreter(words: List[String]): Option[LanguageId] =
    words match
      case Nil => None
      case first :: rest if baseName(first) == "env" =>
        rest.dropWhile(_.startsWith("-")).headOption.flatMap(word => languageOf(baseName(word)))
      case first :: _ => languageOf(baseName(first))

  private def baseName(word: String): String = word.substring(word.lastIndexOf('/') + 1)

  private def languageOf(interpreter: String): Option[LanguageId] =
    interpreter match
      case Python(_*)                        => Some(LanguageId.Python)
      case "ruby"                            => Some(LanguageId.Ruby)
      case Lua(_*)                           => Some(LanguageId.Lua)
      case name if JavaScript.contains(name) => Some(LanguageId.JavaScript)
      case _                                 => None
