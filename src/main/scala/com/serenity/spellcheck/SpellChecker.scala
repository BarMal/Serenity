package com.serenity.spellcheck

import java.util.Locale

import com.serenity.config.SpellCheckConfig
import com.serenity.lsp.model.*
import com.serenity.state.models.{AppState, Buffer, BufferId}

object SpellChecker:

  val Source: String = "spell-check"

  private val WordPattern = """[\p{L}\p{M}]+(?:['’-][\p{L}\p{M}]+)*""".r

  private val BuiltInDictionaries: Map[String, Set[String]] = Map(
    "en" -> Set(
      "a",
      "an",
      "and",
      "are",
      "as",
      "be",
      "buffer",
      "code",
      "document",
      "editor",
      "for",
      "hello",
      "in",
      "is",
      "json",
      "language",
      "markdown",
      "of",
      "ok",
      "parse",
      "prose",
      "serenity",
      "spell",
      "text",
      "the",
      "to",
      "with",
      "world"
    ),
    "fr" -> Set(
      "bonjour",
      "caf\u00e9",
      "fran\u00e7ais",
      "langue",
      "monde",
      "r\u00e9sum\u00e9",
      "texte"
    ),
    "el" -> Set(
      "\u03b3\u03b5\u03b9\u03ac",
      "\u03ba\u03cc\u03c3\u03bc\u03bf\u03c2"
    )
  )

  def check(text: String, config: SpellCheckConfig): List[Diagnostic] =
    val normalized = config.normalized
    if !normalized.enabled then Nil
    else
      val dictionary = dictionaryFor(normalized)
      text
        .split("\n", -1)
        .zipWithIndex
        .flatMap { (line, lineIndex) =>
          WordPattern
            .findAllMatchIn(line)
            .filterNot(match_ => isAccepted(match_.matched, dictionary))
            .map { match_ =>
              val word = match_.matched
              Diagnostic(
                range = LspRange(
                  LspPosition(lineIndex, match_.start),
                  LspPosition(lineIndex, match_.end)
                ),
                severity = Some(DiagnosticSeverity.Hint),
                message = s"Possible spelling issue: $word",
                source = Some(Source),
                code = Some("unknown-word")
              )
            }
            .toList
        }
        .toList

  def refreshDiagnostics(state: AppState): AppState =
    val preserved = state.diagnostics.view
      .mapValues(_.filterNot(isSpellCheckDiagnostic))
      .filter(_._2.nonEmpty)
      .toMap

    val refreshed = state.buffers.values.foldLeft(preserved) { (diagnostics, buffer) =>
      val spellDiagnostics =
        if shouldCheck(buffer, state.config.spellCheck) then check(buffer.content.collect(), state.config.spellCheck)
        else Nil
      if spellDiagnostics.isEmpty then diagnostics
      else diagnostics + (diagnosticsUri(buffer) -> spellDiagnostics)
    }

    state.copy(diagnostics = refreshed)

  def diagnosticsUri(buffer: Buffer): String =
    buffer.filePath.map(_.toUri.toString).getOrElse(bufferDiagnosticsUri(buffer.id))

  def bufferDiagnosticsUri(bufferId: BufferId): String =
    s"buffer:${bufferId.value}"

  private def shouldCheck(buffer: Buffer, config: SpellCheckConfig): Boolean =
    config.enabled && buffer.usesTextFont

  private def dictionaryFor(config: SpellCheckConfig): Set[String] =
    val languageWords = config.languages.flatMap(language => BuiltInDictionaries.getOrElse(language, Set.empty))
    (languageWords ++ config.additionalWords).map(normalizeWord).toSet

  private def isAccepted(word: String, dictionary: Set[String]): Boolean =
    val normalized = normalizeWord(word)
    normalized.length < 3 || dictionary.contains(normalized)

  private def normalizeWord(word: String): String =
    word.toLowerCase(Locale.ROOT)

  private def isSpellCheckDiagnostic(diagnostic: Diagnostic): Boolean =
    diagnostic.source.contains(Source)
end SpellChecker
