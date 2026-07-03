package com.serenity.spellcheck

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

import scala.util.control.NonFatal

import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}
import com.serenity.lsp.model.*
import com.serenity.state.models.*

object SpellChecker:

  val Source: String = "spell-check"

  private val WordPattern = """[\p{L}\p{M}]+(?:['’-][\p{L}\p{M}]+)*""".r

  private val DictionaryCache = ConcurrentHashMap[DictionaryCacheKey, DictionaryLoadResult]()

  private case class DictionaryCacheKey(path: String, size: Long, lastModifiedMillis: Long)
  private case class DictionaryLoadResult(words: Set[String], failures: List[String])
  private case class DictionaryContext(words: Set[String], failures: List[String])

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
      dictionaryLoadDiagnostics(dictionary.failures) ++
        text
          .split("\n", -1)
          .zipWithIndex
          .flatMap { (line, lineIndex) =>
            WordPattern
              .findAllMatchIn(line)
              .filterNot(match_ => isAccepted(match_.matched, dictionary.words))
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

    val (refreshed, cache) = state.buffers.values.foldLeft((preserved, Map.empty[String, SpellCheckCacheEntry])) {
      case ((diagnostics, cache), buffer) =>
        val uri = diagnosticsUri(buffer)
        if shouldCheck(buffer, state.config.spellCheck) then
          val fingerprint = SpellCheckFingerprint.from(buffer, state.config.spellCheck)
          val entry = state.spellCheckCache
            .get(uri)
            .filter(_.fingerprint == fingerprint)
            .getOrElse {
              val spellDiagnostics = check(buffer.content.collect(), state.config.spellCheck)
              SpellCheckCacheEntry(fingerprint, spellDiagnostics)
            }
          val nextDiagnostics =
            if entry.diagnostics.isEmpty then diagnostics
            else diagnostics + (uri -> entry.diagnostics)
          nextDiagnostics -> (cache + (uri -> entry))
        else diagnostics -> cache
    }

    state.copy(diagnostics = refreshed, spellCheckCache = cache)

  def analysisFingerprints(state: AppState): Map[String, SpellCheckFingerprint] =
    state.buffers.values
      .filter(buffer => shouldCheck(buffer, state.config.spellCheck))
      .map(buffer => diagnosticsUri(buffer) -> SpellCheckFingerprint.from(buffer, state.config.spellCheck))
      .toMap

  def applyIfCurrent(current: AppState, analyzed: AppState, expected: Map[String, SpellCheckFingerprint]): AppState =
    if analysisFingerprints(current) == expected then
      current.copy(
        diagnostics = analyzed.diagnostics,
        spellCheckCache = analyzed.spellCheckCache
      )
    else current

  def diagnosticsUri(buffer: Buffer): String =
    buffer.filePath.map(_.toUri.toString).getOrElse(bufferDiagnosticsUri(buffer.id))

  def bufferDiagnosticsUri(bufferId: BufferId): String =
    s"buffer:${bufferId.value}"

  private def shouldCheck(buffer: Buffer, config: SpellCheckConfig): Boolean =
    config.enabled && buffer.usesTextFont

  private def dictionaryFor(config: SpellCheckConfig): DictionaryContext =
    val externalResults = config.dictionarySourcePaths.map(loadDictionary)
    val externalWords   = externalResults.flatMap(_.words).toSet
    val failures        = externalResults.flatMap(_.failures)
    val fallbackWords =
      if config.dictionaryPaths.nonEmpty && externalWords.nonEmpty then Set.empty[String]
      else config.languages.flatMap(language => BuiltInDictionaries.getOrElse(language, Set.empty)).toSet

    DictionaryContext(
      words = (externalWords ++ fallbackWords ++ config.additionalWords).map(normalizeWord),
      failures = failures.distinct
    )

  private def loadDictionary(path: Path): DictionaryLoadResult =
    val fingerprint = SpellCheckDictionaryFingerprint.fromPath(path)
    val cacheKey = DictionaryCacheKey(
      fingerprint.path,
      fingerprint.size,
      fingerprint.lastModifiedMillis
    )
    DictionaryCache.computeIfAbsent(cacheKey, _ => readDictionary(path))

  private def readDictionary(path: Path): DictionaryLoadResult =
    if !Files.exists(path) then DictionaryLoadResult(Set.empty, List(s"Dictionary file does not exist: $path"))
    else if Files.isDirectory(path) then DictionaryLoadResult(Set.empty, List(s"Dictionary path is a directory: $path"))
    else
      try
        val lines = Files.readAllLines(path, StandardCharsets.UTF_8)
        val words = lines.toArray.toList
          .collect { case line: String => line.trim }
          .dropWhile(line => line.forall(_.isDigit))
          .filter(line => line.nonEmpty && !line.startsWith("#"))
          .flatMap(parseHunspellDictionaryWord)
          .map(normalizeWord)
          .toSet

        DictionaryLoadResult(words, Nil)
      catch
        case NonFatal(error) =>
          DictionaryLoadResult(Set.empty, List(s"Could not load dictionary $path: ${error.getMessage}"))

  private def parseHunspellDictionaryWord(line: String): Option[String] =
    val withoutMorphology = line.takeWhile(char => !char.isWhitespace)
    val word              = withoutMorphology.takeWhile(_ != '/').trim
    Option(word).filter(_.exists(_.isLetter))

  private def dictionaryLoadDiagnostics(failures: List[String]): List[Diagnostic] =
    failures.map { message =>
      Diagnostic(
        range = LspRange(LspPosition(0, 0), LspPosition(0, 0)),
        severity = Some(DiagnosticSeverity.Warning),
        message = message,
        source = Some(Source),
        code = Some("dictionary-load-failed")
      )
    }

  private def isAccepted(word: String, dictionary: Set[String]): Boolean =
    val normalized = normalizeWord(word)
    normalized.length < 3 || dictionary.contains(normalized)

  private def normalizeWord(word: String): String =
    word.toLowerCase(Locale.ROOT)

  private def isSpellCheckDiagnostic(diagnostic: Diagnostic): Boolean =
    diagnostic.source.contains(Source)
end SpellChecker
