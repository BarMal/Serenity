package com.serenity.spellcheck

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

import scala.util.control.NonFatal
import scala.util.matching.Regex

import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}
import com.serenity.lsp.model.*
import com.serenity.state.models.*

object SpellChecker:

  val Source: String = "spell-check"

  private val WordPattern = """[\p{L}\p{M}]+(?:['’-][\p{L}\p{M}]+)*""".r

  private val DictionaryCache                   = ConcurrentHashMap[DictionaryCacheKey, DictionaryLoadResult]()
  private val DefaultDictionaryCharset: Charset = StandardCharsets.UTF_8

  final private case class DictionaryCacheKey(fingerprints: List[SpellCheckDictionaryFingerprint])

  final private case class DictionaryLoadResult(
      words: Set[String],
      replacements: Map[String, List[String]],
      failures: List[String]
  )

  final private case class DictionaryContext(
      words: Set[String],
      replacements: Map[String, List[String]],
      failures: List[String]
  )

  private enum HunspellFlagMode:
    case Simple
    case Long
    case Num

  final private case class HunspellAffixRules(
      flagMode: HunspellFlagMode,
      flagAliases: Map[String, Set[String]],
      prefixes: Map[String, List[HunspellAffixRule]],
      suffixes: Map[String, List[HunspellAffixRule]],
      replacements: Map[String, List[String]]
  )

  private object HunspellAffixRules:
    val empty: HunspellAffixRules =
      HunspellAffixRules(HunspellFlagMode.Simple, Map.empty, Map.empty, Map.empty, Map.empty)

  final private case class HunspellAffixRule(strip: String, append: String, condition: String, combineable: Boolean)
  final private case class HunspellEntry(word: String, flags: Set[String])

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
                val word        = match_.matched
                val suggestions = dictionary.replacements.getOrElse(normalizeWord(word), Nil)
                Diagnostic(
                  range = LspRange(
                    LspPosition(lineIndex, match_.start),
                    LspPosition(lineIndex, match_.end)
                  ),
                  severity = Some(DiagnosticSeverity.Hint),
                  message = diagnosticMessage(word, suggestions),
                  source = Some(Source),
                  code = Some("unknown-word")
                )
              }
              .toList
          }
          .toList

  def refreshDiagnostics(state: AppState): AppState =
    val preserved = state.runtime.diagnosticsState.diagnostics.view
      .mapValues(_.filterNot(isSpellCheckDiagnostic))
      .filter(_._2.nonEmpty)
      .toMap

    val (refreshed, cache) =
      state.persisted.buffers.values.foldLeft((preserved, Map.empty[String, SpellCheckCacheEntry])) {
        case ((diagnostics, cache), buffer) =>
          val uri = diagnosticsUri(buffer)
          if shouldCheck(buffer, state.persisted.config.languageToolsConfig.spellCheck) then
            val fingerprint = SpellCheckFingerprint.from(buffer, state.persisted.config.languageToolsConfig.spellCheck)
            val entry = state.runtime.diagnosticsState.spellCheckCache
              .get(uri)
              .filter(_.fingerprint == fingerprint)
              .getOrElse {
                val spellDiagnostics =
                  check(buffer.document.content.collect(), state.persisted.config.languageToolsConfig.spellCheck)
                SpellCheckCacheEntry(fingerprint, spellDiagnostics)
              }
            val nextDiagnostics =
              if entry.diagnostics.isEmpty then diagnostics
              else diagnostics + (uri -> entry.diagnostics)
            nextDiagnostics -> (cache + (uri -> entry))
          else diagnostics -> cache
      }

    state.copy(runtime =
      state.runtime.copy(diagnosticsState =
        state.runtime.diagnosticsState.copy(diagnostics = refreshed, spellCheckCache = cache)
      )
    )

  def analysisFingerprints(state: AppState): Map[String, SpellCheckFingerprint] =
    state.persisted.buffers.values
      .filter(buffer => shouldCheck(buffer, state.persisted.config.languageToolsConfig.spellCheck))
      .map(buffer =>
        diagnosticsUri(buffer) -> SpellCheckFingerprint
          .from(buffer, state.persisted.config.languageToolsConfig.spellCheck)
      )
      .toMap

  def applyIfCurrent(current: AppState, analyzed: AppState, expected: Map[String, SpellCheckFingerprint]): AppState =
    if analysisFingerprints(current) == expected then
      current.copy(runtime =
        current.runtime.copy(diagnosticsState =
          current.runtime.diagnosticsState.copy(
            diagnostics = analyzed.runtime.diagnosticsState.diagnostics,
            spellCheckCache = analyzed.runtime.diagnosticsState.spellCheckCache
          )
        )
      )
    else current

  def diagnosticsUri(buffer: Buffer): String =
    buffer.document.filePath.map(_.toUri.toString).getOrElse(bufferDiagnosticsUri(buffer.id))

  def bufferDiagnosticsUri(bufferId: BufferId): String =
    s"buffer:${bufferId.value}"

  private def shouldCheck(buffer: Buffer, config: SpellCheckConfig): Boolean =
    config.enabled && buffer.usesTextFont

  private def dictionaryFor(config: SpellCheckConfig): DictionaryContext =
    val externalResults = config.dictionarySourcePaths.map(loadDictionary)
    val externalWords   = externalResults.flatMap(_.words).toSet
    val externalReplacements =
      mergeReplacementMaps(externalResults.map(_.replacements))
    val failures = externalResults.flatMap(_.failures)
    val fallbackWords =
      if config.dictionaryPaths.nonEmpty && externalWords.nonEmpty then Set.empty[String]
      else config.languages.flatMap(language => BuiltInDictionaries.getOrElse(language, Set.empty)).toSet

    DictionaryContext(
      words = (externalWords ++ fallbackWords ++ config.additionalWords).map(normalizeWord),
      replacements = externalReplacements,
      failures = failures.distinct
    )

  private def loadDictionary(path: Path): DictionaryLoadResult =
    val dependencyPaths = SpellCheckConfig.dictionaryDependencyPaths(List(path))
    val cacheKey        = DictionaryCacheKey(dependencyPaths.map(SpellCheckDictionaryFingerprint.fromPath))
    DictionaryCache.computeIfAbsent(cacheKey, _ => readDictionary(path))

  private def readDictionary(path: Path): DictionaryLoadResult =
    if !Files.exists(path) then
      DictionaryLoadResult(Set.empty, Map.empty, List(s"Dictionary file does not exist: $path"))
    else if Files.isDirectory(path) then
      DictionaryLoadResult(Set.empty, Map.empty, List(s"Dictionary path is a directory: $path"))
    else
      try
        val affixPath  = affixPathFor(path)
        val charset    = affixPath.map(readDeclaredCharset).getOrElse(DefaultDictionaryCharset)
        val affixRules = affixPath.map(parseAffixRules(_, charset)).getOrElse(HunspellAffixRules.empty)
        val lines      = Files.readAllLines(path, charset)
        val entries = lines.toArray.toList
          .collect { case line: String => line.trim }
          .dropWhile(line => line.forall(_.isDigit))
          .filter(line => line.nonEmpty && !line.startsWith("#"))
          .flatMap(line => parseHunspellDictionaryEntry(line, affixRules))

        val words = entries
          .flatMap(entry => expandHunspellEntry(entry, affixRules))
          .map(normalizeWord)
          .toSet

        DictionaryLoadResult(words, affixRules.replacements, Nil)
      catch
        case NonFatal(error) =>
          DictionaryLoadResult(Set.empty, Map.empty, List(s"Could not load dictionary $path: ${error.getMessage}"))

  private def affixPathFor(dictionaryPath: Path): Option[Path] =
    SpellCheckConfig
      .affixPathForDictionary(dictionaryPath)
      .filter(Files.exists(_))
      .filterNot(Files.isDirectory(_))

  private def readDeclaredCharset(path: Path): Charset =
    val lines =
      Files.readAllLines(path, StandardCharsets.ISO_8859_1).toArray.toList.collect { case line: String => line.trim }
    lines
      .collectFirst {
        case line if line.toUpperCase(Locale.ROOT).startsWith("SET ") =>
          line.drop(4).trim
      }
      .filter(_.nonEmpty)
      .map(Charset.forName)
      .getOrElse(DefaultDictionaryCharset)

  private def parseAffixRules(path: Path, charset: Charset): HunspellAffixRules =
    val lines =
      Files.readAllLines(path, charset).toArray.toList.collect { case line: String => line.trim }
    val flagMode     = parseFlagMode(lines)
    val flagAliases  = parseFlagAliases(lines, flagMode)
    val prefixRules  = parseAffixRules(lines, "PFX")
    val suffixRules  = parseAffixRules(lines, "SFX")
    val replacements = parseReplacements(lines)
    HunspellAffixRules(flagMode, flagAliases, prefixRules, suffixRules, replacements)

  private def parseFlagMode(lines: List[String]): HunspellFlagMode =
    lines
      .collectFirst {
        case line if line.startsWith("FLAG ") =>
          line.stripPrefix("FLAG ").trim.toLowerCase(Locale.ROOT)
      }
      .flatMap {
        case "long" => Some(HunspellFlagMode.Long)
        case "num"  => Some(HunspellFlagMode.Num)
        case _      => Some(HunspellFlagMode.Simple)
      }
      .getOrElse(HunspellFlagMode.Simple)

  private def parseAffixRules(lines: List[String], kind: String): Map[String, List[HunspellAffixRule]] =
    val combinability = parseAffixRuleCombinability(lines, kind)
    lines.foldLeft(Map.empty[String, List[HunspellAffixRule]]) { (rules, line) =>
      val columns = line.split("\\s+").toList
      columns match
        case ruleKind :: flag :: strip :: append :: condition :: _ if ruleKind == kind =>
          val rule = HunspellAffixRule(
            strip = zeroAsEmpty(strip),
            append = zeroAsEmpty(append.takeWhile(_ != '/')),
            condition = condition,
            combineable = combinability.getOrElse(flag, false)
          )
          rules.updated(flag, rules.getOrElse(flag, Nil) :+ rule)
        case _ => rules
    }

  private def parseAffixRuleCombinability(lines: List[String], kind: String): Map[String, Boolean] =
    lines.foldLeft(Map.empty[String, Boolean]) { (combinability, line) =>
      line.split("\\s+").toList match
        case ruleKind :: flag :: crossProduct :: count :: Nil if ruleKind == kind && count.forall(_.isDigit) =>
          combinability.updated(flag, crossProduct.equalsIgnoreCase("Y"))
        case _ => combinability
    }

  private def parseFlagAliases(lines: List[String], flagMode: HunspellFlagMode): Map[String, Set[String]] =
    val (aliases, _) =
      lines.foldLeft((Map.empty[String, Set[String]], 0)) {
        case ((aliases, aliasIndex), line) =>
          line.split("\\s+").toList match
            case "AF" :: count :: Nil if count.forall(_.isDigit) =>
              aliases -> aliasIndex
            case "AF" :: flags :: _ =>
              val nextIndex = aliasIndex + 1
              aliases.updated(nextIndex.toString, parseHunspellFlagList(flags, flagMode)) -> nextIndex
            case _ =>
              aliases -> aliasIndex
      }
    aliases

  private def parseReplacements(lines: List[String]): Map[String, List[String]] =
    lines.foldLeft(Map.empty[String, List[String]]) { (replacements, line) =>
      line.split("\\s+").toList match
        case "REP" :: source :: replacement :: _ if !source.forall(_.isDigit) =>
          val key   = normalizeWord(source)
          val value = normalizeWord(replacement)
          replacements.updated(key, (replacements.getOrElse(key, Nil) :+ value).distinct)
        case _ =>
          replacements
    }

  private def parseHunspellDictionaryEntry(line: String, affixRules: HunspellAffixRules): Option[HunspellEntry] =
    val withoutMorphology = line.takeWhile(char => !char.isWhitespace)
    val word              = withoutMorphology.takeWhile(_ != '/').trim
    Option(word)
      .filter(_.exists(_.isLetter))
      .map(word => HunspellEntry(word, parseHunspellFlags(withoutMorphology, affixRules)))

  private def parseHunspellFlags(entry: String, affixRules: HunspellAffixRules): Set[String] =
    entry.dropWhile(_ != '/') match
      case "" => Set.empty
      case flagsWithSlash =>
        val flags = flagsWithSlash.drop(1)
        affixRules.flagAliases.getOrElse(flags, parseHunspellFlagList(flags, affixRules.flagMode))

  private def parseHunspellFlagList(flags: String, flagMode: HunspellFlagMode): Set[String] =
    flagMode match
      case HunspellFlagMode.Simple =>
        flags.toList.map(_.toString).toSet
      case HunspellFlagMode.Long =>
        flags.grouped(2).filter(_.length == 2).toSet
      case HunspellFlagMode.Num =>
        flags.split(",").map(_.trim).filter(_.nonEmpty).toSet

  private def expandHunspellEntry(entry: HunspellEntry, affixRules: HunspellAffixRules): Set[String] =
    val prefixRules = entry.flags.flatMap(flag => affixRules.prefixes.getOrElse(flag, Nil))
    val suffixRules = entry.flags.flatMap(flag => affixRules.suffixes.getOrElse(flag, Nil))
    val prefixes    = prefixRules.flatMap(applyPrefix(entry.word, _))
    val suffixes    = suffixRules.flatMap(applySuffix(entry.word, _))
    val combined =
      for
        prefixRule <- prefixRules if prefixRule.combineable
        suffixRule <- suffixRules if suffixRule.combineable
        suffixed   <- applySuffix(entry.word, suffixRule)
        combined   <- applyPrefix(suffixed, prefixRule)
      yield combined
    Set(entry.word) ++ prefixes ++ suffixes ++ combined

  private def applyPrefix(word: String, rule: HunspellAffixRule): Option[String] =
    Option.when(word.startsWith(rule.strip) && prefixConditionMatches(word, rule.condition)) {
      rule.append + word.drop(rule.strip.length)
    }

  private def applySuffix(word: String, rule: HunspellAffixRule): Option[String] =
    Option.when(word.endsWith(rule.strip) && suffixConditionMatches(word, rule.condition)) {
      word.dropRight(rule.strip.length) + rule.append
    }

  private def prefixConditionMatches(word: String, condition: String): Boolean =
    condition == "." || regexMatches(s"^(?:$condition).*", word)

  private def suffixConditionMatches(word: String, condition: String): Boolean =
    condition == "." || regexMatches(s".*(?:$condition)$$", word)

  private def regexMatches(pattern: String, word: String): Boolean =
    try Regex(pattern).pattern.matcher(word).matches
    catch case NonFatal(_) => false

  private def zeroAsEmpty(value: String): String =
    if value == "0" then "" else value

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

  private def diagnosticMessage(word: String, suggestions: List[String]): String =
    val base = s"Possible spelling issue: $word"
    suggestions.distinct match
      case Nil =>
        base
      case suggestion :: Nil =>
        s"$base (suggestion: $suggestion)"
      case values =>
        s"$base (suggestions: ${values.mkString(", ")})"

  private def mergeReplacementMaps(maps: List[Map[String, List[String]]]): Map[String, List[String]] =
    maps.foldLeft(Map.empty[String, List[String]]) { (merged, replacements) =>
      replacements.foldLeft(merged) {
        case (acc, (source, suggestions)) =>
          acc.updated(source, (acc.getOrElse(source, Nil) ++ suggestions).distinct)
      }
    }

  private def isAccepted(word: String, dictionary: Set[String]): Boolean =
    val normalized = normalizeWord(word)
    normalized.length < 3 || dictionary.contains(normalized)

  private def normalizeWord(word: String): String =
    word.toLowerCase(Locale.ROOT)

  private def isSpellCheckDiagnostic(diagnostic: Diagnostic): Boolean =
    diagnostic.source.contains(Source)
end SpellChecker
