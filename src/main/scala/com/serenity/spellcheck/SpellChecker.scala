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

  /** One entry per normalized dictionary path, holding only the most recently loaded version of that dictionary. A path
    * whose fingerprint no longer matches is replaced in place rather than accumulating a new entry, so repeated
    * dictionary edits cannot grow this map without bound.
    */
  private val DictionaryCache                   = ConcurrentHashMap[String, DictionaryCacheEntry]()
  private val DefaultDictionaryCharset: Charset = StandardCharsets.UTF_8

  /** Number of distinct dictionary paths currently cached -- exposed only so tests can assert the cache stays bounded
    * to one entry per normalized path rather than growing with every historical fingerprint.
    */
  private[serenity] def dictionaryCacheSize: Int = DictionaryCache.size()

  final private case class DictionaryCacheEntry(
      fingerprints: List[SpellCheckDictionaryFingerprint],
      result: DictionaryLoadResult
  )

  final private case class DictionaryLoadResult(
      words: Set[String],
      replacements: Map[String, List[String]],
      failures: List[String],
      iconv: List[(String, String)],
      oconv: List[(String, String)],
      compoundRules: List[String],
      compoundMin: Int,
      compoundWordFlags: Map[String, Set[String]]
  )

  /** Immutable, already-loaded dictionary data that pure analysis (`analyzeText`, `refreshDiagnostics`) consumes.
    * Building one performs no filesystem IO; only `loadDictionarySnapshot` does.
    *
    * `iconv`/`oconv` are the merged Hunspell ICONV/OCONV conversion tables (issue #1182) across every loaded
    * dictionary, applied respectively to words before dictionary lookup and to REP-based suggestions before display.
    *
    * `compoundRules`/`compoundMin`/`compoundWordFlags` (issue #1187) back Hunspell COMPOUNDRULE matching: the raw
    * `COMPOUNDRULE` pattern strings, the merged `COMPOUNDMIN` (minimum compound-member length, default 3), and every
    * dictionary word's flags keyed by its normalized text -- the same shape `matchesCompoundRule` needs to recognize an
    * unmatched word as a valid compound without re-reading the filesystem.
    */
  final case class DictionaryContext(
      words: Set[String],
      replacements: Map[String, List[String]],
      failures: List[String],
      iconv: List[(String, String)] = Nil,
      oconv: List[(String, String)] = Nil,
      compoundRules: List[String] = Nil,
      compoundMin: Int = 3,
      compoundWordFlags: Map[String, Set[String]] = Map.empty
  )

  /** The result of one explicit dictionary-discovery pass: the loaded words/replacements/failures plus the on-disk
    * fingerprints that produced them. Obtain one via `loadDictionarySnapshot`, called from `IO.blocking`, then thread
    * it into `refreshDiagnostics`/`analysisFingerprints`/`applyIfCurrent` so those stay pure.
    */
  final case class DictionarySnapshot(
      context: DictionaryContext,
      fingerprints: List[SpellCheckDictionaryFingerprint]
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
      replacements: Map[String, List[String]],
      needAffixFlag: Option[String],
      iconv: List[(String, String)],
      oconv: List[(String, String)],
      compoundRules: List[String],
      compoundMin: Int,
      circumfixFlag: Option[String]
  )

  private object HunspellAffixRules:

    val empty: HunspellAffixRules =
      HunspellAffixRules(
        HunspellFlagMode.Simple,
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        None,
        Nil,
        Nil,
        Nil,
        3,
        None
      )

  /** `continuationFlags` (issue #1187) are the flags attached after '/' in a PFX/SFX rule's append field --
    * hunspell(5)'s continuation classes, granted to the derived word for further affixation. Only CIRCUMFIX consumes
    * them here (see `expandHunspellEntry`); every other rule ignores them exactly as before, so a dictionary using
    * continuation classes for anything else sees no behavior change.
    */
  final private case class HunspellAffixRule(
      strip: String,
      append: String,
      condition: String,
      combineable: Boolean,
      continuationFlags: Set[String]
  )

  final private case class HunspellEntry(word: String, flags: Set[String])

  private enum CompoundQuantifier:
    case Exactly, ZeroOrOne, ZeroOrMore

  final private case class CompoundToken(flag: String, quantifier: CompoundQuantifier)

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

  /** Convenience entry point that discovers and loads dictionaries itself -- handy for tests and one-off checks, but it
    * performs filesystem IO synchronously and so must never be called from a pure state method or from inside
    * `Ref.update`. Production analysis instead calls `loadDictionarySnapshot` explicitly from `IO.blocking` and passes
    * the resulting `DictionaryContext` into the pure `analyzeText`.
    */
  def check(text: String, config: SpellCheckConfig): List[Diagnostic] =
    analyzeText(text, config, loadDictionarySnapshot(config).context)

  /** Pure: matches `text` against an already-loaded `dictionary`. Performs no filesystem access -- its signature
    * carries no `Path`, so there is nothing here for a future change to accidentally turn into IO.
    */
  def analyzeText(text: String, config: SpellCheckConfig, dictionary: DictionaryContext): List[Diagnostic] =
    val normalized = config.normalized
    if !normalized.enabled then Nil
    else
      dictionaryLoadDiagnostics(dictionary.failures) ++
        text
          .split("\n", -1)
          .zipWithIndex
          .flatMap { (line, lineIndex) =>
            WordPattern
              .findAllMatchIn(line)
              // ICONV (#1182): normalize input character variants (ligatures, alternate quote glyphs, ...) to the
              // form the dictionary was built from before checking membership -- exactly what hunspell itself does
              // before matching checked text against the dictionary.
              .filterNot(match_ => isAccepted(applyConversionTable(match_.matched, dictionary.iconv), dictionary))
              .map { match_ =>
                val word          = match_.matched
                val convertedWord = applyConversionTable(word, dictionary.iconv)
                // OCONV (#1182): applied only to generated suggestions, matching hunspell's output-conversion
                // semantics -- the word as typed (`word`, above) is shown unconverted in the diagnostic message.
                val suggestions = dictionary.replacements
                  .getOrElse(normalizeWord(convertedWord), Nil)
                  .map(applyConversionTable(_, dictionary.oconv))
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

  /** Pure: recomputes cached diagnostics against an already-loaded `dictionary` snapshot. Callers obtain that snapshot
    * once via `loadDictionarySnapshot` inside `IO.blocking`, then pass the same immutable value here -- this method
    * itself never touches the filesystem, so it is safe to call from inside `Ref.update`.
    */
  def refreshDiagnostics(state: AppState, dictionary: DictionarySnapshot): AppState =
    val preserved = state.runtime.diagnosticsState.diagnostics.view
      .mapValues(_.filterNot(isSpellCheckDiagnostic))
      .filter(_._2.nonEmpty)
      .toMap

    val (refreshed, cache) =
      state.persisted.buffers.values.foldLeft((preserved, Map.empty[String, SpellCheckCacheEntry])) {
        case ((diagnostics, cache), buffer) =>
          val uri = diagnosticsUri(buffer)
          if shouldCheck(buffer, state.persisted.config.languageToolsConfig.spellCheck) then
            val fingerprint = SpellCheckFingerprint.from(
              buffer,
              state.persisted.config.languageToolsConfig.spellCheck,
              dictionary.fingerprints
            )
            val entry = state.runtime.diagnosticsState.spellCheckCache
              .get(uri)
              .filter(_.fingerprint == fingerprint)
              .getOrElse {
                val spellDiagnostics = analyzeText(
                  buffer.document.content.collect(),
                  state.persisted.config.languageToolsConfig.spellCheck,
                  dictionary.context
                )
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

  /** Pure: `dictionaryFingerprints` must be discovered once (via `SpellCheckConfig.discoverDictionaryFingerprints` or
    * `loadDictionarySnapshot`, both `IO.blocking`) and passed in -- this method never reads the filesystem, so it is
    * safe to call from inside `Ref.update` when comparing against a state commit's expected fingerprints.
    */
  def analysisFingerprints(
    state: AppState,
    dictionaryFingerprints: List[SpellCheckDictionaryFingerprint]
  ): Map[String, SpellCheckFingerprint] =
    state.persisted.buffers.values
      .filter(buffer => shouldCheck(buffer, state.persisted.config.languageToolsConfig.spellCheck))
      .map(buffer =>
        diagnosticsUri(buffer) -> SpellCheckFingerprint
          .from(buffer, state.persisted.config.languageToolsConfig.spellCheck, dictionaryFingerprints)
      )
      .toMap

  /** Pure: publishes `analyzed` onto `current` only if `current` still matches the fingerprints the analysis was
    * computed against, rejecting stale results from a buffer or dictionary that changed mid-analysis.
    * `dictionaryFingerprints` is the same precomputed value used to build `expected`, not re-read here -- this is what
    * makes the comparison safe to run from inside `Ref.update`.
    */
  def applyIfCurrent(
    current: AppState,
    analyzed: AppState,
    expected: Map[String, SpellCheckFingerprint],
    dictionaryFingerprints: List[SpellCheckDictionaryFingerprint]
  ): AppState =
    if analysisFingerprints(current, dictionaryFingerprints) == expected then
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

  /** The one function that performs all dictionary discovery, reading and fingerprinting -- explicit filesystem IO
    * throughout. Callers must invoke this from `IO.blocking` and thread the resulting immutable snapshot into the pure
    * analysis methods above rather than calling this (or `check`) from a state-commit path.
    */
  def loadDictionarySnapshot(config: SpellCheckConfig): DictionarySnapshot =
    val normalized      = config.normalized
    val sourcePaths     = SpellCheckConfig.discoverDictionarySourcePaths(normalized)
    val externalResults = sourcePaths.map(loadDictionary)
    val externalWords   = externalResults.flatMap(_.words).toSet
    val externalReplacements =
      mergeReplacementMaps(externalResults.map(_.replacements))
    val failures = externalResults.flatMap(_.failures)
    val fallbackWords =
      if normalized.dictionaryPaths.nonEmpty && externalWords.nonEmpty then Set.empty[String]
      else normalized.languages.flatMap(language => BuiltInDictionaries.getOrElse(language, Set.empty)).toSet

    val context = DictionaryContext(
      words = (externalWords ++ fallbackWords ++ normalized.additionalWords).map(normalizeWord),
      replacements = externalReplacements,
      failures = failures.distinct,
      iconv = externalResults.flatMap(_.iconv).distinct,
      oconv = externalResults.flatMap(_.oconv).distinct,
      compoundRules = externalResults.flatMap(_.compoundRules).distinct,
      compoundMin = externalResults.map(_.compoundMin).foldLeft(3)(math.min),
      compoundWordFlags = mergeCompoundWordFlags(externalResults.map(_.compoundWordFlags))
    )
    DictionarySnapshot(context, SpellCheckConfig.discoverDictionaryFingerprints(normalized))

  /** Loads (or reuses) the dictionary at `path`, keyed by its normalized path so a later call with a changed
    * fingerprint replaces the cached entry rather than adding a new one -- the cache never holds more than one loaded
    * dictionary per distinct path.
    */
  private def loadDictionary(path: Path): DictionaryLoadResult =
    val normalizedPath  = path.toAbsolutePath.normalize().toString
    val dependencyPaths = SpellCheckConfig.dictionaryDependencyPaths(List(path))
    val fingerprints    = dependencyPaths.map(SpellCheckDictionaryFingerprint.fromPath)
    DictionaryCache
      .compute(
        normalizedPath,
        (_, existing) =>
          Option(existing)
            .filter(_.fingerprints == fingerprints)
            .getOrElse(DictionaryCacheEntry(fingerprints, readDictionary(path)))
      )
      .result

  private def readDictionary(path: Path): DictionaryLoadResult =
    if !Files.exists(path) then
      DictionaryLoadResult(
        Set.empty,
        Map.empty,
        List(s"Dictionary file does not exist: $path"),
        Nil,
        Nil,
        Nil,
        3,
        Map.empty
      )
    else if Files.isDirectory(path) then
      DictionaryLoadResult(
        Set.empty,
        Map.empty,
        List(s"Dictionary path is a directory: $path"),
        Nil,
        Nil,
        Nil,
        3,
        Map.empty
      )
    else
      try
        val affixPath      = affixPathFor(path)
        val charset        = affixPath.map(readDeclaredCharset).getOrElse(DefaultDictionaryCharset)
        val affixLines     = affixPath.map(readTrimmedLines(_, charset)).getOrElse(Nil)
        val affixRules     = affixPath.map(_ => parseAffixRules(affixLines)).getOrElse(HunspellAffixRules.empty)
        val unsupportedAff = affixPath.map(unsupportedAffixDirectives(affixLines, _)).getOrElse(Nil)
        val lines          = Files.readAllLines(path, charset)
        val entries = lines.toArray.toList
          .collect { case line: String => line.trim }
          .dropWhile(line => line.forall(_.isDigit))
          .filter(line => line.nonEmpty && !line.startsWith("#"))
          .flatMap(line => parseHunspellDictionaryEntry(line, affixRules))

        val words = entries
          .flatMap(entry => expandHunspellEntry(entry, affixRules))
          .map(normalizeWord)
          .toSet

        // COMPOUNDRULE (#1187) matches compound candidates against dictionary entries' own flags, keyed by their
        // normalized text -- computed only when the affix file actually declares compounding, since it is otherwise
        // unused.
        val compoundWordFlags =
          if affixRules.compoundRules.isEmpty then Map.empty[String, Set[String]]
          else entries.groupMapReduce(entry => normalizeWord(entry.word))(_.flags)(_ ++ _)

        DictionaryLoadResult(
          words,
          affixRules.replacements,
          unsupportedAff,
          affixRules.iconv,
          affixRules.oconv,
          affixRules.compoundRules,
          affixRules.compoundMin,
          compoundWordFlags
        )
      catch
        case NonFatal(error) =>
          DictionaryLoadResult(
            Set.empty,
            Map.empty,
            List(s"Could not load dictionary $path: ${error.getMessage}"),
            Nil,
            Nil,
            Nil,
            3,
            Map.empty
          )

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

  private def readTrimmedLines(path: Path, charset: Charset): List[String] =
    Files.readAllLines(path, charset).toArray.toList.collect { case line: String => line.trim }

  private def parseAffixRules(lines: List[String]): HunspellAffixRules =
    val flagMode      = parseFlagMode(lines)
    val flagAliases   = parseFlagAliases(lines, flagMode)
    val prefixRules   = parseAffixRules(lines, "PFX", flagMode)
    val suffixRules   = parseAffixRules(lines, "SFX", flagMode)
    val replacements  = parseReplacements(lines)
    val needAffixFlag = parseNeedAffixFlag(lines)
    val iconv         = parseConversionTable(lines, "ICONV")
    val oconv         = parseConversionTable(lines, "OCONV")
    val compoundRules = parseCompoundRules(lines)
    val compoundMin   = parseCompoundMin(lines)
    val circumfixFlag = parseCircumfixFlag(lines)
    HunspellAffixRules(
      flagMode,
      flagAliases,
      prefixRules,
      suffixRules,
      replacements,
      needAffixFlag,
      iconv,
      oconv,
      compoundRules,
      compoundMin,
      circumfixFlag
    )

  /** Directives from the Hunspell affix format that this handwritten parser does not implement (the free-form
    * COMPOUNDFLAG compounding mechanism, morphological generation, and similar). Rather than silently ignoring them --
    * which would mis-flag words that rely on them -- their presence is surfaced as an explicit dictionary-load
    * diagnostic. See the PR description for why this project carries a partial parser instead of a dependency on
    * Lucene's Hunspell implementation.
    *
    * ICONV/OCONV and NEEDAFFIX (issue #1182), and COMPOUNDRULE/COMPOUNDMIN and CIRCUMFIX (issue #1187), are implemented
    * and intentionally absent from this set -- see `parseConversionTable`/`parseNeedAffixFlag` and
    * `parseCompoundRules`/`parseCompoundMin`/`parseCircumfixFlag`. COMPOUNDFLAG/COMPOUNDBEGIN/COMPOUNDMIDDLE/
    * COMPOUNDLAST (Hunspell's free-form dictionary word-segmentation compounding, as opposed to COMPOUNDRULE's explicit
    * flag grammar) and the CHECKCOMPOUND family (plus SIMPLIFIEDTRIPLE) validation directives remain unsupported and
    * are deferred to a follow-up issue -- see the PR description for why.
    */
  private val UnsupportedAffixDirectives = Set(
    "COMPOUNDFLAG",
    "COMPOUNDBEGIN",
    "COMPOUNDMIDDLE",
    "COMPOUNDLAST",
    "COMPOUNDWORDMAX",
    "COMPOUNDSYLLABLE",
    "SYLLABLENUM",
    "ONLYINCOMPOUND",
    "CHECKCOMPOUNDCASE",
    "CHECKCOMPOUNDDUP",
    "CHECKCOMPOUNDREP",
    "CHECKCOMPOUNDTRIPLE",
    "CHECKCOMPOUNDPATTERN",
    "SIMPLIFIEDTRIPLE",
    "PSEUDOROOT",
    "FORBIDDENWORD",
    "WARN",
    "FORBIDWARN",
    "LEMMA_PRESENT",
    "COMPLEXPREFIXES",
    "KEEPCASE",
    "FULLSTRIP",
    "BREAK",
    "MAP",
    "PHONE",
    "IGNORE"
  )

  private def unsupportedAffixDirectives(lines: List[String], affixPath: Path): List[String] =
    lines
      .flatMap(_.split("\\s+").toList.headOption)
      .filter(UnsupportedAffixDirectives.contains)
      .distinct
      .map(directive =>
        s"Unsupported Hunspell affix directive '$directive' in $affixPath is not applied " +
          "(words relying on it may be mis-flagged)"
      )

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

  private def parseAffixRules(
    lines: List[String],
    kind: String,
    flagMode: HunspellFlagMode
  ): Map[String, List[HunspellAffixRule]] =
    val combinability = parseAffixRuleCombinability(lines, kind)
    lines.foldLeft(Map.empty[String, List[HunspellAffixRule]]) { (rules, line) =>
      val columns = line.split("\\s+").toList
      columns match
        case ruleKind :: flag :: strip :: appendField :: condition :: _ if ruleKind == kind =>
          val (appendText, continuationFlags) = appendField.split("/", 2) match
            case Array(text, continuation) => text        -> parseHunspellFlagList(continuation, flagMode)
            case _                         => appendField -> Set.empty[String]
          val rule = HunspellAffixRule(
            strip = zeroAsEmpty(strip),
            append = zeroAsEmpty(appendText),
            condition = condition,
            combineable = combinability.getOrElse(flag, false),
            continuationFlags = continuationFlags
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

  /** `NEEDAFFIX <flag>` (hunspell(5)): marks a root as a "virtual stem", valid only when affixed -- the bare root must
    * be excluded from the accepted word set even though it appears in the .dic file, while its affixed forms remain
    * valid. `<flag>` is a single flag token in whatever representation the file's FLAG mode uses, matched directly
    * against an entry's parsed flag set.
    */
  private def parseNeedAffixFlag(lines: List[String]): Option[String] =
    lines
      .collectFirst {
        case line if line.startsWith("NEEDAFFIX ") => line.stripPrefix("NEEDAFFIX ").trim
      }
      .filter(_.nonEmpty)

  /** `ICONV`/`OCONV` (hunspell(5)): an input/output character conversion table, one `directive from to` line per entry
    * after the `directive count` header. Real-world dictionaries (en_US.aff, fr_FR.aff, nl_NL.aff, and others) use it
    * for ligature and typographic-quote normalization, e.g. `ICONV ﬁ fi`. This implements only the common
    * literal-substring form seen in every real dictionary checked for issue #1182; the rarer `_` end-of-word anchor
    * form is read as an ordinary pattern rather than specially handled, so a rule using it is a safe no-op (its
    * pattern, containing a literal underscore, will not occur in real words) instead of matching the wrong position --
    * no real dictionary surveyed for this issue used it.
    */
  private def parseConversionTable(lines: List[String], directive: String): List[(String, String)] =
    lines.flatMap { line =>
      line.split("\\s+").toList match
        case candidate :: from :: to :: Nil if candidate == directive => Some(from -> to)
        case _                                                        => None
    }

  private def applyConversionTable(word: String, table: List[(String, String)]): String =
    table.foldLeft(word) { case (converted, (from, to)) => converted.replace(from, to) }

  /** `CIRCUMFIX <flag>` (hunspell(5)): the flag that, when carried in a PFX/SFX rule's continuation class, marks that
    * rule as usable only paired with its circumfix counterpart -- see `expandHunspellEntry`.
    */
  private def parseCircumfixFlag(lines: List[String]): Option[String] =
    lines
      .collectFirst {
        case line if line.startsWith("CIRCUMFIX ") => line.stripPrefix("CIRCUMFIX ").trim
      }
      .filter(_.nonEmpty)

  /** `COMPOUNDMIN <num>` (hunspell(5)): the minimum length, in characters, of a word usable as a compound member.
    * Defaults to 3, hunspell's own documented default, when absent.
    */
  private def parseCompoundMin(lines: List[String]): Int =
    lines
      .collectFirst {
        case line if line.startsWith("COMPOUNDMIN ") => line.stripPrefix("COMPOUNDMIN ").trim.toIntOption
      }
      .flatten
      .getOrElse(3)

  /** `COMPOUNDRULE` (hunspell(5)): one `COMPOUNDRULE count` header (skipped here -- its all-digit body distinguishes it
    * from a pattern line) followed by `count` `COMPOUNDRULE pattern` lines, each a small regex-like grammar over
    * compound flags (`tokenizeCompoundPattern` parses the grammar itself; the raw pattern strings are kept here and
    * re-tokenized by the pure, filesystem-free matching path so this stays a plain `List[String]` that
    * `DictionaryContext` -- a public type -- can carry without exposing `CompoundToken`).
    */
  private def parseCompoundRules(lines: List[String]): List[String] =
    lines.flatMap { line =>
      line.split("\\s+").toList match
        case "COMPOUNDRULE" :: pattern :: Nil if !pattern.forall(_.isDigit) => Some(pattern)
        case _                                                              => None
    }

  /** Parses one COMPOUNDRULE pattern into flag/quantifier tokens. A flag is either a single character (Simple flag
    * mode) or a parenthesized group (`(XX)`/`(1234)`, mandatory for Long/Num flag modes per hunspell(5): "With long and
    * numerical flag types, use only parenthesized flags"), optionally followed by `*` (0 or more) or `?` (0 or 1); a
    * flag with neither suffix matches exactly once.
    */
  private def tokenizeCompoundPattern(pattern: String): List[CompoundToken] =
    def loop(remaining: String, acc: List[CompoundToken]): List[CompoundToken] =
      if remaining.isEmpty then acc.reverse
      else
        val (flag, afterFlag) =
          if remaining.head == '(' then
            val closeIndex = remaining.indexOf(')')
            if closeIndex < 0 then (remaining.drop(1), "")
            else (remaining.substring(1, closeIndex), remaining.substring(closeIndex + 1))
          else (remaining.head.toString, remaining.tail)
        val (quantifier, rest) = afterFlag.headOption match
          case Some('*') => (CompoundQuantifier.ZeroOrMore, afterFlag.drop(1))
          case Some('?') => (CompoundQuantifier.ZeroOrOne, afterFlag.drop(1))
          case _         => (CompoundQuantifier.Exactly, afterFlag)
        loop(rest, CompoundToken(flag, quantifier) :: acc)
    loop(pattern, Nil)

  /** Every dictionary word (from `compoundWordFlags`) that `remaining` starts with, is at least `compoundMin`
    * characters long, and carries `flag` -- paired with what remains of the candidate compound after removing it. Real
    * dictionary words are never empty, so this always strictly shortens `remaining`, which is what guarantees
    * `compoundMatches`/`compoundStarMatches` below terminate.
    */
  private def compoundMemberCandidates(
    flag: String,
    remaining: String,
    compoundWordFlags: Map[String, Set[String]],
    compoundMin: Int
  ): List[String] =
    compoundWordFlags.iterator.collect {
      case (word, flags) if word.length >= compoundMin && flags.contains(flag) && remaining.startsWith(word) =>
        remaining.drop(word.length)
    }.toList

  /** Matches `remaining` against a COMPOUNDRULE pattern's tokens, recursively segmenting it into dictionary words
    * flagged for each token in turn; succeeds only when every token is satisfied and the entire candidate is consumed.
    * Mirrors hunspell(5)'s own description: pattern matching and word segmentation happen together, not as separate
    * phases, since which segmentation is valid depends on which flags the pattern still needs.
    */
  private def compoundMatches(
    tokens: List[CompoundToken],
    remaining: String,
    compoundWordFlags: Map[String, Set[String]],
    compoundMin: Int
  ): Boolean =
    tokens match
      case Nil => remaining.isEmpty
      case token :: rest =>
        token.quantifier match
          case CompoundQuantifier.Exactly =>
            compoundMemberCandidates(token.flag, remaining, compoundWordFlags, compoundMin)
              .exists(next => compoundMatches(rest, next, compoundWordFlags, compoundMin))
          case CompoundQuantifier.ZeroOrOne =>
            compoundMatches(rest, remaining, compoundWordFlags, compoundMin) ||
            compoundMemberCandidates(token.flag, remaining, compoundWordFlags, compoundMin)
              .exists(next => compoundMatches(rest, next, compoundWordFlags, compoundMin))
          case CompoundQuantifier.ZeroOrMore =>
            compoundStarMatches(token, rest, remaining, compoundWordFlags, compoundMin)

  private def compoundStarMatches(
    token: CompoundToken,
    rest: List[CompoundToken],
    remaining: String,
    compoundWordFlags: Map[String, Set[String]],
    compoundMin: Int
  ): Boolean =
    compoundMatches(rest, remaining, compoundWordFlags, compoundMin) ||
      compoundMemberCandidates(token.flag, remaining, compoundWordFlags, compoundMin)
        .exists(next => compoundStarMatches(token, rest, next, compoundWordFlags, compoundMin))

  private def matchesCompoundRule(word: String, dictionary: DictionaryContext): Boolean =
    dictionary.compoundRules.nonEmpty && dictionary.compoundRules.exists { pattern =>
      compoundMatches(tokenizeCompoundPattern(pattern), word, dictionary.compoundWordFlags, dictionary.compoundMin)
    }

  private def mergeCompoundWordFlags(maps: List[Map[String, Set[String]]]): Map[String, Set[String]] =
    maps.foldLeft(Map.empty[String, Set[String]]) { (merged, wordFlags) =>
      wordFlags.foldLeft(merged) {
        case (acc, (word, flags)) => acc.updated(word, acc.getOrElse(word, Set.empty) ++ flags)
      }
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

    def isCircumfix(rule: HunspellAffixRule): Boolean =
      affixRules.circumfixFlag.exists(rule.continuationFlags.contains)
    def circumfixPairValid(prefixRule: HunspellAffixRule, suffixRule: HunspellAffixRule): Boolean =
      isCircumfix(prefixRule) == isCircumfix(suffixRule)

    // CIRCUMFIX (#1187, hunspell(5)): an affix whose continuation class carries the CIRCUMFIX flag may never
    // surface on its own -- only paired with a counterpart that is itself CIRCUMFIX-flagged.
    val prefixes = prefixRules.filterNot(isCircumfix).flatMap(applyPrefix(entry.word, _))
    val suffixes = suffixRules.filterNot(isCircumfix).flatMap(applySuffix(entry.word, _))

    val combined =
      for
        prefixRule <- prefixRules if prefixRule.combineable
        suffixRule <- suffixRules if suffixRule.combineable && circumfixPairValid(prefixRule, suffixRule)
        suffixed   <- applySuffix(entry.word, suffixRule)
        combined   <- applyPrefix(suffixed, prefixRule)
      yield combined

    // CIRCUMFIX continuation chaining: the canonical Hungarian superlative ("legnagyobb") only reaches its prefix
    // via the suffix's continuation class -- the bare root never carries the prefix's own flag directly (see the
    // circumfix.aff fixture in hunspell's own test suite) -- so a CIRCUMFIX-flagged affix also looks for its
    // counterpart among the flags the *other* affix's continuation class grants, applying only when that
    // counterpart is itself CIRCUMFIX-flagged.
    val suffixThenPrefix: Set[String] =
      suffixRules.filter(isCircumfix).flatMap { suffixRule =>
        applySuffix(entry.word, suffixRule).toList.flatMap { suffixed =>
          suffixRule.continuationFlags
            .flatMap(flag => affixRules.prefixes.getOrElse(flag, Nil))
            .filter(isCircumfix)
            .flatMap(prefixRule => applyPrefix(suffixed, prefixRule))
        }
      }
    val prefixThenSuffix: Set[String] =
      prefixRules.filter(isCircumfix).flatMap { prefixRule =>
        applyPrefix(entry.word, prefixRule).toList.flatMap { prefixed =>
          prefixRule.continuationFlags
            .flatMap(flag => affixRules.suffixes.getOrElse(flag, Nil))
            .filter(isCircumfix)
            .flatMap(suffixRule => applySuffix(prefixed, suffixRule))
        }
      }

    // NEEDAFFIX (#1182): a root flagged with the configured NEEDAFFIX flag is a "virtual stem" -- valid only
    // affixed, per hunspell(5) -- so the bare word is dropped here while its affixed forms above are kept.
    val standalone = if affixRules.needAffixFlag.exists(entry.flags.contains) then Set.empty else Set(entry.word)
    standalone ++ prefixes ++ suffixes ++ combined ++ suffixThenPrefix ++ prefixThenSuffix

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

  private def isAccepted(word: String, dictionary: DictionaryContext): Boolean =
    val normalized = normalizeWord(word)
    normalized.length < 3 || dictionary.words.contains(normalized) || matchesCompoundRule(normalized, dictionary)

  private def normalizeWord(word: String): String =
    word.toLowerCase(Locale.ROOT)

  private def isSpellCheckDiagnostic(diagnostic: Diagnostic): Boolean =
    diagnostic.source.contains(Source)
end SpellChecker
