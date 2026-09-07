package com.serenity.spellcheck

import java.nio.file.Path
import java.util.Locale

import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}
import com.serenity.lsp.model.*
import com.serenity.state.models.*

object SpellChecker:

  val Source: String = "spell-check"

  private val WordPattern = """[\p{L}\p{M}]+(?:['’-][\p{L}\p{M}]+)*""".r

  /** Number of distinct dictionary paths currently cached -- exposed only so tests can assert the cache stays bounded
    * to one entry per normalized path rather than growing with every historical fingerprint.
    */
  private[serenity] def dictionaryCacheSize: Int = DictionaryLoader.cacheSize

  /** How many cache entries currently exist for `path`'s own normalized key -- 0 or 1, per this cache's own bounded-
    * per-path contract. Exposed alongside `dictionaryCacheSize` so a test asserting that contract for one specific
    * dictionary doesn't have to reason about the loader's total cache size, which this process-wide cache shares with
    * every other test suite exercising `loadDictionarySnapshot`/`check` concurrently in the same JVM (sbt/ ScalaTest's
    * default cross-suite parallelism) -- a size-based assertion is vulnerable to unrelated suites adding their own
    * (different-path) entries mid-test, while this stays scoped to the one path under test.
    */
  private[serenity] def dictionaryCacheEntryCount(path: Path): Int = DictionaryLoader.cacheEntryCount(path)

  /** Immutable, already-loaded dictionary data that pure analysis (`analyzeText`, `refreshDiagnostics`) consumes.
    * Building one performs no filesystem IO; only `loadDictionarySnapshot` does.
    *
    * `iconv`/`oconv` are the merged Hunspell ICONV/OCONV conversion tables (issue #1182) across every loaded
    * dictionary, applied respectively to words before dictionary lookup and to REP-based suggestions before display.
    *
    * `compoundRules`/`compoundMin`/`compoundWordFlags` (issue #1187) back Hunspell COMPOUNDRULE matching: the raw
    * `COMPOUNDRULE` pattern strings, the merged `COMPOUNDMIN` (minimum compound-member length, default 3), and every
    * dictionary word's flags keyed by its normalized text -- the same shape `HunspellCompoundMatcher.matches` needs to
    * recognize an unmatched word as a valid compound without re-reading the filesystem.
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

  private[spellcheck] val BuiltInDictionaries: Map[String, Set[String]] = Map(
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
      "café",
      "français",
      "langue",
      "monde",
      "résumé",
      "texte"
    ),
    "el" -> Set(
      "γειά",
      "κόσμος"
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
              .filterNot(match_ =>
                isAccepted(HunspellAffixParser.applyConversionTable(match_.matched, dictionary.iconv), dictionary)
              )
              .map { match_ =>
                val word          = match_.matched
                val convertedWord = HunspellAffixParser.applyConversionTable(word, dictionary.iconv)
                // OCONV (#1182): applied only to generated suggestions, matching hunspell's output-conversion
                // semantics -- the word as typed (`word`, above) is shown unconverted in the diagnostic message.
                val suggestions = dictionary.replacements
                  .getOrElse(normalizeWord(convertedWord), Nil)
                  .map(HunspellAffixParser.applyConversionTable(_, dictionary.oconv))
                Diagnostic(
                  range = LspRange(
                    LspPosition(lineIndex, match_.start),
                    LspPosition(lineIndex, match_.end)
                  ),
                  severity = Some(DiagnosticSeverity.Warning),
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
    * throughout, delegated to `DictionaryLoader`. Callers must invoke this from `IO.blocking` and thread the resulting
    * immutable snapshot into the pure analysis methods above rather than calling this (or `check`) from a state-commit
    * path.
    */
  def loadDictionarySnapshot(config: SpellCheckConfig): DictionarySnapshot =
    DictionaryLoader.loadSnapshot(config)

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

  private def isAccepted(word: String, dictionary: DictionaryContext): Boolean =
    val normalized = normalizeWord(word)
    normalized.length < 3 || dictionary.words.contains(normalized) ||
    HunspellCompoundMatcher.matches(
      normalized,
      dictionary.compoundRules,
      dictionary.compoundWordFlags,
      dictionary.compoundMin
    )

  private[spellcheck] def normalizeWord(word: String): String =
    word.toLowerCase(Locale.ROOT)

  private def isSpellCheckDiagnostic(diagnostic: Diagnostic): Boolean =
    diagnostic.source.contains(Source)
end SpellChecker
