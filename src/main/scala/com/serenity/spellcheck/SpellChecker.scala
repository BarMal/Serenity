package com.serenity.spellcheck

import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}
import com.serenity.lsp.model.*
import com.serenity.state.models.*

object SpellChecker:

  val Source: String = "spell-check"

  private val WordPattern = """[\p{L}\p{M}]+(?:['’-][\p{L}\p{M}]+)*""".r

  /** Convenience entry point that discovers and loads dictionaries itself -- handy for tests and one-off checks, but it
    * performs filesystem IO synchronously and so must never be called from a pure state method or from inside
    * `Ref.update`. Production analysis instead calls `DictionaryLoader.loadSnapshot` explicitly from `IO.blocking` and
    * passes the resulting `DictionaryContext` into the pure `analyzeText`.
    */
  def check(text: String, config: SpellCheckConfig): List[Diagnostic] =
    analyzeText(text, config, DictionaryLoader.loadSnapshot(config).context)

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
                isAccepted(HunspellFormat.applyConversionTable(match_.matched, dictionary.iconv), dictionary)
              )
              .map { match_ =>
                val word          = match_.matched
                val convertedWord = HunspellFormat.applyConversionTable(word, dictionary.iconv)
                // OCONV (#1182): applied only to generated suggestions, matching hunspell's output-conversion
                // semantics -- the word as typed (`word`, above) is shown unconverted in the diagnostic message.
                val suggestions = dictionary.replacements
                  .getOrElse(DictionaryWord.normalize(convertedWord), Nil)
                  .map(HunspellFormat.applyConversionTable(_, dictionary.oconv))
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
    * once via `DictionaryLoader.loadSnapshot` inside `IO.blocking`, then pass the same immutable value here -- this
    * method itself never touches the filesystem, so it is safe to call from inside `Ref.update`.
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
    * `DictionaryLoader.loadSnapshot`, both `IO.blocking`) and passed in -- this method never reads the filesystem, so
    * it is safe to call from inside `Ref.update` when comparing against a state commit's expected fingerprints.
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
    val normalized = DictionaryWord.normalize(word)
    normalized.length < 3 || dictionary.words.contains(normalized) ||
    HunspellCompoundMatcher.matches(
      normalized,
      dictionary.compoundRules,
      dictionary.compoundCandidateIndex,
      dictionary.compoundMin
    ) ||
    // Free-form COMPOUNDFLAG compounding (#1198) is a second, independent mechanism a dictionary may declare
    // alongside COMPOUNDRULE (real Croatian/Persian .aff files do this) -- tried as a sibling ('||'-shaped) so either
    // mechanism accepting the word is sufficient, and neither masks the other's rejection.
    HunspellFreeCompoundMatcher.matches(
      normalized,
      dictionary.compoundFlagTrie,
      HunspellFreeCompoundMatcher.CompoundFlags(
        dictionary.compoundFlag,
        dictionary.compoundBeginFlag,
        dictionary.compoundMiddleFlag,
        dictionary.compoundEndFlag
      ),
      dictionary.compoundMin,
      dictionary.compoundWordMax,
      // CHECKCOMPOUND*/SIMPLIFIEDTRIPLE/CHECKCOMPOUNDPATTERN (#1198, PR 2 of 2): a post-hoc filter over the
      // segmentation(s) the DP above finds, backed by the same word/flag/REP data `dictionary.words`,
      // `dictionary.compoundWordFlags` and `dictionary.replacements` already carry for standalone lookup.
      dictionary.compoundCheckRules,
      dictionary.compoundWordFlags,
      dictionary.words,
      dictionary.replacements,
      // CHECKCOMPOUNDCASE needs the word as typed, not `normalized`'s case-folded form used for the trie walk.
      originalWord = word
    )

  private def isSpellCheckDiagnostic(diagnostic: Diagnostic): Boolean =
    diagnostic.source.contains(Source)
end SpellChecker
