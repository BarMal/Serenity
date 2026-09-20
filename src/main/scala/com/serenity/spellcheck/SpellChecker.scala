package com.serenity.spellcheck

import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}
import com.serenity.lsp.model.*
import com.serenity.state.models.*

object SpellChecker:

  val Source: String = "spell-check"

  // A hyphen/apostrophe-joined segment after the first may also carry digits (#1528): a numeric compound like
  // "COVID-19" would otherwise only match its "COVID" prefix -- the "-19" suffix cannot extend a purely-letter
  // token -- leaving the orphaned "COVID" fragment to fail the dictionary lookup on its own. The leading segment stays
  // letters-only so a bare number is never tokenized as a word by itself.
  private val WordPattern = """[\p{L}\p{M}]+(?:['’-][\p{L}\p{M}\p{N}]+)*""".r

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
                isAccepted(HunspellFormat.applyConversionTable(match_.matched, dictionary.iconv), dictionary) ||
                  isExemptFromCasing(match_.matched, isSentenceInitial(line, match_.start))
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

  /** The word covered by an active spell-check diagnostic at the cursor (#1531), read back from the buffer's current
    * content at the diagnostic's own range rather than reparsed from the diagnostic message. `None` when there is no
    * active buffer/cursor, or the cursor sits outside every spell-check diagnostic's range for that buffer.
    */
  def flaggedWordAtCursor(state: AppState): Option[String] =
    for
      buffer <- state.activeBuffer
      cursor <- state.activeCursorPosition
      diagnostic <- state.runtime.diagnosticsState.diagnostics
        .getOrElse(diagnosticsUri(buffer), Nil)
        .find(diagnostic => diagnostic.source.contains(Source) && containsCursor(diagnostic.range, cursor))
      line <- buffer.document.content.getLine(diagnostic.range.start.line)
      word <- wordInRange(line, diagnostic.range)
    yield word

  private def containsCursor(range: LspRange, cursor: CursorPosition): Boolean =
    cursor.line == range.start.line && cursor.column >= range.start.character &&
      cursor.column <= range.end.character

  private def wordInRange(line: String, range: LspRange): Option[String] =
    val start = range.start.character.max(0).min(line.length)
    val end   = range.end.character.max(start).min(line.length)
    Option.when(end > start)(line.substring(start, end))

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

  /** Whether `word` should be skipped regardless of dictionary membership (#1528):
    *   - it carries a digit at all (a numeric compound like "COVID-19" or an alphanumeric identifier like "MP3" --
    *     hunspell dictionaries have no notion of digits, so no spelling of one is ever "in the dictionary")
    *   - it is a capitalized word (not an all-caps acronym) that is not the first word of its sentence -- far more
    *     often a proper noun than a genuine misspelling, and dictionaries cannot enumerate every proper noun
    *   - it is an all-caps acronym, regardless of sentence position
    *
    * Caution, here be imagine dragons: this is a heuristic, not a certainty. A genuinely misspelled proper noun in a
    * non-sentence-initial, capitalized position is indistinguishable from a correctly-spelled one this dictionary has
    * simply never seen, so it is silently accepted (a false negative) rather than flagged -- see `SpellCheckerSpec`'s
    * "known limitation" test.
    */
  private def isExemptFromCasing(word: String, sentenceInitial: Boolean): Boolean =
    word.exists(_.isDigit) || isAllCapsAcronym(word) || isCapitalizedNotSentenceInitial(word, sentenceInitial)

  private def isAllCapsAcronym(word: String): Boolean =
    word.length >= 2 && word.exists(_.isUpper) && !word.exists(_.isLower)

  private def isCapitalizedNotSentenceInitial(word: String, sentenceInitial: Boolean): Boolean =
    !sentenceInitial && word.headOption.exists(_.isUpper) && word.exists(_.isLower)

  /** Whether the token starting at `matchStart` in `line` is the first word of its sentence: either nothing but
    * whitespace precedes it, or the nearest non-whitespace character before it is a sentence-ending mark.
    */
  private def isSentenceInitial(line: String, matchStart: Int): Boolean =
    val precedingNonBlank = line.substring(0, matchStart).reverse.dropWhile(_.isWhitespace)
    precedingNonBlank.isEmpty || precedingNonBlank.headOption.exists(SentenceTerminators.contains)

  private val SentenceTerminators = Set('.', '!', '?')

  private def isSpellCheckDiagnostic(diagnostic: Diagnostic): Boolean =
    diagnostic.source.contains(Source)
end SpellChecker
