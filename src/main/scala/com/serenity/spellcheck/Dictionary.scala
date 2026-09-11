package com.serenity.spellcheck

import java.util.Locale

import com.serenity.config.SpellCheckDictionaryFingerprint

/** Immutable, already-loaded dictionary data that pure analysis (`SpellChecker.analyzeText`,
  * `SpellChecker.refreshDiagnostics`) consumes. Building one performs no filesystem IO; only `DictionaryLoader` does.
  *
  * This is the contract between the two halves of the package: `DictionaryLoader` is the only producer, `SpellChecker`
  * the only consumer. It lives here rather than nested in either so neither has to reach into the other to name it.
  *
  * `iconv`/`oconv` are the merged Hunspell ICONV/OCONV conversion tables (issue #1182) across every loaded dictionary,
  * applied respectively to words before dictionary lookup and to REP-based suggestions before display.
  *
  * `compoundRules`/`compoundMin`/`compoundWordFlags` (issue #1187) back Hunspell COMPOUNDRULE matching: the raw
  * `COMPOUNDRULE` pattern strings, the merged `COMPOUNDMIN` (minimum compound-member length, default 3), and every
  * dictionary word's flags keyed by its `DictionaryWord.normalize`d text -- the shape `HunspellCompoundMatcher.matches`
  * needs to recognize an unmatched word as a valid compound without re-reading the filesystem.
  *
  * `compoundFlag`/`compoundBeginFlag`/`compoundMiddleFlag`/`compoundEndFlag`/`compoundWordMax`/`compoundFlagTrie`
  * (issue #1198) back Hunspell's free-form COMPOUNDFLAG compounding: the (first-declared, across merged dictionaries)
  * flag letters marking a word eligible as a general or positionally-restricted compound member, the merged
  * `COMPOUNDWORDMAX` (`None` means hunspell's documented default of unlimited), and a trie over every word carrying any
  * of those flags -- built once here by `DictionaryLoader.loadSnapshot` so `HunspellFreeCompoundMatcher.matches` never
  * rebuilds it per check. Empty/`None` on every dictionary that declares no free-form compounding, in which case
  * `HunspellFreeCompoundMatcher.matches` returns `false` immediately.
  *
  * `compoundCheckRules` (issue #1198, PR 2 of 2) merges the `CHECKCOMPOUND*`/`SIMPLIFIEDTRIPLE`/`CHECKCOMPOUNDPATTERN`
  * boundary-validation directives across every loaded dictionary (booleans OR'd, patterns concatenated) --
  * `HunspellFreeCompoundMatcher.matches` applies them to each adjacent pair of members in a segmentation it finds,
  * using `words`/`replacements`/`compoundWordFlags` above for the standalone-word and pattern-reconstruction lookups
  * `CHECKCOMPOUNDREP`/`CHECKCOMPOUNDPATTERN` need.
  */
final case class DictionaryContext(
    words: Set[String],
    replacements: Map[String, List[String]],
    failures: List[String],
    iconv: List[(String, String)] = Nil,
    oconv: List[(String, String)] = Nil,
    compoundRules: List[String] = Nil,
    compoundMin: Int = 3,
    compoundWordFlags: Map[String, Set[String]] = Map.empty,
    compoundFlag: Option[String] = None,
    compoundBeginFlag: Option[String] = None,
    compoundMiddleFlag: Option[String] = None,
    compoundEndFlag: Option[String] = None,
    compoundWordMax: Option[Int] = None,
    compoundFlagTrie: CompoundTrie = CompoundTrie.empty,
    compoundCheckRules: CompoundCheckRules = CompoundCheckRules.empty
)

/** The result of one explicit dictionary-discovery pass: the loaded words/replacements/failures plus the on-disk
  * fingerprints that produced them. Obtain one via `DictionaryLoader.loadSnapshot`, called from `IO.blocking`, then
  * thread it into `SpellChecker.refreshDiagnostics`/`analysisFingerprints`/`applyIfCurrent` so those stay pure.
  */
final case class DictionarySnapshot(
    context: DictionaryContext,
    fingerprints: List[SpellCheckDictionaryFingerprint]
)

/** The canonical form every word takes before it is used as a dictionary key.
  *
  * Every producer and consumer of `DictionaryContext` must agree on this single function or lookups silently miss: it
  * keys the accepted-word set, REP replacement sources, and COMPOUNDRULE member flags on the loading side, and the
  * checked word on the analysis side.
  */
object DictionaryWord:

  def normalize(word: String): String =
    word.toLowerCase(Locale.ROOT)
