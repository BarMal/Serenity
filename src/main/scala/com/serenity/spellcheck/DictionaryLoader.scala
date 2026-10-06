package com.serenity.spellcheck

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal

import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}

/** What one dictionary file (plus its sibling `.aff`) contributed, before merging with the other configured
  * dictionaries into a `DictionaryContext`.
  */
final private[spellcheck] case class DictionaryLoadResult(
    words: Set[String],
    replacements: Map[String, List[String]],
    failures: List[String],
    iconv: List[(String, String)],
    oconv: List[(String, String)],
    compoundRules: List[String],
    compoundMin: Int,
    compoundWordFlags: Map[String, Set[String]],
    compoundFlag: Option[String],
    compoundBeginFlag: Option[String],
    compoundMiddleFlag: Option[String],
    compoundEndFlag: Option[String],
    compoundWordMax: Option[Int],
    compoundCheckRules: CompoundCheckRules
)

/** One entry per normalized dictionary path (or bundled dictionary), holding only the most recently loaded version of
  * that dictionary. A key whose fingerprint no longer matches is replaced in place rather than accumulating a new
  * entry, so repeated dictionary edits cannot grow this map without bound.
  */
final private[spellcheck] case class DictionaryCacheEntry(
    fingerprints: List[SpellCheckDictionaryFingerprint],
    result: DictionaryLoadResult
)

/** Everything a merged [[DictionarySnapshot]] is derived from. Merging unions every loaded word, which for a full
  * dictionary is far too costly to repeat on each document analysis, so the latest merge is kept while this is
  * unchanged.
  */
final private[spellcheck] case class DictionaryMergeInputs(
    config: SpellCheckConfig,
    fingerprints: List[SpellCheckDictionaryFingerprint],
    bundled: List[String]
)

/** The bounded dictionary cache backing `DictionaryLoader`.
  *
  * `size` and `entryCount` exist for tests asserting the bounded-per-path contract; they are the intentional
  * observation points on that contract, so tests call them here rather than through an accessor forwarded from
  * `SpellChecker`, which has nothing to do with caching.
  *
  * Instance-scoped (issue #1677): owned by the analysis service that calls [[DictionaryLoader.loadSnapshot]]
  * (`StateManagerOperationBoundary`, one per `StateManager`) rather than living as a JVM-wide singleton `object`, so
  * two independently constructed instances share no cache state. [[retainOnly]] is `loadSnapshot`'s eviction hook: a
  * dictionary path removed from config no longer lingers here forever (issue #860), since every load re-derives the
  * currently configured paths and prunes anything else.
  */
final class DictionaryCache:

  private val entries = ConcurrentHashMap[String, DictionaryCacheEntry]()
  private val merged  = AtomicReference[Option[(DictionaryMergeInputs, DictionarySnapshot)]](None)

  /** Number of distinct dictionary paths currently cached -- the whole-cache view of the contract that this map holds
    * one entry per normalized path rather than growing with every historical fingerprint.
    */
  def size: Int = entries.size()

  /** How many entries currently exist for `path`'s own normalized key -- 0 or 1, per the bounded-per-path contract. */
  def entryCount(path: Path): Int =
    if entries.containsKey(DictionaryCache.keyOf(path)) then 1 else 0

  /** Returns the cached load for `key` when it was produced by exactly `fingerprints`, otherwise stores and returns
    * `load()`. Keyed by [[DictionaryCache.keyOf]] so a changed fingerprint replaces the entry rather than adding one.
    */
  private[spellcheck] def getOrLoad(
    key: String,
    fingerprints: List[SpellCheckDictionaryFingerprint],
    load: () => DictionaryLoadResult
  ): DictionaryLoadResult =
    entries
      .compute(
        key,
        (_, existing) =>
          Option(existing)
            .filter(_.fingerprints == fingerprints)
            .getOrElse(DictionaryCacheEntry(fingerprints, load()))
      )
      .result

  /** Drops every cached entry whose key is not in `activeKeys` (issue #860): a dictionary removed from config must not
    * linger here forever, unboundedly.
    */
  private[spellcheck] def retainOnly(activeKeys: Set[String]): Unit =
    val _ = entries.keySet().removeIf(key => !activeKeys.contains(key))

  /** The merged snapshot for `inputs` when the latest merge was built from exactly them, otherwise `build()`. */
  private[spellcheck] def mergedFor(
    inputs: DictionaryMergeInputs,
    build: () => DictionarySnapshot
  ): DictionarySnapshot =
    merged
      .get()
      .collect { case (previous, snapshot) if previous == inputs => snapshot }
      .getOrElse {
        val snapshot = build()
        merged.set(Some(inputs -> snapshot))
        snapshot
      }

object DictionaryCache:
  def apply(): DictionaryCache = new DictionaryCache

  private[spellcheck] def keyOf(path: Path): String =
    path.toAbsolutePath.normalize().toString

  private[spellcheck] def keyOf(dictionary: BundledDictionary): String =
    s"bundled:${dictionary.name}"

/** All dictionary discovery, reading, fingerprinting and caching -- explicit filesystem IO throughout, and the only
  * producer of `DictionaryContext`/`DictionarySnapshot`. Callers must invoke `loadSnapshot` from `IO.blocking` and
  * thread the resulting immutable snapshot into `SpellChecker`'s pure analysis methods rather than calling this (or
  * `SpellChecker.check`) from a state-commit path.
  */
object DictionaryLoader:

  private val DefaultDictionaryCharset: Charset = StandardCharsets.UTF_8

  /** The fallback word lists used when no external dictionary is configured, or when every configured one failed to
    * contribute a word -- enough vocabulary to keep spell-check useful out of the box without shipping a dictionary.
    */
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

  /** `osDictionaryDirectories` is a parameter, as in `SpellCheckConfig.discoverDictionarySourcePaths`, so tests can
    * keep an installed system dictionary out of the result.
    *
    * A bundled dictionary is parsed only while spell check is enabled and one of its languages is configured, and only
    * when the user supplied no dictionary for that language.
    */
  def loadSnapshot(
    config: SpellCheckConfig,
    cache: DictionaryCache,
    osDictionaryDirectories: List[String] = SpellCheckConfig.defaultOsDictionaryDirectories()
  ): DictionarySnapshot =
    val normalized  = config.normalized
    val sourcePaths = SpellCheckConfig.discoverDictionarySourcePaths(normalized, osDictionaryDirectories)
    val bundled = if normalized.enabled then BundledDictionary.defaultsFor(normalized.languages, sourcePaths) else Nil
    // #860: a dictionary that left the config must not linger in the cache forever -- every load re-derives the
    // currently configured dictionaries and prunes anything else before (re)loading them.
    cache.retainOnly(sourcePaths.map(DictionaryCache.keyOf).toSet ++ bundled.map(DictionaryCache.keyOf))
    val fingerprints =
      SpellCheckConfig.dictionaryDependencyPaths(sourcePaths).map(SpellCheckDictionaryFingerprint.fromPath)
    cache.mergedFor(
      DictionaryMergeInputs(normalized, fingerprints, bundled.map(_.name)),
      () =>
        mergeSnapshot(
          normalized,
          sourcePaths.map(loadDictionary(_, cache)) ++ bundled.map(loadBundled(_, cache)),
          fingerprints
        )
    )

  private def mergeSnapshot(
    normalized: SpellCheckConfig,
    externalResults: List[DictionaryLoadResult],
    fingerprints: List[SpellCheckDictionaryFingerprint]
  ): DictionarySnapshot =
    val externalWords = externalResults.flatMap(_.words).toSet
    val externalReplacements =
      mergeReplacementMaps(externalResults.map(_.replacements))
    val failures = externalResults.flatMap(_.failures)
    val fallbackWords =
      if normalized.dictionaryPaths.nonEmpty && externalWords.nonEmpty then Set.empty[String]
      else normalized.languages.flatMap(language => BuiltInDictionaries.getOrElse(language, Set.empty)).toSet

    val compoundWordFlags = mergeCompoundWordFlags(externalResults.map(_.compoundWordFlags))
    val compoundRules     = externalResults.flatMap(_.compoundRules).distinct
    // Built once here, not per `HunspellCompoundMatcher.matches` call (issue #1445) -- mirroring `compoundFlagTrie`
    // below for the free-form COMPOUNDFLAG path. Skipped (kept empty) when no dictionary declares COMPOUNDRULE, since
    // `matches` short-circuits on an empty `compoundRules` before ever consulting the index.
    val compoundCandidateIndex =
      if compoundRules.isEmpty then CompoundCandidateIndex.empty else CompoundCandidateIndex.build(compoundWordFlags)
    // First-declared wins (issue #1198): unlike COMPOUNDMIN/COMPOUNDRULE, these flag letters are meaningful only
    // relative to the one dictionary that declared them (its own FLAG-mode alphabet), so merging across multiple
    // dictionaries that each declare their own free-form compounding is not well-defined in general -- the common
    // case (and the one real .aff/.dic pairs surveyed in #1198's investigation) is a single dictionary source per
    // configured language.
    val compoundFlag       = externalResults.flatMap(_.compoundFlag).headOption
    val compoundBeginFlag  = externalResults.flatMap(_.compoundBeginFlag).headOption
    val compoundMiddleFlag = externalResults.flatMap(_.compoundMiddleFlag).headOption
    val compoundEndFlag    = externalResults.flatMap(_.compoundEndFlag).headOption
    val compoundWordMax = externalResults.flatMap(_.compoundWordMax) match
      case Nil          => None
      case head :: tail => Some(tail.foldLeft(head)(math.min))
    val compoundRoleFlags = List(compoundFlag, compoundBeginFlag, compoundMiddleFlag, compoundEndFlag).flatten.toSet
    val compoundFlagTrie =
      if compoundRoleFlags.isEmpty then CompoundTrie.empty
      else CompoundTrie.build(compoundWordFlags.filter { case (_, flags) => flags.exists(compoundRoleFlags.contains) })
    val compoundCheckRules = mergeCompoundCheckRules(externalResults.map(_.compoundCheckRules))

    val context = DictionaryContext(
      words = (externalWords ++ fallbackWords ++ normalized.additionalWords).map(DictionaryWord.normalize),
      replacements = externalReplacements,
      failures = failures.distinct,
      iconv = externalResults.flatMap(_.iconv).distinct,
      oconv = externalResults.flatMap(_.oconv).distinct,
      compoundRules = compoundRules,
      compoundMin = externalResults.map(_.compoundMin).foldLeft(3)(math.min),
      compoundWordFlags = compoundWordFlags,
      compoundCandidateIndex = compoundCandidateIndex,
      compoundFlag = compoundFlag,
      compoundBeginFlag = compoundBeginFlag,
      compoundMiddleFlag = compoundMiddleFlag,
      compoundEndFlag = compoundEndFlag,
      compoundWordMax = compoundWordMax,
      compoundFlagTrie = compoundFlagTrie,
      compoundCheckRules = compoundCheckRules
    )
    DictionarySnapshot(context, fingerprints)

  private def loadDictionary(path: Path, cache: DictionaryCache): DictionaryLoadResult =
    val dependencyPaths = SpellCheckConfig.dictionaryDependencyPaths(List(path))
    val fingerprints    = dependencyPaths.map(SpellCheckDictionaryFingerprint.fromPath)
    cache.getOrLoad(DictionaryCache.keyOf(path), fingerprints, () => readDictionary(path))

  private def loadBundled(dictionary: BundledDictionary, cache: DictionaryCache): DictionaryLoadResult =
    cache.getOrLoad(DictionaryCache.keyOf(dictionary), List(dictionary.fingerprint), () => readBundled(dictionary))

  private def readBundled(dictionary: BundledDictionary): DictionaryLoadResult =
    try
      val loaded =
        for
          affixLines      <- dictionary.affixLines
          dictionaryLines <- dictionary.dictionaryLines
        yield
          val unsupported = HunspellFormat.unsupportedAffixDirectives(
            affixLines,
            s"the bundled ${dictionary.name} dictionary",
            dictionary.inertDirectives
          )
          parseDictionary(Some(affixLines), dictionaryLines, unsupported)
      loaded.fold(failedLoad, identity)
    catch
      case NonFatal(error) =>
        failedLoad(s"Could not load the bundled ${dictionary.name} dictionary: ${error.getMessage}")

  private def readDictionary(path: Path): DictionaryLoadResult =
    if !Files.exists(path) then failedLoad(s"Dictionary file does not exist: $path")
    else if Files.isDirectory(path) then failedLoad(s"Dictionary path is a directory: $path")
    else
      try
        val affixPath = affixPathFor(path)
        val charset   = affixPath.map(readDeclaredCharset).getOrElse(DefaultDictionaryCharset)
        val affix     = affixPath.map(path => path -> readTrimmedLines(path, charset))
        val unsupportedAff =
          affix.map((affixFile, lines) => HunspellFormat.unsupportedAffixDirectives(lines, affixFile.toString))
        parseDictionary(affix.map(_._2), readTrimmedLines(path, charset), unsupportedAff.getOrElse(Nil))
      catch
        case NonFatal(error) =>
          failedLoad(s"Could not load dictionary $path: ${error.getMessage}")

  private def parseDictionary(
    affixLines: Option[List[String]],
    dictionaryLines: List[String],
    unsupportedAff: List[String]
  ): DictionaryLoadResult =
    val affixRules = affixLines.map(HunspellFormat.parseAffixRules).getOrElse(HunspellAffixRules.empty)
    val entries = dictionaryLines
      .dropWhile(line => line.forall(_.isDigit))
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .flatMap(line => HunspellFormat.parseEntry(line, affixRules))

    val words = entries
      .flatMap(entry => HunspellFormat.expand(entry, affixRules))
      .map(DictionaryWord.normalize)
      .toSet

    // COMPOUNDRULE (#1187) and free-form COMPOUNDFLAG (#1198) both match compound candidates against dictionary
    // entries' own flags, keyed by their normalized text -- computed only when the affix file actually declares
    // one of these compounding mechanisms, since it is otherwise unused.
    val declaresFreeFormCompounding =
      affixRules.compoundFlag.isDefined || affixRules.compoundBeginFlag.isDefined ||
        affixRules.compoundMiddleFlag.isDefined || affixRules.compoundEndFlag.isDefined
    val compoundWordFlags =
      if affixRules.compoundRules.isEmpty && !declaresFreeFormCompounding then Map.empty[String, Set[String]]
      else entries.groupMapReduce(entry => DictionaryWord.normalize(entry.word))(_.flags)(_ ++ _)

    DictionaryLoadResult(
      words,
      affixRules.replacements,
      unsupportedAff,
      affixRules.iconv,
      affixRules.oconv,
      affixRules.compoundRules,
      affixRules.compoundMin,
      compoundWordFlags,
      affixRules.compoundFlag,
      affixRules.compoundBeginFlag,
      affixRules.compoundMiddleFlag,
      affixRules.compoundEndFlag,
      affixRules.compoundWordMax,
      affixRules.compoundCheckRules
    )

  private def failedLoad(failure: String): DictionaryLoadResult =
    DictionaryLoadResult(
      Set.empty,
      Map.empty,
      List(failure),
      Nil,
      Nil,
      Nil,
      3,
      Map.empty,
      None,
      None,
      None,
      None,
      None,
      CompoundCheckRules.empty
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

  private def mergeReplacementMaps(maps: List[Map[String, List[String]]]): Map[String, List[String]] =
    maps.foldLeft(Map.empty[String, List[String]]) { (merged, replacements) =>
      replacements.foldLeft(merged) {
        case (acc, (source, suggestions)) =>
          acc.updated(source, (acc.getOrElse(source, Nil) ++ suggestions).distinct)
      }
    }

  private def mergeCompoundWordFlags(maps: List[Map[String, Set[String]]]): Map[String, Set[String]] =
    maps.foldLeft(Map.empty[String, Set[String]]) { (merged, wordFlags) =>
      wordFlags.foldLeft(merged) {
        case (acc, (word, flags)) => acc.updated(word, acc.getOrElse(word, Set.empty) ++ flags)
      }
    }

  // Booleans OR'd (any configured dictionary enabling a CHECKCOMPOUND* check applies it) and CHECKCOMPOUNDPATTERN
  // rules concatenated, matching how compoundRules/iconv/oconv are already merged above.
  private def mergeCompoundCheckRules(rules: List[CompoundCheckRules]): CompoundCheckRules =
    rules.foldLeft(CompoundCheckRules.empty) { (merged, rule) =>
      CompoundCheckRules(
        checkCase = merged.checkCase || rule.checkCase,
        checkDup = merged.checkDup || rule.checkDup,
        checkRep = merged.checkRep || rule.checkRep,
        checkTriple = merged.checkTriple || rule.checkTriple,
        simplifiedTriple = merged.simplifiedTriple || rule.simplifiedTriple,
        patterns = merged.patterns ++ rule.patterns
      )
    }
