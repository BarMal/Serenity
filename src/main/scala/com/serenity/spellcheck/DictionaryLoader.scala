package com.serenity.spellcheck

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

import scala.util.control.NonFatal

import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}

/** One entry per normalized dictionary path, holding only the most recently loaded version of that dictionary. A path
  * whose fingerprint no longer matches is replaced in place rather than accumulating a new entry, so repeated
  * dictionary edits cannot grow this map without bound.
  */
final private[spellcheck] case class DictionaryCacheEntry(
    fingerprints: List[SpellCheckDictionaryFingerprint],
    result: DictionaryLoadResult
)

final private[spellcheck] case class DictionaryLoadResult(
    words: Set[String],
    replacements: Map[String, List[String]],
    failures: List[String],
    iconv: List[(String, String)],
    oconv: List[(String, String)],
    compoundRules: List[String],
    compoundMin: Int,
    compoundWordFlags: Map[String, Set[String]]
)

/** All dictionary discovery, reading, fingerprinting and caching -- explicit filesystem IO throughout. Callers must
  * invoke `loadSnapshot` from `IO.blocking` and thread the resulting immutable snapshot into `SpellChecker`'s pure
  * analysis methods rather than calling this (or `SpellChecker.check`) from a state-commit path.
  */
private[spellcheck] object DictionaryLoader:

  private val DictionaryCache                   = ConcurrentHashMap[String, DictionaryCacheEntry]()
  private val DefaultDictionaryCharset: Charset = StandardCharsets.UTF_8

  /** Number of distinct dictionary paths currently cached -- exposed only so tests can assert the cache stays bounded
    * to one entry per normalized path rather than growing with every historical fingerprint.
    */
  def cacheSize: Int = DictionaryCache.size()

  /** How many cache entries currently exist for `path`'s own normalized key -- 0 or 1, per this cache's own bounded-
    * per-path contract. Exposed alongside `cacheSize` so a test asserting that contract for one specific dictionary
    * doesn't have to reason about `DictionaryCache`'s total size, which this process-wide cache shares with every other
    * test suite exercising `loadSnapshot`/`SpellChecker.check` concurrently in the same JVM (sbt/ScalaTest's default
    * cross-suite parallelism) -- a size-based assertion is vulnerable to unrelated suites adding their own
    * (different-path) entries mid-test, while this stays scoped to the one path under test.
    */
  def cacheEntryCount(path: Path): Int =
    if DictionaryCache.containsKey(path.toAbsolutePath.normalize().toString) then 1 else 0

  def loadSnapshot(config: SpellCheckConfig): SpellChecker.DictionarySnapshot =
    val normalized      = config.normalized
    val sourcePaths     = SpellCheckConfig.discoverDictionarySourcePaths(normalized)
    val externalResults = sourcePaths.map(loadDictionary)
    val externalWords   = externalResults.flatMap(_.words).toSet
    val externalReplacements =
      mergeReplacementMaps(externalResults.map(_.replacements))
    val failures = externalResults.flatMap(_.failures)
    val fallbackWords =
      if normalized.dictionaryPaths.nonEmpty && externalWords.nonEmpty then Set.empty[String]
      else
        normalized.languages.flatMap(language => SpellChecker.BuiltInDictionaries.getOrElse(language, Set.empty)).toSet

    val context = SpellChecker.DictionaryContext(
      words = (externalWords ++ fallbackWords ++ normalized.additionalWords).map(SpellChecker.normalizeWord),
      replacements = externalReplacements,
      failures = failures.distinct,
      iconv = externalResults.flatMap(_.iconv).distinct,
      oconv = externalResults.flatMap(_.oconv).distinct,
      compoundRules = externalResults.flatMap(_.compoundRules).distinct,
      compoundMin = externalResults.map(_.compoundMin).foldLeft(3)(math.min),
      compoundWordFlags = HunspellCompoundMatcher.mergeCompoundWordFlags(externalResults.map(_.compoundWordFlags))
    )
    SpellChecker.DictionarySnapshot(context, SpellCheckConfig.discoverDictionaryFingerprints(normalized))

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
        val affixPath  = affixPathFor(path)
        val charset    = affixPath.map(readDeclaredCharset).getOrElse(DefaultDictionaryCharset)
        val affixLines = affixPath.map(readTrimmedLines(_, charset)).getOrElse(Nil)
        val affixRules = affixPath.map(_ => HunspellAffixParser.parse(affixLines)).getOrElse(HunspellAffixRules.empty)
        val unsupportedAff = affixPath.map(HunspellAffixParser.unsupportedAffixDirectives(affixLines, _)).getOrElse(Nil)
        val lines          = Files.readAllLines(path, charset)
        val entries = lines.toArray.toList
          .collect { case line: String => line.trim }
          .dropWhile(line => line.forall(_.isDigit))
          .filter(line => line.nonEmpty && !line.startsWith("#"))
          .flatMap(line => HunspellWordExpander.parseEntry(line, affixRules))

        val words = entries
          .flatMap(entry => HunspellWordExpander.expand(entry, affixRules))
          .map(SpellChecker.normalizeWord)
          .toSet

        // COMPOUNDRULE (#1187) matches compound candidates against dictionary entries' own flags, keyed by their
        // normalized text -- computed only when the affix file actually declares compounding, since it is otherwise
        // unused.
        val compoundWordFlags =
          if affixRules.compoundRules.isEmpty then Map.empty[String, Set[String]]
          else entries.groupMapReduce(entry => SpellChecker.normalizeWord(entry.word))(_.flags)(_ ++ _)

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

  private def mergeReplacementMaps(maps: List[Map[String, List[String]]]): Map[String, List[String]] =
    maps.foldLeft(Map.empty[String, List[String]]) { (merged, replacements) =>
      replacements.foldLeft(merged) {
        case (acc, (source, suggestions)) =>
          acc.updated(source, (acc.getOrElse(source, Nil) ++ suggestions).distinct)
      }
    }
