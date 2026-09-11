package com.serenity.spellcheck

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

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
    compoundWordMax: Option[Int]
)

/** One entry per normalized dictionary path, holding only the most recently loaded version of that dictionary. A path
  * whose fingerprint no longer matches is replaced in place rather than accumulating a new entry, so repeated
  * dictionary edits cannot grow this map without bound.
  */
final private[spellcheck] case class DictionaryCacheEntry(
    fingerprints: List[SpellCheckDictionaryFingerprint],
    result: DictionaryLoadResult
)

/** The process-wide bounded dictionary cache backing `DictionaryLoader`.
  *
  * `size` and `entryCount` exist for tests asserting the bounded-per-path contract; they are the intentional
  * observation points on that contract, so tests call them here rather than through an accessor forwarded from
  * `SpellChecker`, which has nothing to do with caching.
  */
private[serenity] object DictionaryCache:

  private val Entries = ConcurrentHashMap[String, DictionaryCacheEntry]()

  /** Number of distinct dictionary paths currently cached -- the whole-cache view of the contract that this map holds
    * one entry per normalized path rather than growing with every historical fingerprint.
    */
  def size: Int = Entries.size()

  /** How many entries currently exist for `path`'s own normalized key -- 0 or 1, per the bounded-per-path contract.
    * Prefer this over `size` in tests: this map is process-wide and shared with every other suite exercising
    * `DictionaryLoader.loadSnapshot`/`SpellChecker.check` concurrently in the same JVM (sbt/ScalaTest's default
    * cross-suite parallelism), so a size-based assertion is vulnerable to unrelated suites adding their own
    * (different-path) entries mid-test, while this stays scoped to the one path under test.
    */
  def entryCount(path: Path): Int =
    if Entries.containsKey(cacheKey(path)) then 1 else 0

  /** Returns the cached load for `path` when it was produced by exactly `fingerprints`, otherwise stores and returns
    * `load()`. Keyed by normalized path so a changed fingerprint replaces the entry rather than adding one.
    */
  private[spellcheck] def getOrLoad(
    path: Path,
    fingerprints: List[SpellCheckDictionaryFingerprint],
    load: () => DictionaryLoadResult
  ): DictionaryLoadResult =
    Entries
      .compute(
        cacheKey(path),
        (_, existing) =>
          Option(existing)
            .filter(_.fingerprints == fingerprints)
            .getOrElse(DictionaryCacheEntry(fingerprints, load()))
      )
      .result

  private def cacheKey(path: Path): String =
    path.toAbsolutePath.normalize().toString

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

  def loadSnapshot(config: SpellCheckConfig): DictionarySnapshot =
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

    val compoundWordFlags = mergeCompoundWordFlags(externalResults.map(_.compoundWordFlags))
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

    val context = DictionaryContext(
      words = (externalWords ++ fallbackWords ++ normalized.additionalWords).map(DictionaryWord.normalize),
      replacements = externalReplacements,
      failures = failures.distinct,
      iconv = externalResults.flatMap(_.iconv).distinct,
      oconv = externalResults.flatMap(_.oconv).distinct,
      compoundRules = externalResults.flatMap(_.compoundRules).distinct,
      compoundMin = externalResults.map(_.compoundMin).foldLeft(3)(math.min),
      compoundWordFlags = compoundWordFlags,
      compoundFlag = compoundFlag,
      compoundBeginFlag = compoundBeginFlag,
      compoundMiddleFlag = compoundMiddleFlag,
      compoundEndFlag = compoundEndFlag,
      compoundWordMax = compoundWordMax,
      compoundFlagTrie = compoundFlagTrie
    )
    DictionarySnapshot(context, SpellCheckConfig.discoverDictionaryFingerprints(normalized))

  private def loadDictionary(path: Path): DictionaryLoadResult =
    val dependencyPaths = SpellCheckConfig.dictionaryDependencyPaths(List(path))
    val fingerprints    = dependencyPaths.map(SpellCheckDictionaryFingerprint.fromPath)
    DictionaryCache.getOrLoad(path, fingerprints, () => readDictionary(path))

  private def readDictionary(path: Path): DictionaryLoadResult =
    if !Files.exists(path) then failedLoad(s"Dictionary file does not exist: $path")
    else if Files.isDirectory(path) then failedLoad(s"Dictionary path is a directory: $path")
    else
      try
        val affixPath  = affixPathFor(path)
        val charset    = affixPath.map(readDeclaredCharset).getOrElse(DefaultDictionaryCharset)
        val affixLines = affixPath.map(readTrimmedLines(_, charset)).getOrElse(Nil)
        val affixRules =
          affixPath.map(_ => HunspellFormat.parseAffixRules(affixLines)).getOrElse(HunspellAffixRules.empty)
        val unsupportedAff = affixPath.map(HunspellFormat.unsupportedAffixDirectives(affixLines, _)).getOrElse(Nil)
        val lines          = Files.readAllLines(path, charset)
        val entries = lines.toArray.toList
          .collect { case line: String => line.trim }
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
          affixRules.compoundWordMax
        )
      catch
        case NonFatal(error) =>
          failedLoad(s"Could not load dictionary $path: ${error.getMessage}")

  private def failedLoad(failure: String): DictionaryLoadResult =
    DictionaryLoadResult(Set.empty, Map.empty, List(failure), Nil, Nil, Nil, 3, Map.empty, None, None, None, None, None)

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
