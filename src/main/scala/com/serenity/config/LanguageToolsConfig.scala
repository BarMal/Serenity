package com.serenity.config

import java.nio.file.{Files, Path, Paths}

import scala.util.control.NonFatal

import com.serenity.lsp.config.LspUserConfig

final case class SpellCheckDictionaryFingerprint(
    path: String,
    exists: Boolean,
    isDirectory: Boolean,
    size: Long,
    lastModifiedMillis: Long
)

object SpellCheckDictionaryFingerprint:

  def fromPath(path: Path): SpellCheckDictionaryFingerprint =
    try
      val exists      = Files.exists(path)
      val isDirectory = exists && Files.isDirectory(path)
      SpellCheckDictionaryFingerprint(
        path = path.toAbsolutePath.normalize().toString,
        exists = exists,
        isDirectory = isDirectory,
        size = if exists && !isDirectory then Files.size(path) else 0L,
        lastModifiedMillis = if exists then Files.getLastModifiedTime(path).toMillis else 0L
      )
    catch
      case NonFatal(_) =>
        SpellCheckDictionaryFingerprint(
          path = path.toString,
          exists = false,
          isDirectory = false,
          size = 0L,
          lastModifiedMillis = 0L
        )

final case class SpellCheckConfig(
    enabled: Boolean = false,
    languages: List[String] = List("en"),
    dictionaryPaths: List[String] = Nil,
    additionalWords: List[String] = Nil
):

  def normalized: SpellCheckConfig =
    copy(
      languages = languages.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct,
      dictionaryPaths = dictionaryPaths.map(_.trim).filter(_.nonEmpty).distinct,
      additionalWords = additionalWords.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct
    )

object SpellCheckConfig:

  /** Resolves the on-disk dictionary paths implied by `config`, expanding directories into per-language candidates.
    * This walks the filesystem (`Files.isDirectory`/`Files.exists`) and must only be called from an explicit
    * `IO.blocking` boundary -- never from a method that otherwise looks pure.
    *
    * When `config.dictionaryPaths` is empty -- the zero-config case -- `osDictionaryDirectories` (standard
    * Hunspell/MySpell install locations, `defaultOsDictionaryDirectories()` by default) are searched for a
    * language-matching dictionary instead, so a system that already has e.g. `hunspell-en-gb` installed spell-checks
    * without any configuration (#1175). A dictionary the user *did* configure is never second-guessed this way: as soon
    * as `dictionaryPaths` is non-empty, `osDictionaryDirectories` is not consulted at all, so existing explicit
    * configuration resolves exactly as it always has. `osDictionaryDirectories` is a parameter (rather than always
    * reading `defaultOsDictionaryDirectories()` internally) so tests can point it at a directory they control instead
    * of depending on what is actually installed on the machine running the suite.
    */
  def discoverDictionarySourcePaths(
    config: SpellCheckConfig,
    osDictionaryDirectories: List[String] = defaultOsDictionaryDirectories()
  ): List[Path] =
    val normalized = config.normalized
    if normalized.dictionaryPaths.nonEmpty then
      normalized.dictionaryPaths.flatMap(path => expandDictionaryPath(path, normalized.languages)).distinct
    else osDictionaryDirectories.flatMap(directory => osDictionaryCandidates(directory, normalized.languages)).distinct

  /** Standard Hunspell/MySpell dictionary install directories for the running OS, most-specific first. Best-effort:
    * Linux distributions are consistent about `/usr/share/hunspell` and `/usr/share/myspell/dicts`, but there is no
    * single standard location on macOS or Windows -- `/Library/Spelling` and a user's own `~/Library/Spelling` are what
    * macOS's built-in spell-check panel itself uses, and `%PROGRAMDATA%\hunspell` mirrors where LibreOffice installs
    * its bundled dictionaries on Windows. `osName`/`userHome`/`programData` default to reading the running JVM's own
    * properties/environment, and are parameters only so callers (tests, primarily) can supply a specific platform
    * without needing to run on it.
    */
  def defaultOsDictionaryDirectories(
    osName: String = System.getProperty("os.name", ""),
    userHome: String = System.getProperty("user.home", ""),
    programData: Option[String] = Option(System.getenv("PROGRAMDATA"))
  ): List[String] =
    val normalizedOsName = osName.toLowerCase
    if normalizedOsName.contains("win") then programData.map(dir => s"$dir\\hunspell").toList
    else if normalizedOsName.contains("mac") then
      List("/Library/Spelling", s"$userHome/Library/Spelling", "/usr/share/hunspell", "/usr/local/share/hunspell")
    else
      List(
        "/usr/share/hunspell",
        "/usr/share/myspell/dicts",
        "/usr/local/share/hunspell",
        "/usr/local/share/myspell/dicts"
      )

  /** The language-matching dictionary files that actually exist directly inside `directory`, or `Nil` when `directory`
    * itself does not exist or none of `languages`' candidate filenames are present in it. Unlike a user-configured
    * directory (`expandDictionaryPath`, below), a missing or empty OS directory contributes no candidate at all --
    * there is nothing to fail loading later, since it was never something the user asked for.
    */
  private def osDictionaryCandidates(directory: String, languages: List[String]): List[Path] =
    pathOption(directory)
      .filter(path => Files.isDirectory(path))
      .toList
      .flatMap(dir => languages.flatMap(languageCandidates).map(dir.resolve).filter(path => Files.exists(path)))

  /** Discovers dictionary source paths and fingerprints their current on-disk state. Filesystem IO throughout -- call
    * only from `IO.blocking`. The resulting immutable list is what pure analysis and state-commit comparisons must be
    * given, rather than recomputing this themselves.
    */
  def discoverDictionaryFingerprints(config: SpellCheckConfig): List[SpellCheckDictionaryFingerprint] =
    dictionaryDependencyPaths(discoverDictionarySourcePaths(config)).map(SpellCheckDictionaryFingerprint.fromPath)

  def dictionaryDependencyPaths(dictionarySourcePaths: List[Path]): List[Path] =
    dictionarySourcePaths.flatMap(path => path :: affixPathForDictionary(path).toList).distinct

  def affixPathForDictionary(path: Path): Option[Path] =
    val fileName = path.getFileName
    Option(fileName).flatMap { name =>
      val text = name.toString
      if text.toLowerCase.endsWith(".dic") then Some(path.resolveSibling(text.dropRight(4) + ".aff"))
      else None
    }

  private def expandDictionaryPath(path: String, languages: List[String]): List[Path] =
    pathOption(path)
      .map { sourcePath =>
        if Files.isDirectory(sourcePath) then
          val candidates = languages.flatMap(languageCandidates).map(sourcePath.resolve)
          val existing   = candidates.filter(path => Files.exists(path))
          if existing.nonEmpty then existing else List(sourcePath)
        else
          val normalizedPath = sourcePath.toString
          if normalizedPath.toLowerCase.endsWith(".aff") then
            List(sourcePath.resolveSibling(sourcePath.getFileName.toString.dropRight(4) + ".dic"))
          else List(sourcePath)
      }
      .getOrElse(Nil)

  private def languageCandidates(language: String): List[String] =
    val normalized = language.trim.toLowerCase
    List(
      normalized,
      normalized.replace("-", "_"),
      normalized.replace("_", "-")
    ).distinct.map(_ + ".dic")

  private def pathOption(path: String): Option[Path] =
    try Some(Paths.get(path))
    catch case NonFatal(_) => None

final case class LanguageToolsConfig(
    syntaxHighlightingEnabled: Boolean = false,
    lspUserConfig: LspUserConfig = LspUserConfig.empty,
    spellCheck: SpellCheckConfig = SpellCheckConfig()
):

  def normalized: LanguageToolsConfig =
    copy(spellCheck = spellCheck.normalized)
