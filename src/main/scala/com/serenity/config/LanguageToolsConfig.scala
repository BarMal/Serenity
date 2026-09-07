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
    */
  def discoverDictionarySourcePaths(config: SpellCheckConfig): List[Path] =
    val normalized = config.normalized
    normalized.dictionaryPaths.flatMap(path => expandDictionaryPath(path, normalized.languages)).distinct

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
