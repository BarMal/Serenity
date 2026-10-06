package com.serenity.spellcheck

import java.nio.file.Path

import scala.io.Source
import scala.util.Using

import com.serenity.config.SpellCheckDictionaryFingerprint

/** A Hunspell dictionary shipped inside the application and used as the default for its `languages`.
  *
  * `inertDirectives` are affix directives the dictionary declares that [[HunspellFormat]] does not apply but that
  * cannot change which words are accepted, so loading it must not warn about them.
  */
final private[spellcheck] case class BundledDictionary(
    name: String,
    languages: Set[String],
    dictionaryResource: String,
    affixResource: String,
    inertDirectives: Set[String]
):

  def fingerprint: SpellCheckDictionaryFingerprint =
    SpellCheckDictionaryFingerprint(
      path = s"classpath:$dictionaryResource",
      exists = Option(getClass.getResource(dictionaryResource)).isDefined,
      isDirectory = false,
      size = 0L,
      lastModifiedMillis = 0L
    )

  def dictionaryLines: Either[String, List[String]] = readLines(dictionaryResource)

  def affixLines: Either[String, List[String]] = readLines(affixResource)

  private def readLines(resource: String): Either[String, List[String]] =
    Option(getClass.getResourceAsStream(resource))
      .toRight(s"Bundled dictionary resource is missing: $resource")
      .flatMap { stream =>
        Using(Source.fromInputStream(stream, "UTF-8"))(
          _.getLines().map(_.trim.stripPrefix(BundledDictionary.ByteOrderMark)).toList
        ).toEither.left
          .map(error => s"Could not read bundled dictionary resource $resource: ${error.getMessage}")
      }

private[spellcheck] object BundledDictionary:

  private[spellcheck] val ByteOrderMark = "\uFEFF"

  /** LibreOffice's en_GB (Marco A.G.Pinto's British English). Its `PHONE` table only drives phonetic suggestions and
    * its `BREAK` table splits at the dashes that `SpellChecker`'s tokenizer already treats as word separators, so
    * neither changes which words are accepted.
    */
  val EnGb: BundledDictionary =
    BundledDictionary(
      name = "en_GB",
      languages = Set("en-gb"),
      dictionaryResource = "/spellcheck/en_GB.dic",
      affixResource = "/spellcheck/en_GB.aff",
      inertDirectives = Set("BREAK", "PHONE")
    )

  val all: List[BundledDictionary] = List(EnGb)

  /** The bundled dictionaries serving `languages` that no dictionary in `sourcePaths` already serves: a user-supplied
    * dictionary named for the language (`en_GB.dic` for `en-gb`) always replaces the bundled one.
    */
  def defaultsFor(languages: List[String], sourcePaths: List[Path]): List[BundledDictionary] =
    val requested = languages.map(languageKey).toSet
    val supplied  = sourcePaths.flatMap(dictionaryLanguage).toSet
    all.filter(dictionary => dictionary.languages.exists(requested.contains) && !dictionary.languages.exists(supplied))

  private def dictionaryLanguage(path: Path): Option[String] =
    Option(path.getFileName)
      .map(_.toString)
      .filter(_.toLowerCase.endsWith(".dic"))
      .map(name => languageKey(name.dropRight(4)))

  private def languageKey(tag: String): String =
    tag.trim.toLowerCase.replace('_', '-')
