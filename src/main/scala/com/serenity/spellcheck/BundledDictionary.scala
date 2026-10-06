package com.serenity.spellcheck

import java.nio.file.Path

import scala.io.Source
import scala.util.Using

import com.serenity.config.SpellCheckDictionaryFingerprint

/** A Hunspell dictionary shipped inside the application and used as the default for its `languages`.
  *
  * `standsInFor` are bare language tags (`en`) it serves when nothing supplied serves that language or any region of it
  * (`en-US`); `languages` are the exact tags it serves under its own name.
  *
  * `inertDirectives` are affix directives the dictionary declares that [[HunspellFormat]] does not apply but that
  * cannot change which words are accepted, so loading it must not warn about them.
  */
final private[spellcheck] case class BundledDictionary(
    name: String,
    languages: Set[String],
    standsInFor: Set[String],
    dictionaryResource: String,
    affixResource: String,
    inertDirectives: Set[String]
):

  def servedLanguages: Set[String] = languages ++ standsInFor

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
      standsInFor = Set("en"),
      dictionaryResource = "/spellcheck/en_GB.dic",
      affixResource = "/spellcheck/en_GB.aff",
      inertDirectives = Set("BREAK", "PHONE")
    )

  val all: List[BundledDictionary] = List(EnGb)

  /** The bundled dictionaries to load for `languages`. A user-supplied dictionary named for a language (`en_GB.dic` for
    * `en-gb`) always replaces the bundled one. A bare tag it stands in for (`en`) uses it only when no dictionary that
    * `loaded` words from serves that tag or one of its regions, and none is named for a language that was not asked for
    * (a bare `words.dic`, which may be serving anything). A dictionary that failed to load serves nothing, so `en`
    * falls back to the bundled one rather than to no dictionary at all.
    */
  def defaultsFor(languages: List[String], sourcePaths: List[Path], loaded: Path => Boolean): List[BundledDictionary] =
    val requested = languages.map(languageKey).toSet
    val supplied  = sourcePaths.flatMap(dictionaryLanguage).toSet
    val serving   = sourcePaths.filter(loaded).flatMap(dictionaryLanguage)
    def named(dictionary: BundledDictionary) =
      dictionary.languages.exists(requested.contains) && !dictionary.languages.exists(supplied)
    def standingIn(dictionary: BundledDictionary) =
      dictionary.standsInFor.exists { tag =>
        requested.contains(tag) && !serving.exists(language => inFamily(tag, language) || !requested.contains(language))
      }
    all.filter(dictionary => named(dictionary) || standingIn(dictionary))

  private def inFamily(tag: String, language: String): Boolean =
    language == tag || language.startsWith(s"$tag-")

  def dictionaryLanguage(path: Path): Option[String] =
    Option(path.getFileName)
      .map(_.toString)
      .filter(_.toLowerCase.endsWith(".dic"))
      .map(name => languageKey(name.dropRight(4)))

  def languageKey(tag: String): String =
    tag.trim.toLowerCase.replace('_', '-')
