package com.serenity.manuscript

import java.util.Locale

/** The fixed wording of a reflowable book -- navigation headings and the titles of its front matter -- which is the
  * reader's language rather than the writer's.
  */
enum LabelKey(val key: String):
  case Contents       extends LabelKey("contents")
  case Guide          extends LabelKey("guide")
  case TitlePage      extends LabelKey("title-page")
  case StartOfContent extends LabelKey("start-of-content")
  case Dedication     extends LabelKey("dedication")
  case Chapter        extends LabelKey("chapter")

object LabelKey:
  def fromKey(key: String): Option[LabelKey] =
    LabelKey.values.find(_.key == key.trim.toLowerCase(Locale.ROOT))

/** `chapter` is the title of a chapter that has no heading of its own; `{n}` stands for its number. */
final case class NavigationLabels(
    contents: String,
    guide: String,
    titlePage: String,
    startOfContent: String,
    dedication: String,
    chapter: String
):

  def chapterTitle(number: Int): String = chapter.replace("{n}", number.toString)

  /** Blank overrides are ignored, so an empty `labels.guide = ""` in a conf file keeps the language's own. */
  def overriddenBy(overrides: Map[LabelKey, String]): NavigationLabels =
    def pick(key: LabelKey, own: String): String =
      overrides.get(key).map(_.trim).filter(_.nonEmpty).getOrElse(own)
    NavigationLabels(
      contents = pick(LabelKey.Contents, contents),
      guide = pick(LabelKey.Guide, guide),
      titlePage = pick(LabelKey.TitlePage, titlePage),
      startOfContent = pick(LabelKey.StartOfContent, startOfContent),
      dedication = pick(LabelKey.Dedication, dedication),
      chapter = pick(LabelKey.Chapter, chapter)
    )

object NavigationLabels:

  val English: NavigationLabels =
    NavigationLabels("Contents", "Guide", "Title Page", "Start of Content", "Dedication", "Chapter {n}")

  private val byLanguage: Map[String, NavigationLabels] = Map(
    "en"    -> English,
    "en-gb" -> English,
    "fr" -> NavigationLabels(
      "Table des matières",
      "Guide",
      "Page de titre",
      "Début du contenu",
      "Dédicace",
      "Chapitre {n}"
    ),
    "de" -> NavigationLabels("Inhalt", "Wegweiser", "Titelseite", "Textanfang", "Widmung", "Kapitel {n}"),
    "es" -> NavigationLabels("Contenido", "Guía", "Portada", "Inicio del contenido", "Dedicatoria", "Capítulo {n}"),
    "it" -> NavigationLabels("Indice", "Guida", "Frontespizio", "Inizio del contenuto", "Dedica", "Capitolo {n}"),
    "pt" -> NavigationLabels("Sumário", "Guia", "Folha de rosto", "Início do conteúdo", "Dedicatória", "Capítulo {n}"),
    "nl" -> NavigationLabels("Inhoud", "Gids", "Titelpagina", "Begin van de inhoud", "Opdracht", "Hoofdstuk {n}")
  )

  /** The exact tag first (`en-GB`), then its base language (`fr-CA` gives `fr`), then English. */
  def forLanguage(tag: String): NavigationLabels =
    val normalised = tag.trim.toLowerCase(Locale.ROOT).replace('_', '-')
    val base       = normalised.takeWhile(_ != '-')
    byLanguage.get(normalised).orElse(byLanguage.get(base)).getOrElse(English)

  def resolve(language: String, overrides: Map[LabelKey, String]): NavigationLabels =
    forLanguage(language).overriddenBy(overrides)
