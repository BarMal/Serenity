package com.serenity.manuscript

import cats.syntax.all.*
import pureconfig.{ConfigReader, ConfigSource}

/** Reads `manuscript.conf`, the HOCON file beside a book's sources that turns them into a document set. Every key is
  * optional and overrides the defaults it is decoded against:
  *
  * {{{
  * title = "The Long Night"
  * short-title = "NIGHT"            # running-header keyword; defaults to the title in capitals
  * author = "Jane Q. Writer"
  * surname = "Writer"               # when the last word of the author's name is not the surname
  * byline = "J. Q. Writer"
  * contact = ["1 High Street", "jane@example.com"]
  * sources = [{ path = "01-arrival.md" }, { path = "notes.md", exclude = true }]
  * format = "modern"                # modern | classic
  * paper = "letter"                 # letter | a4
  * chapter-heading = "Chapter <$n>\n<$t>"
  * part-heading = "Part <$R>"
  * part-level = 1
  * chapter-level = 2
  * scene-break-patterns = ["#", "***"]
  * transforms = ["smart-punctuation"]   # defaults to the format's own
  * replacements = [{ find = "Bob", replace = "Robert" }]
  * title-page = true
  * word-count = "novel"             # novel | short-fiction | exact
  * dedication = "For M."
  * end-marker = "END"               # "" for none
  * language = "en"                  # BCP 47 tag, for EPUB
  * identifier = "urn:uuid:..."      # EPUB's permanent book id; derived from title and author when absent
  * labels.contents = "Inhalt"       # EPUB navigation headings, otherwise chosen by language; also
  *                                  # labels.guide, labels.title-page and labels.start-of-content
  * }}}
  */
object ManuscriptConf:

  val FileName: String = "manuscript.conf"

  final private case class SourceConf(path: String, exclude: Option[Boolean]) derives ConfigReader

  final private case class ReplacementConf(find: String, replace: String, regex: Option[Boolean]) derives ConfigReader

  final private case class ConfFile(
      title: Option[String],
      shortTitle: Option[String],
      author: Option[String],
      surname: Option[String],
      byline: Option[String],
      contact: Option[List[String]],
      sources: Option[List[SourceConf]],
      format: Option[String],
      paper: Option[String],
      chapterHeading: Option[String],
      partHeading: Option[String],
      partLevel: Option[Int],
      chapterLevel: Option[Int],
      sceneBreakPatterns: Option[List[String]],
      transforms: Option[List[String]],
      replacements: Option[List[ReplacementConf]],
      titlePage: Option[Boolean],
      wordCount: Option[String],
      dedication: Option[String],
      endMarker: Option[String],
      language: Option[String],
      identifier: Option[String],
      labels: Option[Map[String, String]]
  ) derives ConfigReader

  def decode(text: String, defaults: CompileSpec): Either[CompileError, CompileSpec] =
    ConfigSource
      .string(text)
      .load[ConfFile]
      .leftMap(failures => CompileError.InvalidConfiguration(failures.prettyPrint()))
      .flatMap(applied(_, defaults))

  private def applied(conf: ConfFile, defaults: CompileSpec): Either[CompileError, CompileSpec] =
    for
      preset     <- keyed(conf.format, "format", ManuscriptFormat.fromKey).map(_.getOrElse(defaults.format))
      paper      <- keyed(conf.paper, "paper", PaperSize.fromKey).map(_.getOrElse(preset.paper))
      transforms <- conf.transforms.traverse(_.traverse(required(_, "transform", TextTransform.fromKey)))
      rounding   <- keyed(conf.wordCount, "word-count", WordCountRounding.fromKey)
      rules      <- sectionRules(conf, defaults.rules)
      labels     <- conf.labels.traverse(labelOverrides)
    yield
      val author = conf.author.map(AuthorName.fromLegal).getOrElse(defaults.author)
      defaults.copy(
        title = conf.title.getOrElse(defaults.title),
        shortTitle = conf.shortTitle.orElse(defaults.shortTitle),
        author = conf.surname.fold(author)(surname => author.copy(surname = surname.trim)),
        byline = conf.byline.orElse(defaults.byline),
        contact = conf.contact.getOrElse(defaults.contact),
        sources = conf.sources.fold(defaults.sources)(
          _.map(source => SourceEntry(source.path, !source.exclude.getOrElse(false)))
        ),
        rules = rules,
        chapterHeading = conf.chapterHeading.fold(defaults.chapterHeading)(HeadingTemplate(_)),
        partHeading = conf.partHeading.fold(defaults.partHeading)(HeadingTemplate(_)),
        transforms = transforms.getOrElse(
          if conf.format.isDefined then preset.defaultTransforms else defaults.transforms
        ),
        replacements = conf.replacements.fold(defaults.replacements)(
          _.map(replacement => Replacement(replacement.find, replacement.replace, replacement.regex.getOrElse(false)))
        ),
        titlePage = conf.titlePage.getOrElse(defaults.titlePage),
        wordCountRounding = rounding.getOrElse(defaults.wordCountRounding),
        dedication = conf.dedication.orElse(defaults.dedication),
        endMarker = conf.endMarker.fold(defaults.endMarker)(marker => Option(marker.trim).filter(_.nonEmpty)),
        format = preset.copy(paper = paper),
        language = conf.language.map(_.trim).filter(_.nonEmpty).getOrElse(defaults.language),
        identifier = conf.identifier.map(_.trim).filter(_.nonEmpty).orElse(defaults.identifier),
        labels = labels.getOrElse(defaults.labels)
      )

  private def labelOverrides(raw: Map[String, String]): Either[CompileError, Map[LabelKey, String]] =
    raw.toList
      .traverse((name, text) =>
        LabelKey
          .fromKey(name)
          .toRight(
            CompileError.InvalidConfiguration(
              s"unknown label '$name'; expected one of ${LabelKey.values.map(_.key).mkString(", ")}"
            )
          )
          .map(_ -> text)
      )
      .map(_.toMap)

  private def sectionRules(conf: ConfFile, defaults: SectionRules): Either[CompileError, SectionRules] =
    val rules = SectionRules(
      partLevel = conf.partLevel.orElse(defaults.partLevel),
      chapterLevel = conf.chapterLevel.getOrElse(defaults.chapterLevel),
      sceneBreakPatterns = conf.sceneBreakPatterns.fold(defaults.sceneBreakPatterns)(_.map(_.trim).toSet)
    )
    Either.cond(
      rules.chapterLevel >= 1 && rules.partLevel.forall(level => level >= 1 && level < rules.chapterLevel),
      rules,
      CompileError.InvalidConfiguration("chapter-level must be at least 1, and part-level above it")
    )

  private def keyed[A](
    value: Option[String],
    key: String,
    lookup: String => Option[A]
  ): Either[CompileError, Option[A]] =
    value.traverse(required(_, key, lookup))

  private def required[A](raw: String, key: String, lookup: String => Option[A]): Either[CompileError, A] =
    lookup(raw).toRight(CompileError.InvalidConfiguration(s"unknown $key '$raw'"))
