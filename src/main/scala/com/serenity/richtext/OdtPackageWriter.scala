package com.serenity.richtext

import java.nio.charset.StandardCharsets

import com.serenity.richtext.OdtStyles.{FoNs, OfficeNs, StyleNs, TextNs, XlinkNs}

/** Writes `content.xml` of an ODT package, and saves a document read from one: the package with `content.xml` replaced
  * (the paragraphs that changed written again, with automatic styles added for them) and every other part copied.
  */
private[richtext] object OdtPackageWriter:
  private val ContentEntry = "content.xml"

  private val AutomaticStylesEnd   = """</[\w.-]+:automatic-styles>""".r
  private val AutomaticStylesEmpty = """<([\w.-]+):automatic-styles\s*/>""".r
  private val BodyStart            = """<([\w.-]+):body[\s>]""".r

  final private case class NewParagraphStyle(style: OdtParagraphStyle, parent: Option[String])
  final private case class NewTextStyle(style: RichTextStyle, parent: Option[String])

  def rewrite(document: RichTextDocument, source: DocumentSource): Array[Byte] =
    val paragraphs = document.paragraphs
    val plan       = source.body.fold(BodyPlanner.planWithoutSlices(paragraphs))(BodyPlanner.plan(paragraphs, _))
    val rewritten  = plan.collect { case PlannedBlock.Rewrite(paragraph, _) => paragraph }.toList
    val head       = source.body.fold("")(_.head)
    val generated  = generatedStyles(rewritten, head)
    val naming     = nativeNaming(generated, head)
    val markup = plan.map {
      case PlannedBlock.Verbatim(text)          => text
      case PlannedBlock.Rewrite(paragraph, gap) => OdtParagraphWriter.paragraphXml(paragraph, naming) + gap
    }.mkString
    val contentXml = (source.main, source.body) match
      case (Some(_), Some(body)) => withAutomaticStyles(body.head, generated.xml) + markup + body.tail
      case _                     => newContentXml(document)
    PackageRewriter.rewrite(
      source.archive,
      "ODT",
      Map(ContentEntry -> source.main.fold(contentXml.getBytes(StandardCharsets.UTF_8))(_.encode(contentXml))),
      Nil
    )

  /** `content.xml` for a document with no ODT source. */
  def newContentXml(document: RichTextDocument): String =
    val paragraphs = document.paragraphs
    val textStyleNames = paragraphs
      .flatMap(_.runs.map(run => OdtStyles.formatOf(run.style)))
      .filterNot(_ == RichTextStyle.empty)
      .distinct
      .zipWithIndex
      .map((style, index) => style -> s"T$index")
      .toMap
    val paragraphStyleNames = paragraphs
      .map(OdtParagraphStyle.of)
      .distinct
      .zipWithIndex
      .map((style, index) => style -> s"P$index")
      .toMap
    val naming = OdtNaming(
      paragraph => paragraphStyleNames.get(OdtParagraphStyle.of(paragraph)),
      style => textStyleNames.get(OdtStyles.formatOf(style)),
      anchorDeclarations = "",
      native = false
    )
    val styles =
      textStyleNames.toList.sortBy(_._2).map((style, name) => OdtStyles.textStyleXml(name, style, None, "")) ++
        paragraphStyleNames.toList.sortBy(_._2).map((style, name) => OdtStyles.paragraphStyleXml(name, style, None, ""))
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<office:document-content
       |    xmlns:office="$OfficeNs"
       |    xmlns:style="$StyleNs"
       |    xmlns:text="$TextNs"
       |    xmlns:xlink="$XlinkNs"
       |    xmlns:fo="$FoNs">
       |  <office:automatic-styles>
       |${styles.map("    " + _).mkString("\n")}
       |  </office:automatic-styles>
       |  <office:body>
       |    <office:text>
       |${paragraphs.map(paragraph => "      " + OdtParagraphWriter.paragraphXml(paragraph, naming)).mkString("\n")}
       |    </office:text>
       |  </office:body>
       |</office:document-content>""".stripMargin

  final private case class Generated(
      paragraphs: Map[NewParagraphStyle, String],
      texts: Map[NewTextStyle, String],
      declarations: String
  ):

    def xml: String =
      texts.toList
        .sortBy(_._2)
        .map((style, name) => OdtStyles.textStyleXml(name, style.style, style.parent, declarations))
        .mkString +
        paragraphs.toList
          .sortBy(_._2)
          .map((style, name) => OdtStyles.paragraphStyleXml(name, style.style, style.parent, declarations))
          .mkString

  /** Automatic styles for the formatting of rewritten paragraphs that no existing style name stands for. */
  private def generatedStyles(rewritten: List[RichTextParagraph], head: String): Generated =
    val paragraphKeys = rewritten.flatMap(newParagraphStyle(_, head)).distinct
    val textKeys = rewritten
      .flatMap(_.runs.flatMap(run => newTextStyle(run.style)))
      .distinct
    def fresh(prefix: String, count: Int): List[String] =
      Iterator
        .from(1)
        .map(number => s"$prefix$number")
        .filterNot(name => head.contains(s""""$name""""))
        .take(count)
        .toList
    val declarations =
      List("style" -> StyleNs, "fo" -> FoNs)
        .filterNot((prefix, namespace) => head.contains(s"""xmlns:$prefix="$namespace""""))
        .map((prefix, namespace) => s""" xmlns:$prefix="$namespace"""")
        .mkString
    Generated(
      paragraphKeys.zip(fresh("SerenityParagraph", paragraphKeys.size)).toMap,
      textKeys.zip(fresh("SerenityText", textKeys.size)).toMap,
      declarations
    )

  /** The name an existing style gives the paragraph unchanged, else the style that has to be written for it. */
  private def newParagraphStyle(paragraph: RichTextParagraph, head: String): Option[NewParagraphStyle] =
    val current  = OdtParagraphStyle.of(paragraph)
    val existing = paragraph.source.flatMap(_.properties.find(_.name == OdtParagraphReader.StyleNameProperty))
    Option.unless(existing.exists(_.modelled.contains(current.canonical)) || (existing.isEmpty && isPlain(current)))(
      NewParagraphStyle(current, existing.map(_.raw).filterNot(name => head.contains(s"""style:name="$name"""")))
    )

  private def isPlain(style: OdtParagraphStyle): Boolean =
    style.alignment == ParagraphAlignment.Left && style.dropCapLines.isEmpty

  private def newTextStyle(style: RichTextStyle): Option[NewTextStyle] =
    val format   = OdtStyles.formatOf(style)
    val existing = style.extras.find(_.name == OdtParagraphReader.SpanStyleProperty)
    Option.unless(existing.exists(_.modelled.contains(OdtStyles.canonical(style))) || format == RichTextStyle.empty)(
      NewTextStyle(format, existing.map(_.raw))
    )

  private def nativeNaming(generated: Generated, head: String): OdtNaming =
    OdtNaming(
      paragraph = paragraph =>
        newParagraphStyle(paragraph, head).fold(
          paragraph.source.flatMap(_.properties.find(_.name == OdtParagraphReader.StyleNameProperty)).map(_.raw)
        )(generated.paragraphs.get),
      span = style =>
        newTextStyle(style).fold(
          style.extras.find(_.name == OdtParagraphReader.SpanStyleProperty).map(_.raw)
        )(generated.texts.get),
      anchorDeclarations = if head.contains(s"""xmlns:xlink="$XlinkNs"""") then "" else s""" xmlns:xlink="$XlinkNs"""",
      native = true
    )

  /** `head` with `styles` added to its automatic styles, which it gains if it has none. */
  private def withAutomaticStyles(head: String, styles: String): String =
    if styles.isEmpty then head
    else
      AutomaticStylesEnd.findFirstMatchIn(head) match
        case Some(end) => head.substring(0, end.start) + styles + head.substring(end.start)
        case None =>
          AutomaticStylesEmpty.findFirstMatchIn(head) match
            case Some(empty) =>
              val prefix = empty.group(1)
              head.substring(0, empty.start) + s"<$prefix:automatic-styles>$styles</$prefix:automatic-styles>" +
                head.substring(empty.end)
            case None =>
              BodyStart.findFirstMatchIn(head).fold(head) { body =>
                val prefix = body.group(1)
                head.substring(0, body.start) + s"<$prefix:automatic-styles>$styles</$prefix:automatic-styles>" +
                  head.substring(body.start)
              }
