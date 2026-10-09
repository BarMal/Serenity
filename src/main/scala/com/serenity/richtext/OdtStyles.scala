package com.serenity.richtext

import com.serenity.richtext.XmlDom.{attribute, childElement, elements, escapeAttribute}
import org.w3c.dom.{Document as XmlDocument, Element}

/** A paragraph style's alignment and, when the style carries a `<style:drop-cap>` child, the drop cap's line span.
  * Keeping both on the same style keyed by name (rather than a separate lookup) is what lets the write side dedupe
  * paragraphs sharing both properties into one style, and keeps a drop-cap paragraph and a plain one with the same
  * alignment from colliding onto the same style.
  */
final private[richtext] case class OdtParagraphStyle(alignment: ParagraphAlignment, dropCapLines: Option[Int]):
  /** The form [[RawProperty.modelled]] records, so a rewrite can tell whether the model's value has changed. */
  def canonical: String = s"$alignment|${dropCapLines.getOrElse(0)}"

object OdtParagraphStyle:

  private[richtext] def of(paragraph: RichTextParagraph): OdtParagraphStyle =
    OdtParagraphStyle(
      paragraph.alignment,
      paragraph.role match
        case ParagraphRole.DropCap(lines) => Some(lines.max(1))
        case _                            => None
    )

final private[richtext] case class OdtStyles(
    textStyles: Map[String, RichTextStyle],
    paragraphStyles: Map[String, OdtParagraphStyle]
)

/** The automatic styles of `content.xml`: reading them into the model, and writing the model's formatting as them. */
private[richtext] object OdtStyles:
  val OfficeNs = "urn:oasis:names:tc:opendocument:xmlns:office:1.0"
  val StyleNs  = "urn:oasis:names:tc:opendocument:xmlns:style:1.0"
  val TextNs   = "urn:oasis:names:tc:opendocument:xmlns:text:1.0"
  val FoNs     = "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
  val XlinkNs  = "http://www.w3.org/1999/xlink"

  def fromDocument(document: XmlDocument): OdtStyles =
    val styleElements = elements(document.getElementsByTagNameNS(StyleNs, "style"))
    OdtStyles(
      textStyles = styleElements.flatMap(textStyleFromElement).toMap,
      paragraphStyles = styleElements.flatMap(paragraphStyleFromElement).toMap
    )

  /** The formatting ODT text styles can express: the style without its link and without source-format extras. */
  def formatOf(style: RichTextStyle): RichTextStyle =
    style.copy(link = None, extras = Nil)

  /** The form [[RawProperty.modelled]] records for a run's formatting. */
  def canonical(style: RichTextStyle): String =
    val format = formatOf(style)
    s"${format.marks.toList.map(_.toString).sorted.mkString(",")}|${format.fontFamily}|${format.fontSize}|${format.color}"

  def mergeStyles(base: RichTextStyle, overlay: RichTextStyle): RichTextStyle =
    base.copy(
      marks = base.marks ++ overlay.marks,
      fontFamily = overlay.fontFamily.orElse(base.fontFamily),
      fontSize = overlay.fontSize.orElse(base.fontSize),
      color = overlay.color.orElse(base.color)
    )

  def textStyleXml(name: String, style: RichTextStyle, parent: Option[String], declarations: String): String =
    s"""<style:style style:name="$name" style:family="text"${parentAttribute(parent)}$declarations><style:text-properties${textPropertiesAttributes(
        style
      )}/></style:style>"""

  def paragraphStyleXml(
    name: String,
    style: OdtParagraphStyle,
    parent: Option[String],
    declarations: String
  ): String =
    s"""<style:style style:name="$name" style:family="paragraph"${parentAttribute(parent)}$declarations><style:paragraph-properties fo:text-align="${alignmentAttribute(
        style.alignment
      )}">${dropCapXml(style.dropCapLines)}</style:paragraph-properties></style:style>"""

  private def parentAttribute(parent: Option[String]): String =
    parent.fold("")(name => s""" style:parent-style-name="${escapeAttribute(name)}"""")

  private def textStyleFromElement(element: Element): Option[(String, RichTextStyle)] =
    Option
      .when(attribute(element, StyleNs, "family").contains("text")) {
        val style = childElement(element, StyleNs, "text-properties")
          .map(textStyleFromProperties)
          .getOrElse(RichTextStyle.empty)
        attribute(element, StyleNs, "name").map(_ -> style)
      }
      .flatten

  private def paragraphStyleFromElement(element: Element): Option[(String, OdtParagraphStyle)] =
    Option
      .when(attribute(element, StyleNs, "family").contains("paragraph")) {
        val properties   = childElement(element, StyleNs, "paragraph-properties")
        val alignment    = properties.flatMap(paragraphAlignmentFromProperties).getOrElse(ParagraphAlignment.Left)
        val dropCapLines = properties.flatMap(dropCapLinesFromProperties)
        attribute(element, StyleNs, "name").map(_ -> OdtParagraphStyle(alignment, dropCapLines))
      }
      .flatten

  private def textStyleFromProperties(element: Element): RichTextStyle =
    RichTextStyle(
      marks = List(
        Option.when(isBold(element))(InlineMark.Bold),
        Option.when(attribute(element, FoNs, "font-style").contains("italic"))(InlineMark.Italic),
        Option.when(isUnderlined(element))(InlineMark.Underline)
      ).flatten.toSet,
      fontFamily = attribute(element, StyleNs, "font-name").orElse(attribute(element, FoNs, "font-family")),
      fontSize = attribute(element, FoNs, "font-size").flatMap(parsePointSize),
      color = attribute(element, FoNs, "color").filterNot(_ == "#000000")
    )

  private def isBold(element: Element): Boolean =
    attribute(element, FoNs, "font-weight").exists(weight => weight == "bold" || weight.toIntOption.exists(_ >= 600))

  private def isUnderlined(element: Element): Boolean =
    attribute(element, StyleNs, "text-underline-style").exists(_ != "none")

  private def parsePointSize(value: String): Option[Float] =
    value.stripSuffix("pt").toFloatOption

  private def paragraphAlignmentFromProperties(element: Element): Option[ParagraphAlignment] =
    attribute(element, FoNs, "text-align").map {
      case "center"                => ParagraphAlignment.Center
      case "end" | "right"         => ParagraphAlignment.Right
      case "justify" | "justified" => ParagraphAlignment.Justify
      case _                       => ParagraphAlignment.Left
    }

  /** ODF's native drop cap representation: a `<style:drop-cap style:lines="N" style:length="M"/>` child of the
    * paragraph style's `<style:paragraph-properties>`, per ODF 1.2 section 17.17.
    */
  private def dropCapLinesFromProperties(element: Element): Option[Int] =
    childElement(element, StyleNs, "drop-cap").flatMap(attribute(_, StyleNs, "lines")).flatMap(_.toIntOption)

  private def dropCapXml(dropCapLines: Option[Int]): String =
    dropCapLines.map(lines => s"""<style:drop-cap style:lines="${lines.max(1)}" style:length="1"/>""").getOrElse("")

  private def textPropertiesAttributes(style: RichTextStyle): String =
    List(
      Option.when(style.marks.contains(InlineMark.Bold))("""fo:font-weight="bold""""),
      Option.when(style.marks.contains(InlineMark.Italic))("""fo:font-style="italic""""),
      Option.when(style.marks.contains(InlineMark.Underline))("""style:text-underline-style="solid""""),
      style.fontFamily.map(value =>
        s"""style:font-name="${escapeAttribute(value)}" fo:font-family="${escapeAttribute(value)}""""
      ),
      style.fontSize.map(value => s"""fo:font-size="${value.round}pt""""),
      style.color.map(value => s"""fo:color="${escapeAttribute(value)}"""")
    ).flatten match
      case Nil        => ""
      case attributes => " " + attributes.mkString(" ")

  private def alignmentAttribute(alignment: ParagraphAlignment): String =
    alignment match
      case ParagraphAlignment.Left    => "start"
      case ParagraphAlignment.Center  => "center"
      case ParagraphAlignment.Right   => "end"
      case ParagraphAlignment.Justify => "justify"
