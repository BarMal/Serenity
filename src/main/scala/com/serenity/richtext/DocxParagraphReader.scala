package com.serenity.richtext

import com.serenity.richtext.DocxProperties.{ParagraphKinds, RunKinds, WNs}
import com.serenity.richtext.XmlDom.{attribute, childElement, childElements, isElement}
import org.w3c.dom.Element

/** What a paragraph read needs besides the paragraph itself: relationship targets and, when the part's text could be
  * mapped, the means to keep unmodelled XML verbatim.
  */
final private[richtext] case class DocxReadContext(links: Map[String, String], sources: Option[SourceMap]):
  def raw(element: Element): Option[String] = sources.flatMap(_.raw(element))

/** Turns `w:p` elements into model paragraphs. Anything the model does not hold is kept, never dropped: unknown
  * paragraph and run properties travel with the paragraph and run, and unknown inline content becomes an opaque atom.
  */
private[richtext] object DocxParagraphReader:
  private val RelNs = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  private val MarkerElements = Set(
    "bookmarkStart",
    "bookmarkEnd",
    "commentRangeStart",
    "commentRangeEnd",
    "permStart",
    "permEnd",
    "moveFromRangeStart",
    "moveFromRangeEnd",
    "moveToRangeStart",
    "moveToRangeEnd"
  )

  private val MarkerRunContent     = Set("fldChar", "instrText", "commentReference", "annotationRef")
  private val DiscardedElements    = Set("proofErr", "lastRenderedPageBreak")
  private val LinkTargetAttributes = """\s+[\w.-]+:(id|anchor)\s*=\s*("[^"]*"|'[^']*')""".r
  private val TransparentWrappers  = Set("smartTag", "customXml")

  def read(element: Element, blockIndex: Int, context: DocxReadContext): RichTextParagraph =
    val properties = childElement(element, WNs, "pPr")
    val alignment = properties
      .flatMap(childElement(_, WNs, "jc"))
      .flatMap(attribute(_, WNs, "val"))
      .map(alignmentFromValue)
      .getOrElse(ParagraphAlignment.Left)
    val role = properties
      .flatMap(dropCapRole)
      .orElse(properties.flatMap(childElement(_, WNs, "pStyle")).flatMap(attribute(_, WNs, "val")).flatMap(headingRole))
      .getOrElse(ParagraphRole.Body)
    val content = childElements(element)
      .filterNot(isElement(_, WNs, "pPr"))
      .flatMap(runsFromNode(_, context, None))
    val imported = RichTextParagraph(content, alignment, role).normalized
    context.sources.fold(imported) { sources =>
      imported.copy(source =
        Some(
          ParagraphSource(
            blockIndex,
            attributesOf(element, sources),
            properties.toList.flatMap(childElements).flatMap(rawParagraphProperty(_, imported, context)),
            Some(imported)
          )
        )
      )
    }

  private def attributesOf(element: Element, sources: SourceMap): String =
    val tag  = sources.openTag(element).getOrElse("")
    val name = sources.qualifiedName(element).getOrElse("")
    tag.stripPrefix("<" + name).stripSuffix(">").stripSuffix("/")

  private def rawParagraphProperty(
    property: Element,
    imported: RichTextParagraph,
    context: DocxReadContext
  ): Option[RawProperty] =
    val name = propertyName(property)
    context.raw(property).map { raw =>
      RawProperty(
        name,
        raw,
        Option.when(ParagraphKinds.contains(name))(name).flatMap(DocxProperties.paragraphValue(_, imported))
      )
    }

  private def propertyName(property: Element): String =
    if property.getNamespaceURI == WNs then property.getLocalName else property.getTagName

  private def alignmentFromValue(value: String): ParagraphAlignment =
    value match
      case "center"              => ParagraphAlignment.Center
      case "right" | "end"       => ParagraphAlignment.Right
      case "both" | "distribute" => ParagraphAlignment.Justify
      case _                     => ParagraphAlignment.Left

  /** DOCX's native drop cap: a `w:framePr` paragraph frame property carrying `w:dropCap` (`"drop"` or `"margin"`) and
    * `w:lines` (the span, in lines). It is a frame property, not a named style, unlike headings.
    */
  private def dropCapRole(properties: Element): Option[ParagraphRole] =
    childElement(properties, WNs, "framePr").flatMap { frame =>
      attribute(frame, WNs, "dropCap").filter(_.nonEmpty).map { _ =>
        ParagraphRole.dropCap(
          attribute(frame, WNs, "lines").flatMap(_.toIntOption).getOrElse(ParagraphRole.DefaultDropCapLines)
        )
      }
    }

  private def headingRole(value: String): Option[ParagraphRole] =
    val normalized = value.toLowerCase
    Option.when(normalized.startsWith("heading")) {
      ParagraphRole.Heading(normalized.drop("heading".length).filter(_.isDigit).toIntOption.getOrElse(1).max(1))
    }

  private def runsFromNode(element: Element, context: DocxReadContext, link: Option[String]): List[RichTextRun] =
    val name = Option.when(element.getNamespaceURI == WNs)(element.getLocalName).getOrElse("")
    name match
      case "r"                                       => runsFromRun(element, context, link)
      case "hyperlink"                               => hyperlinkRuns(element, context, link)
      case wrapper if TransparentWrappers(wrapper)   => childRuns(element, context, link)
      case discarded if DiscardedElements(discarded) => Nil
      case marker if MarkerElements(marker)          => opaque(context.raw(element), visible = false, link)
      case _                                         => opaque(context.raw(element), visible = true, link)

  private def hyperlinkRuns(element: Element, context: DocxReadContext, link: Option[String]): List[RichTextRun] =
    val target = hyperlinkTarget(element, context).orElse(link)
    val notes = context.sources.flatMap(_.openTag(element)).toList.flatMap { tag =>
      val attributes =
        tag.stripPrefix("<" + context.sources.flatMap(_.qualifiedName(element)).getOrElse("")).stripSuffix(">")
      RawProperty.linkAttributes(LinkTargetAttributes.replaceAllIn(attributes, "").stripSuffix("/"), target)
    }
    childRuns(element, context, target).map(run => run.copy(style = run.style.copy(extras = run.style.extras ++ notes)))

  private def childRuns(element: Element, context: DocxReadContext, link: Option[String]): List[RichTextRun] =
    childElements(element).flatMap(runsFromNode(_, context, link))

  private def opaque(raw: Option[String], visible: Boolean, link: Option[String]): List[RichTextRun] =
    raw.map(RichTextRun.opaque(_, visible, linkOnly(link))).toList

  private def linkOnly(link: Option[String]): RichTextStyle =
    link.fold(RichTextStyle.empty)(RichTextStyle.empty.withLink)

  private def hyperlinkTarget(element: Element, context: DocxReadContext): Option[String] =
    attribute(element, RelNs, "id")
      .flatMap(context.links.get)
      .orElse(attribute(element, WNs, "anchor").map("#" + _))

  private def runsFromRun(element: Element, context: DocxReadContext, link: Option[String]): List[RichTextRun] =
    val properties = childElement(element, WNs, "rPr")
    val style      = properties.map(runStyle(_, context)).getOrElse(RichTextStyle.empty).copy(link = link)
    childElements(element).filterNot(isElement(_, WNs, "rPr")).flatMap { child =>
      val name = Option.when(child.getNamespaceURI == WNs)(child.getLocalName).getOrElse("")
      name match
        case "t"                                       => Option(child.getTextContent).map(RichTextRun(_, style)).toList
        case "tab"                                     => List(RichTextRun("\t", style))
        case "br" if plainBreak(child)                 => List(RichTextRun.softBreak(style))
        case discarded if DiscardedElements(discarded) => Nil
        case marker if MarkerRunContent(marker) =>
          opaqueRun(element, properties, child, context, style, visible = false)
        case _ => opaqueRun(element, properties, child, context, style, visible = true)
    }

  private def plainBreak(element: Element): Boolean =
    attribute(element, WNs, "type").forall(_ == "textWrapping") && attribute(element, WNs, "clear").isEmpty

  /** The run holding just `child`, as the source wrote it, so the child keeps its run properties and wrapper. */
  private def opaqueRun(
    run: Element,
    properties: Option[Element],
    child: Element,
    context: DocxReadContext,
    style: RichTextStyle,
    visible: Boolean
  ): List[RichTextRun] =
    (for
      sources <- context.sources
      open    <- sources.openTag(run)
      close   <- sources.closeTag(run)
      inner   <- context.raw(child)
    yield RichTextRun.opaque(
      open + properties.flatMap(context.raw).getOrElse("") + inner + close,
      visible,
      linkOnly(style.link)
    )).toList

  private def runStyle(properties: Element, context: DocxReadContext): RichTextStyle =
    val style = RichTextStyle(
      marks = List(
        Option.when(toggleEnabled(properties, "b"))(InlineMark.Bold),
        Option.when(toggleEnabled(properties, "i"))(InlineMark.Italic),
        Option.when(underlineEnabled(properties))(InlineMark.Underline)
      ).flatten.toSet,
      fontFamily = childElement(properties, WNs, "rFonts")
        .flatMap(fonts => attribute(fonts, WNs, "ascii").orElse(attribute(fonts, WNs, "hAnsi"))),
      fontSize = childElement(properties, WNs, "sz").flatMap(attribute(_, WNs, "val")).flatMap(halfPoints),
      color = childElement(properties, WNs, "color").flatMap(attribute(_, WNs, "val")).flatMap(parseColor)
    )
    style.copy(extras = childElements(properties).flatMap(rawRunProperty(_, style, context)))

  /** A run property is kept when the model cannot write it back exactly as it was: anything it does not model, and
    * modelled ones that say more than the model does (an East Asian font, a double underline, a `w:val="0"`).
    */
  private def rawRunProperty(property: Element, style: RichTextStyle, context: DocxReadContext): Option[RawProperty] =
    val name     = propertyName(property)
    val modelled = Option.when(RunKinds.contains(name))(name).flatMap(DocxProperties.runValue(_, style))
    context
      .raw(property)
      .filter(raw => !RunKinds.contains(name) || modelled.map(DocxProperties.runXml(name, _)) != Some(raw))
      .map(RawProperty(name, _, modelled))

  private def toggleEnabled(element: Element, localName: String): Boolean =
    childElement(element, WNs, localName).exists(child =>
      attribute(child, WNs, "val").forall(value => value != "false" && value != "0")
    )

  private def underlineEnabled(element: Element): Boolean =
    childElement(element, WNs, "u").exists(child => attribute(child, WNs, "val").forall(_ != "none"))

  private def halfPoints(value: String): Option[Float] =
    value.toFloatOption.map(_ / 2.0f)

  private def parseColor(value: String): Option[String] =
    Option(value)
      .map(_.stripPrefix("#"))
      .filter(hex => hex.matches("[0-9a-fA-F]{6}") && hex != "000000")
      .map(hex => s"#${hex.toLowerCase}")
