package com.serenity.richtext

/** The `w:rPr` and `w:pPr` children the model understands, how it writes them, and the order the schema requires. */
private[richtext] object DocxProperties:
  val WNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

  private val RunOrder = Vector(
    "rStyle",
    "rFonts",
    "b",
    "bCs",
    "i",
    "iCs",
    "caps",
    "smallCaps",
    "strike",
    "dstrike",
    "outline",
    "shadow",
    "emboss",
    "imprint",
    "noProof",
    "snapToGrid",
    "vanish",
    "webHidden",
    "color",
    "spacing",
    "w",
    "kern",
    "position",
    "sz",
    "szCs",
    "highlight",
    "u",
    "effect",
    "bdr",
    "shd",
    "fitText",
    "vertAlign",
    "rtl",
    "cs",
    "em",
    "lang",
    "eastAsianLayout",
    "specVanish",
    "oMath"
  )

  private val ParagraphOrder = Vector(
    "pStyle",
    "keepNext",
    "keepLines",
    "pageBreakBefore",
    "framePr",
    "widowControl",
    "numPr",
    "suppressLineNumbers",
    "pBdr",
    "shd",
    "tabs",
    "suppressAutoHyphens",
    "kinsoku",
    "wordWrap",
    "overflowPunct",
    "topLinePunct",
    "autoSpaceDE",
    "autoSpaceDN",
    "bidi",
    "adjustRightInd",
    "snapToGrid",
    "spacing",
    "ind",
    "contextualSpacing",
    "mirrorIndents",
    "suppressOverlap",
    "jc",
    "textDirection",
    "textAlignment",
    "textboxTightWrap",
    "outlineLvl",
    "divId",
    "cnfStyle",
    "rPr",
    "sectPr",
    "pPrChange"
  )

  val RunKinds: List[String]       = List("b", "i", "u", "rFonts", "sz", "color")
  val ParagraphKinds: List[String] = List("pStyle", "framePr", "jc")

  /** Properties in schema order; names the schema does not list (extensions) keep their order, after the rest. */
  def inRunOrder[A](entries: List[(String, A)]): List[A]       = ordered(entries, RunOrder)
  def inParagraphOrder[A](entries: List[(String, A)]): List[A] = ordered(entries, ParagraphOrder)

  private def ordered[A](entries: List[(String, A)], order: Vector[String]): List[A] =
    entries
      .sortBy((name, _) =>
        order.indexOf(name) match
          case -1    => Int.MaxValue
          case index => index
      )
      .map(_._2)

  /** The model's value for a run property, in the form [[RawProperty.modelled]] records it. */
  def runValue(kind: String, style: RichTextStyle): Option[String] =
    kind match
      case "b"      => Option.when(style.marks.contains(InlineMark.Bold))("on")
      case "i"      => Option.when(style.marks.contains(InlineMark.Italic))("on")
      case "u"      => Option.when(style.marks.contains(InlineMark.Underline))("on")
      case "rFonts" => style.fontFamily
      case "sz"     => style.fontSize.map(_.toString)
      case "color"  => style.color
      case _        => None

  def runXml(kind: String, value: String): String =
    kind match
      case "b" => "<w:b/>"
      case "i" => "<w:i/>"
      case "u" => """<w:u w:val="single"/>"""
      case "rFonts" =>
        s"""<w:rFonts w:ascii="${XmlDom.escapeAttribute(value)}" w:hAnsi="${XmlDom.escapeAttribute(value)}"/>"""
      case "sz"    => s"""<w:sz w:val="${(value.toFloat * 2).round}"/>"""
      case "color" => s"""<w:color w:val="${XmlDom.escapeAttribute(value.stripPrefix("#"))}"/>"""
      case _       => ""

  /** The model's value for a paragraph property. Alignment `Left` is the default and is not written. */
  def paragraphValue(kind: String, paragraph: RichTextParagraph): Option[String] =
    (kind, paragraph.role) match
      case ("pStyle", ParagraphRole.Heading(level))  => Some(s"Heading${level.max(1)}")
      case ("framePr", ParagraphRole.DropCap(lines)) => Some(lines.max(1).toString)
      case ("jc", _) => Option.when(paragraph.alignment != ParagraphAlignment.Left)(alignmentValue(paragraph.alignment))
      case _         => None

  def paragraphXml(kind: String, value: String): String =
    kind match
      case "pStyle" => s"""<w:pStyle w:val="$value"/>"""
      case "framePr" =>
        s"""<w:framePr w:dropCap="drop" w:lines="$value" w:wrap="around" w:vAnchor="text" w:hAnchor="text"/>"""
      case "jc" => s"""<w:jc w:val="$value"/>"""
      case _    => ""

  private def alignmentValue(alignment: ParagraphAlignment): String =
    alignment match
      case ParagraphAlignment.Left    => "left"
      case ParagraphAlignment.Center  => "center"
      case ParagraphAlignment.Right   => "right"
      case ParagraphAlignment.Justify => "both"

  /** The run properties of `style`: what the model says, except where the source element still says the same thing (and
    * may say more, like an East Asian font), plus every property the model does not hold.
    */
  def runPropertiesXml(style: RichTextStyle, native: Boolean): String =
    val extras = if native then style.extras else Nil
    val modelled = RunKinds.flatMap { kind =>
      val current = runValue(kind, style)
      val kept    = extras.find(extra => extra.name == kind && extra.modelled == current)
      kept.map(_.raw).orElse(current.map(runXml(kind, _))).map(kind -> _)
    }
    val unmodelled =
      extras
        .filterNot(extra => RunKinds.contains(extra.name) || RawProperty.isNote(extra))
        .map(extra => extra.name -> extra.raw)
    inRunOrder(modelled ++ unmodelled) match
      case Nil        => ""
      case properties => s"<w:rPr>${properties.mkString}</w:rPr>"

  def paragraphPropertiesXml(paragraph: RichTextParagraph, native: Boolean): String =
    val originals = paragraph.source.filter(_ => native).fold(List.empty[RawProperty])(_.properties)
    val modelled = ParagraphKinds.flatMap { kind =>
      val current = paragraphValue(kind, paragraph)
      val kept    = originals.find(property => property.name == kind && property.modelled == current)
      kept.map(_.raw).orElse(current.map(paragraphXml(kind, _))).map(kind -> _)
    }
    val unmodelled = originals.filterNot(property => ParagraphKinds.contains(property.name)).map(p => p.name -> p.raw)
    inParagraphOrder(modelled ++ unmodelled) match
      case Nil        => ""
      case properties => s"<w:pPr>${properties.mkString}</w:pPr>"
