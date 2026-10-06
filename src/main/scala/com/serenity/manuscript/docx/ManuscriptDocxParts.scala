package com.serenity.manuscript.docx

import com.serenity.manuscript.{ManuscriptFormat, ManuscriptMeta}

/** The fixed and near-fixed parts of a manuscript DOCX package: everything but `word/document.xml`. */
private[docx] object ManuscriptDocxParts:

  private val WNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

  private val PackageRelationshipsNs = "http://schemas.openxmlformats.org/package/2006/relationships"

  private val OfficeRelationships = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  private val WordprocessingContentType = "application/vnd.openxmlformats-officedocument.wordprocessingml"

  val contentTypes: String =
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
       |<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
       |  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
       |  <Default Extension="xml" ContentType="application/xml"/>
       |  <Override PartName="/word/document.xml" ContentType="$WordprocessingContentType.document.main+xml"/>
       |  <Override PartName="/word/styles.xml" ContentType="$WordprocessingContentType.styles+xml"/>
       |  <Override PartName="/word/settings.xml" ContentType="$WordprocessingContentType.settings+xml"/>
       |  <Override PartName="/word/header1.xml" ContentType="$WordprocessingContentType.header+xml"/>
       |  <Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>
       |</Types>
       |""".stripMargin

  val packageRelationships: String =
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
       |<Relationships xmlns="$PackageRelationshipsNs">
       |  <Relationship Id="rId1" Type="$OfficeRelationships/officeDocument" Target="word/document.xml"/>
       |  <Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>
       |</Relationships>
       |""".stripMargin

  /** `rId3` is the running header the body section's `w:headerReference` names. */
  val documentRelationships: String =
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
       |<Relationships xmlns="$PackageRelationshipsNs">
       |  <Relationship Id="rId1" Type="$OfficeRelationships/styles" Target="styles.xml"/>
       |  <Relationship Id="rId2" Type="$OfficeRelationships/settings" Target="settings.xml"/>
       |  <Relationship Id="rId3" Type="$OfficeRelationships/header" Target="header1.xml"/>
       |</Relationships>
       |""".stripMargin

  def coreProperties(meta: ManuscriptMeta): String =
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
       |<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/">
       |  <dc:title>${escape(meta.title)}</dc:title>
       |  <dc:creator>${escape(meta.author.legal)}</dc:creator>
       |</cp:coreProperties>
       |""".stripMargin

  /** Normal carries the whole format: one face and size, double spacing, a first-line indent, ragged right. The other
    * styles only take the indent away, centre, or tighten the spacing.
    */
  def styles(format: ManuscriptFormat): String =
    val font     = escape(format.fontFamily)
    val halfPts  = format.fontSizePoints * 2
    val spacing  = s"""<w:spacing w:before="0" w:after="0" w:line="${format.lineSpacing}" w:lineRule="auto"/>"""
    val single   = """<w:spacing w:line="240" w:lineRule="auto"/>"""
    val noIndent = """<w:ind w:firstLine="0"/>"""
    val centred  = """<w:jc w:val="center"/>"""
    def style(id: String, name: String, properties: String, extra: String = ""): String =
      s"""  <w:style w:type="paragraph" w:styleId="$id"><w:name w:val="$name"/><w:basedOn w:val="Normal"/>""" +
        s"""<w:next w:val="Normal"/>$extra<w:pPr>$properties</w:pPr></w:style>"""
    val namedStyles = List(
      style("Heading1", "heading 1", s"""<w:keepNext/>$noIndent$centred<w:outlineLvl w:val="0"/>""", "<w:qFormat/>"),
      style("Heading2", "heading 2", s"""<w:keepNext/>$noIndent$centred<w:outlineLvl w:val="1"/>""", "<w:qFormat/>"),
      style("Title", "Title", s"$noIndent$centred", "<w:qFormat/>"),
      style("Byline", "Byline", s"$noIndent$centred"),
      style("ContactBlock", "Contact Block", s"$single$noIndent"),
      style("Centered", "Centered", s"$noIndent$centred"),
      style("SceneBreak", "Scene Break", s"<w:keepNext/>$noIndent$centred"),
      style("BlockQuote", "Block Quote", s"""<w:ind w:left="${format.firstLineIndentTwips}" w:firstLine="0"/>"""),
      style("Preformatted", "Preformatted", noIndent),
      style("Header", "header", s"""$single$noIndent<w:jc w:val="right"/>""")
    ).mkString("\n")
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
       |<w:styles xmlns:w="$WNs">
       |  <w:docDefaults>
       |    <w:rPrDefault><w:rPr><w:rFonts w:ascii="$font" w:hAnsi="$font" w:eastAsia="$font" w:cs="$font"/><w:sz w:val="$halfPts"/><w:szCs w:val="$halfPts"/></w:rPr></w:rPrDefault>
       |    <w:pPrDefault><w:pPr>$spacing</w:pPr></w:pPrDefault>
       |  </w:docDefaults>
       |  <w:style w:type="paragraph" w:default="1" w:styleId="Normal"><w:name w:val="Normal"/><w:qFormat/><w:pPr><w:widowControl/>$spacing<w:ind w:firstLine="${format.firstLineIndentTwips}"/></w:pPr></w:style>
       |$namedStyles
       |</w:styles>
       |""".stripMargin

  def settings(format: ManuscriptFormat): String =
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
       |<w:settings xmlns:w="$WNs">
       |  <w:defaultTabStop w:val="${format.firstLineIndentTwips}"/>
       |  <w:autoHyphenation w:val="false"/>
       |  <w:compat><w:compatSetting w:name="compatibilityMode" w:uri="http://schemas.microsoft.com/office/word" w:val="15"/></w:compat>
       |</w:settings>
       |""".stripMargin

  /** The running header with a live `PAGE` field for `<$p>`. A slash-separated segment left empty (no surname, say) is
    * dropped along with its separator.
    */
  def header(meta: ManuscriptMeta, format: ManuscriptFormat): String =
    val text = format.runningHeader
      .split(" / ", -1)
      .toList
      .map(_.replace("<$surname>", meta.author.surname).replace("<$keyword>", meta.shortTitle).trim)
      .filter(_.nonEmpty)
      .mkString(" / ")
    val pageField =
      """<w:r><w:fldChar w:fldCharType="begin"/></w:r><w:r><w:instrText xml:space="preserve"> PAGE </w:instrText></w:r>""" +
        """<w:r><w:fldChar w:fldCharType="separate"/></w:r><w:r><w:t>1</w:t></w:r><w:r><w:fldChar w:fldCharType="end"/></w:r>"""
    val runs = text
      .split(java.util.regex.Pattern.quote("<$p>"), -1)
      .toList
      .map(segment => if segment.isEmpty then "" else s"<w:r>${textContent(segment)}</w:r>")
      .mkString(pageField)
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
       |<w:hdr xmlns:w="$WNs">
       |  <w:p><w:pPr><w:pStyle w:val="Header"/></w:pPr>$runs</w:p>
       |</w:hdr>
       |""".stripMargin

  /** Run content for `text`: tabs and line breaks become their own elements, everything else `w:t`. */
  def textContent(text: String): String =
    text
      .split("((?<=[\t\n])|(?=[\t\n]))")
      .toList
      .filter(_.nonEmpty)
      .map {
        case "\t"    => "<w:tab/>"
        case "\n"    => "<w:br/>"
        case segment => s"""<w:t xml:space="preserve">${escape(segment)}</w:t>"""
      }
      .mkString

  def escape(text: String): String =
    text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")
