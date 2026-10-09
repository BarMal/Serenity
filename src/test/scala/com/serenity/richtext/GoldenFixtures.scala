package com.serenity.richtext

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Base64
import java.util.zip.{CRC32, ZipEntry, ZipOutputStream}

/** The golden fixtures under `src/test/resources/richtext/golden`, authored for this project so that their licence is
  * the project's own. They are small hand-written packages shaped like what Word and LibreOffice write: a `styles.xml`,
  * a comment, a table, an image, a hyperlink, bookmarks, section properties, unmodelled paragraph and run properties.
  *
  * Regenerate the files with `sbt "Test / runMain com.serenity.richtext.GoldenFixtures"`; `GoldenFixturesSpec` fails
  * when the committed files drift from what this builds.
  */
object GoldenFixtures:
  final case class Fixture(name: String, entries: List[(String, Array[Byte])], editsJson: String, expectedMain: String)

  private val fixedTime = 1_700_000_000_000L

  private val PngPixel: Array[Byte] = Base64.getDecoder.decode(
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  )

  def all: List[Fixture] = List(wordReport, writerNotes)

  def zip(entries: List[(String, Array[Byte])], stored: Set[String] = Set.empty): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output, StandardCharsets.UTF_8)
    try
      entries.foreach { (name, bytes) =>
        val entry = ZipEntry(name)
        entry.setTime(fixedTime)
        if stored.contains(name) then
          val crc = CRC32()
          crc.update(bytes)
          entry.setMethod(ZipEntry.STORED)
          entry.setSize(bytes.length.toLong)
          entry.setCompressedSize(bytes.length.toLong)
          entry.setCrc(crc.getValue)
        zip.putNextEntry(entry)
        zip.write(bytes)
        zip.closeEntry()
      }
    finally zip.close()
    output.toByteArray

  private def utf8(text: String): Array[Byte] = text.getBytes(StandardCharsets.UTF_8)

  private val WordNamespaces =
    """xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" """ +
      """xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" """ +
      """xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" """ +
      """xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" """ +
      """xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" """ +
      """xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" """ +
      """xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" mc:Ignorable="w14""""

  private val sharedParagraphs = List(
    """<w:p w:rsidR="00A10001" w14:paraId="1A2B3C01"><w:pPr><w:pStyle w:val="Heading1"/></w:pPr><w:bookmarkStart w:id="0" w:name="_Toc1000"/><w:r><w:t>Quarterly report</w:t></w:r><w:bookmarkEnd w:id="0"/></w:p>""",
    """<w:p w:rsidR="00A10002" w14:paraId="1A2B3C02"><w:pPr><w:keepNext/><w:spacing w:after="120"/><w:jc w:val="both"/></w:pPr><w:r><w:rPr><w:lang w:val="en-GB"/></w:rPr><w:t xml:space="preserve">This is the first body paragraph with a </w:t></w:r><w:commentRangeStart w:id="1"/><w:r><w:rPr><w:b/></w:rPr><w:t>commented phrase</w:t></w:r><w:commentRangeEnd w:id="1"/><w:r><w:rPr><w:rStyle w:val="CommentReference"/></w:rPr><w:commentReference w:id="1"/></w:r><w:r><w:t xml:space="preserve"> and more text.</w:t></w:r></w:p>""",
    """<w:p w:rsidR="00A10003" w14:paraId="1A2B3C03"><w:r><w:t xml:space="preserve">Visit </w:t></w:r><w:hyperlink r:id="rId5" w:history="1"><w:r><w:rPr><w:rStyle w:val="Hyperlink"/></w:rPr><w:t>the project page</w:t></w:r></w:hyperlink><w:r><w:t xml:space="preserve"> for details.</w:t></w:r></w:p>""",
    """<w:tbl><w:tblPr><w:tblStyle w:val="TableGrid"/><w:tblW w:w="0" w:type="auto"/></w:tblPr><w:tblGrid><w:gridCol w:w="4513"/><w:gridCol w:w="4513"/></w:tblGrid><w:tr><w:tc><w:tcPr><w:tcW w:w="4513" w:type="dxa"/></w:tcPr><w:p w14:paraId="1A2B3C11"><w:r><w:t>Region</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcW w:w="4513" w:type="dxa"/></w:tcPr><w:p w14:paraId="1A2B3C12"><w:r><w:t>Revenue</w:t></w:r></w:p></w:tc></w:tr><w:tr><w:tc><w:tcPr><w:tcW w:w="4513" w:type="dxa"/></w:tcPr><w:p w14:paraId="1A2B3C13"><w:r><w:t>North</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcW w:w="4513" w:type="dxa"/></w:tcPr><w:p w14:paraId="1A2B3C14"><w:r><w:t>1,200</w:t></w:r></w:p></w:tc></w:tr></w:tbl>""",
    """<w:p w:rsidR="00A10004" w14:paraId="1A2B3C04"><w:r><w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0"><wp:extent cx="190500" cy="190500"/><wp:docPr id="1" name="Picture 1" descr="A single pixel"/><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic><pic:nvPicPr><pic:cNvPr id="0" name="pixel.png"/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill><a:blip r:embed="rId4"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill><pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="190500" cy="190500"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p>""",
    """<w:p w:rsidR="00A10005" w14:paraId="1A2B3C05"><w:r><w:t>Closing words.</w:t></w:r></w:p>""",
    """<w:sectPr w:rsidR="00A10001"><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:header="708" w:footer="708" w:gutter="0"/></w:sectPr>"""
  )

  private def wordDocument(paragraphs: List[String]): String =
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n<w:document $WordNamespaces><w:body>${paragraphs.mkString}</w:body></w:document>"""

  private val editedFirstParagraph =
    """<w:p w:rsidR="00A10002" w14:paraId="1A2B3C02"><w:pPr><w:keepNext/><w:spacing w:after="120"/><w:jc w:val="both"/></w:pPr><w:r><w:rPr><w:lang w:val="en-GB"/></w:rPr><w:t xml:space="preserve">This is the edited body paragraph with a </w:t></w:r><w:commentRangeStart w:id="1"/><w:r><w:rPr><w:b/></w:rPr><w:t xml:space="preserve">commented phrase</w:t></w:r><w:commentRangeEnd w:id="1"/><w:r><w:rPr><w:rStyle w:val="CommentReference"/></w:rPr><w:commentReference w:id="1"/></w:r><w:r><w:t xml:space="preserve"> and more text.</w:t></w:r></w:p>"""

  val wordReport: Fixture =
    val entries = List(
      "[Content_Types].xml" -> utf8(
        """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
          |<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Default Extension="png" ContentType="image/png"/><Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/><Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/><Override PartName="/word/settings.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"/><Override PartName="/word/comments.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"/><Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/></Types>""".stripMargin
      ),
      "_rels/.rels" -> utf8(
        """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
          |<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/></Relationships>""".stripMargin
      ),
      "word/document.xml" -> utf8(wordDocument(sharedParagraphs)),
      "word/_rels/document.xml.rels" -> utf8(
        """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
          |<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings" Target="settings.xml"/><Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments" Target="comments.xml"/><Relationship Id="rId4" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/pixel.png"/><Relationship Id="rId5" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink" Target="https://example.com/project?tab=1&amp;view=full" TargetMode="External"/></Relationships>""".stripMargin
      ),
      "word/styles.xml" -> utf8(
        """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
          |<w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:eastAsia="Calibri" w:cs="Calibri"/><w:sz w:val="22"/><w:lang w:val="en-GB"/></w:rPr></w:rPrDefault></w:docDefaults><w:style w:type="paragraph" w:default="1" w:styleId="Normal"><w:name w:val="Normal"/><w:qFormat/></w:style><w:style w:type="paragraph" w:styleId="Heading1"><w:name w:val="heading 1"/><w:basedOn w:val="Normal"/><w:next w:val="Normal"/><w:qFormat/><w:pPr><w:keepNext/><w:outlineLvl w:val="0"/></w:pPr><w:rPr><w:b/><w:sz w:val="32"/></w:rPr></w:style><w:style w:type="character" w:styleId="Hyperlink"><w:name w:val="Hyperlink"/><w:rPr><w:color w:val="0563C1"/><w:u w:val="single"/></w:rPr></w:style><w:style w:type="character" w:styleId="CommentReference"><w:name w:val="annotation reference"/><w:rPr><w:sz w:val="16"/></w:rPr></w:style></w:styles>""".stripMargin
      ),
      "word/settings.xml" -> utf8(
        """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
          |<w:settings xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:zoom w:percent="100"/><w:defaultTabStop w:val="720"/></w:settings>""".stripMargin
      ),
      "word/comments.xml" -> utf8(
        """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
          |<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:comment w:id="1" w:author="Reviewer" w:date="2026-10-05T09:00:00Z" w:initials="R"><w:p><w:r><w:t>Please check this phrase.</w:t></w:r></w:p></w:comment></w:comments>""".stripMargin
      ),
      "word/media/pixel.png" -> PngPixel,
      "docProps/core.xml" -> utf8(
        """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
          |<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/"><dc:title>Quarterly report</dc:title><dc:creator>Serenity tests</dc:creator></cp:coreProperties>""".stripMargin
      )
    )
    Fixture(
      "word-report",
      entries,
      """{"edits":[{"paragraph":1,"find":"first","replaceWith":"edited","touches":"1A2B3C02"}]}""",
      wordDocument(sharedParagraphs.updated(1, editedFirstParagraph))
    )

  private val OdfNamespaces =
    """xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" """ +
      """xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" """ +
      """xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" """ +
      """xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" """ +
      """xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" """ +
      """xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" """ +
      """xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dc="http://purl.org/dc/elements/1.1/" office:version="1.3""""

  private val odfAutomaticStyles =
    """<office:automatic-styles><style:style style:name="P1" style:family="paragraph" style:parent-style-name="Standard"><style:paragraph-properties fo:text-align="center"/></style:style><style:style style:name="T1" style:family="text"><style:text-properties fo:font-weight="bold"/></style:style></office:automatic-styles>"""

  private val odfParagraphs = List(
    """<text:h text:style-name="Heading_20_1" text:outline-level="1">Field notes</text:h>""",
    """<text:p text:style-name="P1">A centred line with <text:span text:style-name="T1">bold words</text:span> and a <text:bookmark text:name="here"/>bookmark.</text:p>""",
    """<text:p text:style-name="Standard">See <text:a xlink:type="simple" xlink:href="https://example.org/notes" text:style-name="Internet_20_link">the notes</text:a> online.</text:p>""",
    """<table:table table:name="Table1"><table:table-column table:number-columns-repeated="2"/><table:table-row><table:table-cell office:value-type="string"><text:p>Left</text:p></table:table-cell><table:table-cell office:value-type="string"><text:p>Right</text:p></table:table-cell></table:table-row></table:table>""",
    """<text:p text:style-name="Standard"><draw:frame draw:name="Image1" text:anchor-type="as-char" svg:width="0.2in" svg:height="0.2in" xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"><draw:image xlink:href="Pictures/pixel.png" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad"/></draw:frame></text:p>""",
    """<text:p text:style-name="Standard">Final paragraph.</text:p>"""
  )

  private def odfContent(paragraphs: List[String]): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>\n<office:document-content $OdfNamespaces>$odfAutomaticStyles<office:body><office:text>${paragraphs.mkString}</office:text></office:body></office:document-content>"""

  private val editedOdfParagraph =
    """<text:p text:style-name="Standard">See <text:a xlink:type="simple" xlink:href="https://example.org/notes" text:style-name="Internet_20_link">the notes</text:a> offline.</text:p>"""

  val writerNotes: Fixture =
    val entries = List(
      "mimetype"    -> utf8("application/vnd.oasis.opendocument.text"),
      "content.xml" -> utf8(odfContent(odfParagraphs)),
      "styles.xml" -> utf8(
        s"""<?xml version="1.0" encoding="UTF-8"?>\n<office:document-styles $OdfNamespaces><office:styles><style:style style:name="Standard" style:family="paragraph" style:class="text"/><style:style style:name="Heading_20_1" style:display-name="Heading 1" style:family="paragraph" style:parent-style-name="Standard"><style:text-properties fo:font-size="18pt" fo:font-weight="bold"/></style:style><style:style style:name="Internet_20_link" style:display-name="Internet link" style:family="text"><style:text-properties fo:color="#000080"/></style:style></office:styles></office:document-styles>"""
      ),
      "meta.xml" -> utf8(
        s"""<?xml version="1.0" encoding="UTF-8"?>\n<office:document-meta $OdfNamespaces><office:meta><dc:title>Field notes</dc:title></office:meta></office:document-meta>"""
      ),
      "Pictures/pixel.png" -> PngPixel,
      "META-INF/manifest.xml" -> utf8(
        """<?xml version="1.0" encoding="UTF-8"?>
          |<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0" manifest:version="1.3"><manifest:file-entry manifest:full-path="/" manifest:media-type="application/vnd.oasis.opendocument.text"/><manifest:file-entry manifest:full-path="content.xml" manifest:media-type="text/xml"/><manifest:file-entry manifest:full-path="styles.xml" manifest:media-type="text/xml"/><manifest:file-entry manifest:full-path="meta.xml" manifest:media-type="text/xml"/><manifest:file-entry manifest:full-path="Pictures/pixel.png" manifest:media-type="image/png"/></manifest:manifest>""".stripMargin
      )
    )
    Fixture(
      "writer-notes",
      entries,
      """{"edits":[{"paragraph":2,"find":"online","replaceWith":"offline","touches":"the notes"}]}""",
      odfContent(odfParagraphs.updated(2, editedOdfParagraph))
    )

  def write(directory: Path): Unit =
    all.foreach { fixture =>
      val target = directory.resolve(fixture.name)
      val isOdt  = fixture.entries.exists(_._1 == "mimetype")
      Files.createDirectories(target.resolve("expect"))
      Files.write(target.resolve(if isOdt then "source.odt" else "source.docx"), zip(fixture.entries, Set("mimetype")))
      Files.write(target.resolve("edits.json"), utf8(fixture.editsJson + "\n"))
      Files.write(
        target.resolve(if isOdt then "expect/content.xml" else "expect/document.xml"),
        utf8(fixture.expectedMain)
      )
    }

  def main(args: Array[String]): Unit =
    write(Paths.get("src/test/resources/richtext/golden"))
