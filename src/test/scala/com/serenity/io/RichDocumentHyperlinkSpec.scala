package com.serenity.io

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.models.BufferId
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A DOCX whose only rich content is hyperlinks counts as lossless, so saving it in place must keep every URL. */
class RichDocumentHyperlinkSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val address = "https://example.com/guide?a=1&b=2"

  private val documentXml =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<w:document
      |    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
      |    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
      |  <w:body>
      |    <w:p>
      |      <w:r><w:t xml:space="preserve">Read </w:t></w:r>
      |      <w:hyperlink r:id="rId7"><w:r><w:t>the guide</w:t></w:r></w:hyperlink>
      |      <w:r><w:t>.</w:t></w:r>
      |    </w:p>
      |  </w:body>
      |</w:document>""".stripMargin

  private val relationshipsXml =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
      |  <Relationship Id="rId7" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"
      |      Target="https://example.com/guide?a=1&amp;b=2" TargetMode="External"/>
      |</Relationships>""".stripMargin

  private val RelationshipPattern =
    """<Relationship\s+Id="([^"]+)"\s+Type="[^"]+/hyperlink"\s+Target="([^"]+)"\s+TargetMode="External"\s*/>""".r

  "Saving an opened DOCX in place" should "keep each hyperlink's URL and its relationship" in {
    val path    = docxFile("word/document.xml" -> documentXml, "word/_rels/document.xml.rels" -> relationshipsXml)
    val manager = FileManager()
    val opened  = manager.loadFile(path, BufferId(0)).unsafeRunSync()

    val _ = manager.saveBuffer(opened).unsafeRunSync()

    val saved         = Files.readAllBytes(path)
    val relationships = RelationshipPattern.findAllMatchIn(entryText(saved, "word/_rels/document.xml.rels")).toList
    relationships.map(_.group(2)) shouldBe List("https://example.com/guide?a=1&amp;b=2")
    val relationshipId = relationships.headOption.value.group(1)
    entryText(saved, "word/document.xml") should include regex
      s"""<w:hyperlink r:id="$relationshipId">\\s*<w:r><w:t[^>]*>the guide</w:t></w:r>\\s*</w:hyperlink>"""
  }

  private def docxFile(entries: (String, String)*): Path =
    val path   = Files.createTempDirectory("serenity-hyperlink").resolve("links.docx")
    val output = java.io.ByteArrayOutputStream()
    val zip    = ZipOutputStream(output)
    try
      entries.foreach { (name, content) =>
        zip.putNextEntry(ZipEntry(name))
        zip.write(content.getBytes(StandardCharsets.UTF_8))
        zip.closeEntry()
      }
    finally zip.close()
    Files.write(path, output.toByteArray)

  private def entryText(bytes: Array[Byte], name: String): String =
    val input = ZipInputStream(java.io.ByteArrayInputStream(bytes))
    try
      Iterator
        .continually(input.getNextEntry)
        .takeWhile(_ != null)
        .find(_.getName == name)
        .map(_ => String(input.readAllBytes(), StandardCharsets.UTF_8))
        .getOrElse(fail(s"Missing zip entry: $name"))
    finally input.close()

end RichDocumentHyperlinkSpec
