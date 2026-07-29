package com.serenity.richtext

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import cats.effect.unsafe.implicits.global
import com.sun.net.httpserver.HttpServer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OdtDocumentCodecSpec extends AnyFlatSpec with Matchers:

  "OdtDocumentCodec" should "read inline marks and paragraph alignment from ODT" in {
    val contentXml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<office:document-content
        |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
        |    xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
        |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
        |    xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0">
        |  <office:automatic-styles>
        |    <style:style style:name="Pcenter" style:family="paragraph">
        |      <style:paragraph-properties fo:text-align="center"/>
        |    </style:style>
        |    <style:style style:name="Tbold" style:family="text">
        |      <style:text-properties fo:font-weight="bold"/>
        |    </style:style>
        |    <style:style style:name="Titalic" style:family="text">
        |      <style:text-properties fo:font-style="italic"/>
        |    </style:style>
        |    <style:style style:name="Tunder" style:family="text">
        |      <style:text-properties style:text-underline-style="solid"/>
        |    </style:style>
        |  </office:automatic-styles>
        |  <office:body>
        |    <office:text>
        |      <text:p text:style-name="Pcenter">plain <text:span text:style-name="Tbold">bold</text:span> <text:span text:style-name="Titalic">italic</text:span> <text:span text:style-name="Tunder">under</text:span></text:p>
        |    </office:text>
        |  </office:body>
        |</office:document-content>""".stripMargin

    val document  = OdtDocumentCodec.readBytes(odtBytes(contentXml))
    val paragraph = singleParagraph(document)

    paragraph.alignment shouldBe ParagraphAlignment.Center
    paragraph.plainText shouldBe "plain bold italic under"
    marksForText(paragraph, "bold") should contain(InlineMark.Bold)
    marksForText(paragraph, "italic") should contain(InlineMark.Italic)
    marksForText(paragraph, "under") should contain(InlineMark.Underline)
  }

  it should "write native rich text documents as readable ODT" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("Hello ", RichTextStyle.empty),
            RichTextRun("world", RichTextStyle(marks = Set(InlineMark.Bold, InlineMark.Underline)))
          ),
          alignment = ParagraphAlignment.Right
        )
      )
    )

    val decoded   = OdtDocumentCodec.readBytes(OdtDocumentCodec.writeBytes(source))
    val paragraph = singleParagraph(decoded)

    paragraph.alignment shouldBe ParagraphAlignment.Right
    paragraph.plainText shouldBe "Hello world"
    marksForText(paragraph, "world") should contain allOf (InlineMark.Bold, InlineMark.Underline)
  }

  it should "read and write heading paragraph roles" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph.plain("Chapter One", role = ParagraphRole.Heading(2)),
        RichTextParagraph.plain("Body")
      )
    )

    val decoded = OdtDocumentCodec.readBytes(OdtDocumentCodec.writeBytes(source))

    decoded.paragraphs.map(_.plainText) shouldBe List("Chapter One", "Body")
    decoded.paragraphs.map(_.role) shouldBe List(ParagraphRole.Heading(2), ParagraphRole.Body)
  }

  it should "read existing ODT heading elements as rich text heading roles" in {
    val contentXml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<office:document-content
        |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
        |    xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
        |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
        |    xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0">
        |  <office:automatic-styles/>
        |  <office:body>
        |    <office:text>
        |      <text:h text:outline-level="3">Scene Three</text:h>
        |      <text:p>Body</text:p>
        |    </office:text>
        |  </office:body>
        |</office:document-content>""".stripMargin

    val document = OdtDocumentCodec.readBytes(odtBytes(contentXml))

    document.paragraphs.map(_.plainText) shouldBe List("Scene Three", "Body")
    document.paragraphs.map(_.role) shouldBe List(ParagraphRole.Heading(3), ParagraphRole.Body)
  }

  it should "preserve explicit font size, family, and colour metadata" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(
          List(
            RichTextRun(
              "styled",
              RichTextStyle(fontFamily = Some("Serif"), fontSize = Some(18.0f), color = Some("#336699"))
            )
          )
        )
      )
    )

    val decodedStyle = singleParagraph(OdtDocumentCodec.readBytes(OdtDocumentCodec.writeBytes(source))).runs
      .find(_.text.contains("styled"))
      .map(_.style)

    decodedStyle.flatMap(_.fontFamily) shouldBe Some("Serif")
    decodedStyle.flatMap(_.fontSize).map(_.round) shouldBe Some(18)
    decodedStyle.flatMap(_.color) shouldBe Some("#336699")
  }

  it should "write tabs and line breaks as native ODT text elements" in {
    val source = RichTextDocument.oneParagraph("alpha\tbeta\ngamma")

    val bytes      = OdtDocumentCodec.writeBytes(source)
    val contentXml = zipEntryText(bytes, "content.xml")
    val decoded    = OdtDocumentCodec.readBytes(bytes)

    contentXml should include("<text:tab/>")
    contentXml should include("<text:line-break/>")
    singleParagraph(decoded).plainText shouldBe "alpha\tbeta\ngamma"
  }

  it should "write repeated spaces as native ODT spacing elements" in {
    val source = RichTextDocument.oneParagraph("alpha  beta")

    val bytes      = OdtDocumentCodec.writeBytes(source)
    val contentXml = zipEntryText(bytes, "content.xml")

    contentXml should include("<text:s/>")
    singleParagraph(OdtDocumentCodec.readBytes(bytes)).plainText shouldBe "alpha  beta"
  }

  it should "report unsupported ODT structures before a lossy save" in {
    val xml = fixture("odt-unsupported-table.xml")

    val imported = OdtDocumentCodec.readBytesWithFidelity(odtBytes(xml))

    imported.document.plainText shouldBe "kept text"
    imported.fidelity.isLossless shouldBe false
    imported.fidelity.unsupportedElements should contain("table")
  }

  it should "read and write ODT files through IO" in {
    val path   = Files.createTempFile("serenity-rich-text", ".odt")
    val source = RichTextDocument.oneParagraph("Saved text")

    try
      OdtDocumentCodec.write(source, path).unsafeRunSync()

      val loaded = OdtDocumentCodec.read(path).unsafeRunSync()

      loaded.plainText shouldBe "Saved text"
    finally Files.deleteIfExists(path)
  }

  it should "fail safely when the ODT content entry is missing" in {
    val error = the[RichTextCodecException] thrownBy OdtDocumentCodec.readBytes(emptyZipBytes())

    error.getMessage should include("missing content.xml")
  }

  it should "fail safely when the ODT content entry is oversized" in {
    val bytes = odtRawBytes("content.xml", Array.fill(RichTextArchive.MaxXmlEntryBytes + 1)(0.toByte))

    val error = the[RichTextCodecException] thrownBy OdtDocumentCodec.readBytes(bytes)

    error.getMessage should include("content.xml is too large")
  }

  it should "wrap malformed ODT XML in a codec exception" in {
    val error = the[RichTextCodecException] thrownBy OdtDocumentCodec.readBytes(odtBytes("<office:document-content>"))

    error.getMessage should include("ODT document could not be decoded")
  }

  it should "reject ODT XML with external entities" in
    withHttpRequestCounter { (resourceUrl, requests) =>
      val xml =
        s"""<?xml version="1.0" encoding="UTF-8"?>
           |<!DOCTYPE office:document-content [<!ENTITY external SYSTEM "$resourceUrl">]>
           |<office:document-content
           |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
           |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">
           |  <office:body><office:text><text:p>&external;</text:p></office:text></office:body>
           |</office:document-content>""".stripMargin

      val error = the[RichTextCodecException] thrownBy OdtDocumentCodec.readBytes(odtBytes(xml))

      error.getMessage should include("ODT document could not be decoded")
      requests.get() shouldBe 0
    }

  it should "reject ODT XML with an external DTD without requesting it" in
    withHttpRequestCounter { (resourceUrl, requests) =>
      val xml =
        s"""<?xml version="1.0" encoding="UTF-8"?>
           |<!DOCTYPE office:document-content SYSTEM "$resourceUrl">
           |<office:document-content
           |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
           |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">
           |  <office:body><office:text><text:p>safe</text:p></office:text></office:body>
           |</office:document-content>""".stripMargin

      val error = the[RichTextCodecException] thrownBy OdtDocumentCodec.readBytes(odtBytes(xml))

      error.getMessage should include("ODT document could not be decoded")
      requests.get() shouldBe 0
    }

  it should "not request an external ODT schema hint" in
    withHttpRequestCounter { (resourceUrl, requests) =>
      val xml =
        s"""<?xml version="1.0" encoding="UTF-8"?>
           |<office:document-content
           |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
           |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
           |    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           |    xsi:schemaLocation="urn:oasis:names:tc:opendocument:xmlns:office:1.0 $resourceUrl">
           |  <office:body><office:text><text:p>safe</text:p></office:text></office:body>
           |</office:document-content>""".stripMargin

      OdtDocumentCodec.readBytes(odtBytes(xml)).plainText shouldBe "safe"
      requests.get() shouldBe 0
    }

  it should "reject ODT XML with entity expansion payloads" in {
    val xml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<!DOCTYPE office:document-content [
        |  <!ENTITY a "0123456789">
        |  <!ENTITY b "&a;&a;&a;&a;&a;&a;&a;&a;&a;&a;">
        |  <!ENTITY c "&b;&b;&b;&b;&b;&b;&b;&b;&b;&b;">
        |]>
        |<office:document-content
        |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
        |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">
        |  <office:body><office:text><text:p>&c;</text:p></office:text></office:body>
        |</office:document-content>""".stripMargin

    val error = the[RichTextCodecException] thrownBy OdtDocumentCodec.readBytes(odtBytes(xml))

    error.getMessage should include("ODT document could not be decoded")
  }

  private def odtBytes(contentXml: String): Array[Byte] =
    odtRawBytes("content.xml", contentXml.getBytes(StandardCharsets.UTF_8))

  private def fixture(name: String): String =
    scala.io.Source.fromResource(s"richtext/$name").mkString

  private def emptyZipBytes(): Array[Byte] =
    val output = java.io.ByteArrayOutputStream()
    ZipOutputStream(output).close()
    output.toByteArray

  private def odtRawBytes(entryName: String, content: Array[Byte]): Array[Byte] =
    val output = java.io.ByteArrayOutputStream()
    val zip    = ZipOutputStream(output)
    try
      zip.putNextEntry(ZipEntry(entryName))
      zip.write(content)
      zip.closeEntry()
    finally zip.close()
    output.toByteArray

  private def withHttpRequestCounter(test: (String, AtomicInteger) => Unit): Unit =
    val requests = AtomicInteger(0)
    val server   = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext(
      "/resource",
      exchange =>
        requests.incrementAndGet()
        val body = "<!ELEMENT document ANY>".getBytes(StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(200, body.length.toLong)
        try exchange.getResponseBody.write(body)
        finally exchange.close()
    )
    server.start()
    try test(s"http://127.0.0.1:${server.getAddress.getPort}/resource", requests)
    finally server.stop(0)

  private def zipEntryText(bytes: Array[Byte], name: String): String =
    val input = ZipInputStream(java.io.ByteArrayInputStream(bytes))
    try
      Iterator
        .continually(input.getNextEntry)
        .takeWhile(_ != null)
        .find(_.getName == name)
        .map { _ =>
          val output = java.io.ByteArrayOutputStream()
          input.transferTo(output)
          output.toString(StandardCharsets.UTF_8)
        }
        .getOrElse(fail(s"Missing zip entry: $name"))
    finally input.close()

  private def marksForText(paragraph: RichTextParagraph, text: String): Set[InlineMark] =
    paragraph.runs
      .find(_.text.contains(text))
      .map(_.style.marks)
      .getOrElse(Set.empty)

  private def singleParagraph(document: RichTextDocument): RichTextParagraph =
    document.paragraphs match
      case paragraph :: Nil => paragraph
      case other            => fail(s"Expected one paragraph, got ${other.size}")
