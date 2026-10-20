package com.serenity.richtext

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Failures of the rich-text codecs are values: nothing below `readBytes`/`write`/`encode` throws (#1689). */
class RichTextCodecFailureSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val text = RichTextDocument(List(RichTextParagraph.plain("hello")))

  private def withCorruptRelationships(document: RichTextDocument): RichTextDocument =
    val imported = DocxDocumentCodec.readBytesWithFidelity(DocxDocumentCodec.writeBytes(document)).value
    val source   = imported.document.source.getOrElse(fail("a read DOCX remembers its source"))
    imported.document.withSource(Some(source.copy(relationships = Some("<not xml"))))

  "DocxDocumentCodec.write" should "fail the returned IO, not throw while building it, when the package cannot be encoded" in {
    val path = TestTemp.file("corrupt-rels", ".docx")
    try
      val action = DocxDocumentCodec.write(withCorruptRelationships(text), path)

      action.attempt.unsafeRunSync().left.value shouldBe a[RichTextCodecException]
    finally Files.deleteIfExists(path)
  }

  it should "throw a RichTextCodecException, the one thrown type, from writeBytes" in {
    an[RichTextCodecException] should be thrownBy DocxDocumentCodec.writeBytes(withCorruptRelationships(text))
  }

  "DocxDocumentCodec.encode" should "return the same bytes writeBytes does" in {
    DocxDocumentCodec.encode(text).value shouldBe DocxDocumentCodec.writeBytes(text)
  }

  it should "return a Left naming the codec failure instead of throwing" in {
    val error = DocxDocumentCodec.encode(withCorruptRelationships(text)).left.value

    error.getMessage should include("DOCX document could not be decoded")
  }

  "OdtDocumentCodec.encode" should "return the same bytes writeBytes does" in {
    OdtDocumentCodec.encode(text).value shouldBe OdtDocumentCodec.writeBytes(text)
  }

  "PackageRewriter" should "return a Left when the source archive is truncated mid-entry" in {
    val archive   = GoldenFixtures.zip(List("a.xml" -> Array.fill(4096)('x'.toByte)), stored = Set.empty)
    val truncated = archive.take(60)

    val error = PackageRewriter.rewrite(truncated, "TEST", Map.empty, Nil).left.value

    error.getMessage shouldBe "TEST archive could not be rewritten"
  }

  "RichTextArchive.read" should "return a Left when a wanted entry is over the size limit" in {
    val oversized = GoldenFixtures.zip(
      List("word/document.xml" -> Array.fill(RichTextArchive.MaxXmlEntryBytes + 1)(0.toByte)),
      stored = Set.empty
    )

    val error = RichTextArchive.read(oversized, "DOCX", Set("word/document.xml")).left.value

    error.getMessage should include("word/document.xml is too large")
  }

  "RichTextArchive.readFile" should "fail the IO when the file does not exist" in {
    val missing = TestTemp.directory("missing-docx").resolve("none.docx")

    RichTextArchive.readFile(missing, "DOCX").attempt.unsafeRunSync().isLeft shouldBe true
  }

  "RichTextXmlParser" should "return a Left with the decode message for malformed XML" in {
    val error = RichTextXmlParser.parse("<a>".getBytes("UTF-8"), "DOCX").left.value

    error.getMessage shouldBe "DOCX document could not be decoded"
  }

  it should "return a Left for a document type declaration" in {
    val xml = """<?xml version="1.0"?><!DOCTYPE a [<!ENTITY e "x">]><a>&e;</a>"""

    RichTextXmlParser.parse(xml.getBytes("UTF-8"), "ODT").left.value.getMessage shouldBe
      "ODT document could not be decoded"
  }

  it should "parse well-formed XML" in {
    RichTextXmlParser.parse("<a><b/></a>".getBytes("UTF-8"), "ODT").value.getDocumentElement.getLocalName shouldBe "a"
  }
