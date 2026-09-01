package com.serenity

import com.serenity.io.{FileType, SaveFormat}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SaveFormatSpec extends AnyFlatSpec with Matchers:

  "SaveFormat.ordered" should "list the five saveable formats in cycling order" in {
    SaveFormat.ordered shouldBe List(
      SaveFormat.Text,
      SaveFormat.Markdown,
      SaveFormat.RichText,
      SaveFormat.OpenDocumentText,
      SaveFormat.WordOpenXml
    )
  }

  "SaveFormat.canonicalExtension" should "give each format its canonical extension" in {
    SaveFormat.Text.canonicalExtension shouldBe ".txt"
    SaveFormat.Markdown.canonicalExtension shouldBe ".md"
    SaveFormat.RichText.canonicalExtension shouldBe ".rtf"
    SaveFormat.OpenDocumentText.canonicalExtension shouldBe ".odt"
    SaveFormat.WordOpenXml.canonicalExtension shouldBe ".docx"
  }

  "SaveFormat.displayName" should "give each format a readable label" in {
    SaveFormat.Text.displayName shouldBe "Text"
    SaveFormat.Markdown.displayName shouldBe "Markdown"
    SaveFormat.RichText.displayName shouldBe "Rich Text"
    SaveFormat.OpenDocumentText.displayName shouldBe "OpenDocument Text"
    SaveFormat.WordOpenXml.displayName shouldBe "Word"
  }

  "SaveFormat.fromFileType" should "map a detected FileType to its matching SaveFormat" in {
    SaveFormat.fromFileType(FileType.Markdown) shouldBe SaveFormat.Markdown
    SaveFormat.fromFileType(FileType.RichText) shouldBe SaveFormat.RichText
    SaveFormat.fromFileType(FileType.OpenDocumentText) shouldBe SaveFormat.OpenDocumentText
    SaveFormat.fromFileType(FileType.WordOpenXmlDocument) shouldBe SaveFormat.WordOpenXml
    SaveFormat.fromFileType(FileType.Text) shouldBe SaveFormat.Text
  }

  it should "fall back to Text for a FileType with no matching SaveFormat" in {
    SaveFormat.fromFileType(FileType.Scala) shouldBe SaveFormat.Text
    SaveFormat.fromFileType(FileType.Unknown) shouldBe SaveFormat.Text
    SaveFormat.fromFileType(FileType.WordDocument) shouldBe SaveFormat.Text
  }
