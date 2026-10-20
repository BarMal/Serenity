package com.serenity.io

import com.serenity.lsp.config.{FileExtension, LanguageId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The UI's file classification and the language the editor and LSP use for the same file come from one table (#1897):
  * an extension cannot be a Kotlin file to one and nothing to the other.
  */
class FileTypeLanguageAgreementSpec extends AnyFlatSpec with Matchers:

  "FileType.fromExtension" should "classify the extensions the two tables used to disagree on" in {
    FileType.fromExtension("tsx") shouldBe FileType.TypeScript
    FileType.fromExtension("kt") shouldBe FileType.Kotlin
    FileType.fromExtension("rb") shouldBe FileType.Ruby
    FileType.fromExtension("sbt") shouldBe FileType.Scala
    FileType.fromExtension("h") shouldBe FileType.C
  }

  it should "agree with the language of every extension the language table knows" in
    FileExtension.values.foreach { entry =>
      withClue(s".${entry.ext}: ") {
        FileType.fromExtension(entry.ext).displayName shouldBe entry.languageId.displayName
      }
    }

  it should "keep classifying the extensions only the UI knows" in {
    FileType.fromExtension("txt") shouldBe FileType.Text
    FileType.fromExtension("docx") shouldBe FileType.WordOpenXmlDocument
    FileType.fromExtension("rtf") shouldBe FileType.RichText
    FileType.fromExtension("conf") shouldBe FileType.Config
    FileType.fromExtension("sh") shouldBe FileType.Shell
    FileType.fromExtension("xyz") shouldBe FileType.Unknown
  }

  "FileExtension" should "know the extensions the UI table recognised that it lacked" in {
    FileExtension.languageIdFor("mjs") shouldBe Some(LanguageId.JavaScript)
    FileExtension.languageIdFor("cxx") shouldBe Some(LanguageId.Cpp)
    FileExtension.languageIdFor("markdown") shouldBe Some(LanguageId.Markdown)
  }
