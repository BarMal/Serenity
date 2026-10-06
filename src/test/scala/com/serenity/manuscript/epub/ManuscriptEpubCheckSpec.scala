package com.serenity.manuscript.epub

import java.nio.file.Files
import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import com.adobe.epubcheck.api.{EPUBLocation, EpubCheck}
import com.adobe.epubcheck.messages.{Message, Severity}
import com.adobe.epubcheck.util.DefaultReportImpl
import com.serenity.manuscript.Manuscript
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The W3C's own conformance checker, run on what the writer produces. Anything it rates warning or worse fails. */
class ManuscriptEpubCheckSpec extends AnyFlatSpec with Matchers:

  final private class RecordingReport extends DefaultReportImpl("manuscript.epub"):
    private val problems = ConcurrentLinkedQueue[String]()

    override def message(message: Message, location: EPUBLocation, args: Object*): Unit =
      val severity = message.getSeverity
      if severity == Severity.FATAL || severity == Severity.ERROR || severity == Severity.WARNING then
        problems.add(
          s"$severity ${message.getID}: ${message.getMessage(args*)} at ${location.getPath}:${location.getLine}"
        )
      super.message(message, location, args*)

    def recorded: List[String] = problems.asScala.toList

  private def problemsIn(manuscript: Manuscript): (List[String], Int, Int, Int) =
    val file = Files.createTempFile("manuscript", ".epub")
    try
      Files.write(file, ManuscriptEpubWriter.write(manuscript, ManuscriptEpubFixture.modified))
      val report = RecordingReport()
      EpubCheck(file.toFile, report).doValidate()
      (report.recorded, report.getFatalErrorCount, report.getErrorCount, report.getWarningCount)
    finally Files.deleteIfExists(file)

  "EPUBCheck" should "report no errors and no warnings for a manuscript with front matter, scene breaks and quotes" in {
    problemsIn(ManuscriptEpubFixture.plain) shouldBe (Nil, 0, 0, 0)
  }

  it should "report no errors and no warnings for parts, awkward characters, preformatted text and an identifier" in {
    problemsIn(ManuscriptEpubFixture.withParts) shouldBe (Nil, 0, 0, 0)
  }

  it should "report no errors and no warnings for a book whose navigation is in French" in {
    problemsIn(ManuscriptEpubFixture.french) shouldBe (Nil, 0, 0, 0)
  }

  it should "report no errors and no warnings for a book whose navigation labels come from manuscript.conf" in {
    problemsIn(ManuscriptEpubFixture.labelled) shouldBe (Nil, 0, 0, 0)
  }

  it should "report no errors and no warnings for a book written with raw HTML" in {
    problemsIn(ManuscriptEpubFixture.withHtml) shouldBe (Nil, 0, 0, 0)
  }

  private def untitled(conf: String): Manuscript =
    ManuscriptEpubFixture.headingless(conf).fold(error => sys.error(error.message), identity)

  it should "report no errors and no warnings for French document titles" in {
    problemsIn(untitled("language = \"fr\"")) shouldBe (Nil, 0, 0, 0)
  }

  it should "report no errors and no warnings for document titles overridden in manuscript.conf" in {
    problemsIn(
      untitled("labels.title-page = \"A & B\"\nlabels.dedication = \"<x>\"\nlabels.chapter = \"No. {n}\"")
    ) shouldBe
      (Nil, 0, 0, 0)
  }
