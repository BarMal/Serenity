package com.serenity.richtext

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import io.circe.parser.parse
import org.apache.poi.xwpf.usermodel.XWPFDocument
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The golden harness (rich-document design section 7): every directory under `richtext/golden` holds a `source`
  * package, `edits.json` and the expected main part after the edits in `expect/`.
  */
class RichTextGoldenSpec extends AnyFlatSpec with Matchers with EitherValues:
  import RichTextTestPackages.{entries, names}

  final private case class Edit(paragraph: Int, find: String, replaceWith: String, touches: String)

  final private case class Golden(directory: Path):
    val name: String        = directory.getFileName.toString
    val isOdt: Boolean      = Files.exists(directory.resolve("source.odt"))
    val source: Array[Byte] = Files.readAllBytes(directory.resolve(if isOdt then "source.odt" else "source.docx"))
    val mainEntry: String   = if isOdt then "content.xml" else "word/document.xml"
    val expectedMain: String =
      Files.readString(directory.resolve(if isOdt then "expect/content.xml" else "expect/document.xml"))

    val edits: List[Edit] =
      parse(Files.readString(directory.resolve("edits.json")))
        .flatMap(_.hcursor.downField("edits").as[List[io.circe.Json]])
        .value
        .map { json =>
          val cursor = json.hcursor
          Edit(
            cursor.get[Int]("paragraph").value,
            cursor.get[String]("find").value,
            cursor.get[String]("replaceWith").value,
            cursor.get[String]("touches").value
          )
        }

    def read(bytes: Array[Byte]): RichTextDocument =
      (if isOdt then OdtDocumentCodec.readBytes(bytes) else DocxDocumentCodec.readBytes(bytes)).value

    def write(document: RichTextDocument): Array[Byte] =
      if isOdt then OdtDocumentCodec.writeBytes(document) else DocxDocumentCodec.writeBytes(document)

    def applyEdits(document: RichTextDocument): RichTextDocument =
      edits.foldLeft(document)((current, edit) =>
        RichTextTestPackages.replaceText(current, edit.paragraph, edit.find, edit.replaceWith)
      )

  private val fixtures: List[Golden] =
    val root = Paths.get(getClass.getResource("/richtext/golden").toURI)
    Files.list(root).iterator().asScala.filter(Files.isDirectory(_)).map(Golden.apply).toList.sortBy(_.name)

  private def mainText(archive: Array[Byte], golden: Golden): String =
    entries(archive).find(_.name == golden.mainEntry).map(_.text).getOrElse(fail(s"missing ${golden.mainEntry}"))

  "The golden fixtures" should "include a Word and a Writer package" in {
    fixtures.map(_.name) should contain allOf ("word-report", "writer-notes")
  }

  fixtures.foreach { golden =>
    s"Golden fixture ${golden.name}" should "round-trip without edits with every entry, its order and its bytes" in {
      val saved = golden.write(golden.read(golden.source))

      names(saved) shouldBe names(golden.source)
      entries(saved).map(entry => (entry.name, entry.bytes, entry.method)) shouldBe
        entries(golden.source).map(entry => (entry.name, entry.bytes, entry.method))
    }

    it should "differ from the source only inside the touched paragraph after the scripted edits" in {
      val saved = golden.write(golden.applyEdits(golden.read(golden.source)))

      names(saved) shouldBe names(golden.source)
      entries(saved).filterNot(_.name == golden.mainEntry).map(entry => entry.name -> entry.bytes) shouldBe
        entries(golden.source).filterNot(_.name == golden.mainEntry).map(entry => entry.name -> entry.bytes)
      val original = mainText(golden.source, golden)
      val changed  = mainText(saved, golden)
      val prefix   = original.zip(changed).takeWhile(_ == _).size
      val suffix   = original.reverse.zip(changed.reverse).takeWhile(_ == _).size
      golden.edits.foreach { edit =>
        val marker     = original.indexOf(edit.touches)
        val sliceStart = original.lastIndexOf(if golden.isOdt then "<text:p" else "<w:p ", marker)
        val sliceEnd   = original.indexOf(if golden.isOdt then "</text:p>" else "</w:p>", marker)
        prefix should be >= sliceStart
        original.length - suffix should be <= sliceEnd + 10
      }
    }

    it should "produce the expected main part after the scripted edits" in {
      mainText(golden.write(golden.applyEdits(golden.read(golden.source))), golden) shouldBe golden.expectedMain
    }

    it should "keep the edited package reading back to the edited text" in {
      val edited = golden.applyEdits(golden.read(golden.source))

      golden.read(golden.write(edited)).exportText shouldBe edited.exportText
    }
  }

  "Every DOCX fixture's output" should "open in Apache POI with the model's paragraph text" in
    fixtures.filterNot(_.isOdt).foreach { golden =>
      val edited = golden.applyEdits(golden.read(golden.source))
      val poi    = XWPFDocument(ByteArrayInputStream(golden.write(edited)))
      try
        poi.getParagraphs.asScala.map(_.getText).toList shouldBe
          edited.paragraphs.filterNot(_.isOpaqueBlock).map(_.exportText)
        poi.getTables.asScala.flatMap(_.getRows.asScala).flatMap(_.getTableCells.asScala).map(_.getText).toList shouldBe
          List("Region", "Revenue", "North", "1,200")
        poi.getAllPictures.asScala.size shouldBe 1
        poi.getComments.length shouldBe 1
      finally poi.close()
    }

  "The committed fixtures" should "match what GoldenFixtures builds" in
    GoldenFixtures.all.foreach { fixture =>
      val golden = fixtures.find(_.name == fixture.name).getOrElse(fail(s"${fixture.name} is not committed"))

      entries(golden.source).map(entry => entry.name -> entry.bytes) shouldBe
        fixture.entries.map((name, bytes) => name -> bytes.toSeq)
      golden.expectedMain shouldBe fixture.expectedMain
      Files.readString(golden.directory.resolve("edits.json")).trim shouldBe fixture.editsJson
    }
