package com.serenity.state.manager

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.io.FileManager
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A bulk replacement of a buffer's text (an LSP formatter's output) carries the rich document onto the new text rather
  * than dropping it, so formatting and read-only blocks survive wherever the text did (#1896).
  */
class BufferContentReplacementRichTextSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(7)
  private val block    = InlineAtom.BlockCharacter.toString

  private def withReport[A](use: (Buffer, Array[Byte], java.nio.file.Path) => A): A =
    val file = TestTemp.file("serenity-replace-source", ".docx")
    try
      val bytes = GoldenFixtures.zip(GoldenFixtures.wordReport.entries)
      Files.write(file, bytes)
      use(new FileManager().loadFile(file, bufferId).unsafeRunSync(), bytes, file)
    finally Files.deleteIfExists(file)

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(buffer.id -> buffer), bufferOrder = List(buffer.id))
    )

  private def replaced(buffer: Buffer, content: String): Buffer =
    EditorTransitions
      .bufferContentReplaced(stateWith(buffer), buffer.id, content)
      .value
      .buffer

  private def paragraphsOf(buffer: Buffer): List[RichTextParagraph] =
    buffer.richText.richTextDocument.value.paragraphs

  "A replacement that only changes whitespace in one paragraph" should "keep the marks, the block and the package everywhere" in withReport {
    (buffer, _, _) =>
      val before = paragraphsOf(buffer)
      val text   = buffer.document.content.collect()

      val after = replaced(buffer, text.replace("body paragraph", "body  paragraph"))

      after.richTextInSync shouldBe true
      val paragraphs = paragraphsOf(after)
      paragraphs.map(_.plainText).mkString("\n") shouldBe after.document.content.collect()
      paragraphs.zipWithIndex.filter((paragraph, index) => paragraph != before(index)).map(_._2) shouldBe List(1)
      paragraphs(1).runs.exists(run =>
        run.text == "commented phrase" && run.style.marks.contains(InlineMark.Bold)
      ) shouldBe true
      paragraphs(3).isOpaqueBlock shouldBe true
      after.richText.richTextDocument.value.source shouldBe buffer.richText.richTextDocument.value.source
      FidelityReport.forSave(after.richText.richTextDocument.value, SaveTarget.Docx).summary should include(
        "1 table preserved read-only"
      )
  }

  "A replacement that adds a line" should "keep the paragraphs on either side, the block among them" in withReport {
    (buffer, _, _) =>
      val before = paragraphsOf(buffer)

      val after = replaced(buffer, "Intro\n" + buffer.document.content.collect())

      val paragraphs = paragraphsOf(after)
      paragraphs.map(_.plainText).mkString("\n") shouldBe after.document.content.collect()
      paragraphs.drop(1) shouldBe before
      paragraphs(0).plainText shouldBe "Intro"
  }

  "A replacement without the block line" should "report the table as removed" in withReport { (buffer, _, _) =>
    val lines = buffer.document.content.collect().split("\n", -1).toList
    val index = lines.indexOf(block)

    val after = replaced(buffer, lines.patch(index, Nil, 1).mkString("\n"))

    val document = after.richText.richTextDocument.value
    document.hasOpaqueBlock shouldBe false
    FidelityReport.forSave(document, SaveTarget.Docx).count(DocumentFeature.Tables, Treatment.Removed) shouldBe 1
  }

  "A replacement that puts text beside a block" should "detach the rich document and warn about the table it loses" in withReport {
    (buffer, _, _) =>
      val lines = buffer.document.content.collect().split("\n", -1).toList
      val index = lines.indexOf(block)

      val after = replaced(buffer, lines.updated(index, "x" + block).mkString("\n"))

      after.richText.richTextDocument shouldBe None
      after.richText.richTextFidelity.value.wouldDrop.map(_.feature) should contain(DocumentFeature.Tables)
  }

  "An external reload of a DOCX whose structure is unchanged" should "keep the block lines of the file read again" in withReport {
    (buffer, bytes, file) =>
      val entries = RichTextTestPackages.entries(bytes).map { entry =>
        entry.name -> (if entry.name == "word/document.xml" then
                         entry.text.replace("Closing words.", "Closing remarks.").getBytes("UTF-8")
                       else entry.bytes.toArray)
      }
      Files.write(file, GoldenFixtures.zip(entries))
      val disk = new FileManager().loadFile(file, bufferId).unsafeRunSync()

      val after = FileResults
        .reloaded(stateWith(buffer), bufferId, file, buffer.document.content, disk)
        .persisted
        .buffers(bufferId)

      after.document.content.collect() should include("Closing remarks.")
      paragraphsOf(after).count(_.isOpaqueBlock) shouldBe 1
      after.richText.richTextFidelity.map(_.summary) shouldBe Some("1 table preserved read-only")
  }
