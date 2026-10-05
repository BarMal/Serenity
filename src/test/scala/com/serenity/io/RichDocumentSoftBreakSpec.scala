package com.serenity.io

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.InsertChar
import com.serenity.richtext.{DocxDocumentCodec, InlineMark, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A soft line break sits inside a paragraph, so it must not become a rope line: every rich-text layer reads paragraph
  * i as rope line i.
  */
class RichDocumentSoftBreakSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private val docxWithSoftBreak =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
      |  <w:body>
      |    <w:p>
      |      <w:r><w:rPr><w:b/></w:rPr><w:t>first</w:t><w:br/><w:t>line</w:t></w:r>
      |    </w:p>
      |    <w:p>
      |      <w:r><w:rPr><w:i/></w:rPr><w:t>second</w:t></w:r>
      |    </w:p>
      |  </w:body>
      |</w:document>""".stripMargin

  private val odtWithSoftBreak =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<office:document-content
      |    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
      |    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">
      |  <office:body>
      |    <office:text>
      |      <text:p>first<text:line-break/>line</text:p>
      |      <text:p>second</text:p>
      |    </office:text>
      |  </office:body>
      |</office:document-content>""".stripMargin

  private val rtfWithSoftBreak = """{\rtf1\ansi\pard first\line line\par second\par}"""

  "Opening a DOCX with a soft line break" should "keep one rope line per paragraph" in
    expectOneLinePerParagraph(open(packageFile("draft.docx", "word/document.xml", docxWithSoftBreak)))

  it should "edit the paragraph under the caret and keep every paragraph's formatting on save" in {
    val path    = packageFile("draft.docx", "word/document.xml", docxWithSoftBreak)
    val manager = FileManager()
    val opened  = manager.loadFile(path, bufferId).unsafeRunSync()
    val endOfSecondParagraph =
      opened.richText.richTextDocument.flatMap(_.paragraphAt(1)).value.plainTextLength
    val atEnd = opened.copy(editing = EditingState(List(CursorPosition(1, endOfSecondParagraph))))

    val typed = EditorEventReducer.reduce(InsertChar('!'), paneId, stateWith(atEnd)).state
    val _     = manager.saveBuffer(typed.persisted.buffers(bufferId)).unsafeRunSync()

    val saved = DocxDocumentCodec.readBytes(Files.readAllBytes(path)).toOption.value
    saved.paragraphs.map(_.plainText.length) shouldBe List("first line".length, "second!".length)
    saved.paragraphs.lastOption.map(_.plainText) shouldBe Some("second!")
    marksOf(saved.paragraphs.headOption.value, "first") should contain(InlineMark.Bold)
    marksOf(saved.paragraphs.headOption.value, "line") should contain(InlineMark.Bold)
    marksOf(saved.paragraphs.lastOption.value, "second!") should contain(InlineMark.Italic)
  }

  it should "export the soft break as a line break when saved as plain text or Markdown" in {
    val manager = FileManager()
    val opened =
      manager.loadFile(packageFile("draft.docx", "word/document.xml", docxWithSoftBreak), bufferId).unsafeRunSync()
    val folder = Files.createTempDirectory("serenity-soft-break-export")

    val _ = manager.saveBuffer(opened, folder.resolve("export.txt")).unsafeRunSync()
    val _ = manager.saveBuffer(opened, folder.resolve("export.md")).unsafeRunSync()

    Files.readString(folder.resolve("export.txt")) shouldBe "first\nline\nsecond"
    Files.readString(folder.resolve("export.md")) shouldBe "**first**\\\n**line**\n*second*"
  }

  "Opening an ODT with a soft line break" should "keep one rope line per paragraph" in
    expectOneLinePerParagraph(open(packageFile("draft.odt", "content.xml", odtWithSoftBreak)))

  "Opening an RTF with a soft line break" should "keep one rope line per paragraph" in {
    val path = Files.createTempDirectory("serenity-soft-break").resolve("draft.rtf")
    val _    = Files.write(path, rtfWithSoftBreak.getBytes(StandardCharsets.ISO_8859_1))
    expectOneLinePerParagraph(open(path))
  }

  private def expectOneLinePerParagraph(buffer: Buffer): Unit =
    val content  = buffer.document.content
    val document = buffer.richText.richTextDocument.value
    content.lineCount shouldBe 2
    document.matchesPlainTextShape(content.lineCount, content.weight) shouldBe true
    document.paragraphAt(1).map(_.plainText) shouldBe Some("second")

  private def open(path: Path): Buffer =
    FileManager().loadFile(path, bufferId).unsafeRunSync()

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def marksOf(paragraph: RichTextParagraph, text: String): Set[InlineMark] =
    paragraph.runs.find(_.text.contains(text)).map(_.style.marks).getOrElse(Set.empty)

  private def packageFile(name: String, entryName: String, content: String): Path =
    val path   = Files.createTempDirectory("serenity-soft-break").resolve(name)
    val output = java.io.ByteArrayOutputStream()
    val zip    = ZipOutputStream(output)
    try
      zip.putNextEntry(ZipEntry(entryName))
      zip.write(content.getBytes(StandardCharsets.UTF_8))
      zip.closeEntry()
    finally zip.close()
    Files.write(path, output.toByteArray)

end RichDocumentSoftBreakSpec
