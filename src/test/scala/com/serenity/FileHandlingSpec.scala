package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.*
import com.serenity.keystroke.events.SaveFile
import com.serenity.richtext.{InlineMark, RichTextParagraph, RtfDocumentCodec}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class FileHandlingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("FileHandlingSpec"))
    StateManager.apply(logger).unsafeRunSync()

  "FileType detection" should "work correctly for common extensions" in {
    val scalaPath    = java.nio.file.Paths.get("test.scala")
    val javaPath     = java.nio.file.Paths.get("test.java")
    val jsPath       = java.nio.file.Paths.get("test.js")
    val markdownPath = java.nio.file.Paths.get("notes.markdown")
    val docPath      = java.nio.file.Paths.get("draft.doc")
    val docxPath     = java.nio.file.Paths.get("draft.docx")
    val odtPath      = java.nio.file.Paths.get("draft.odt")
    val rtfPath      = java.nio.file.Paths.get("draft.rtf")
    val unknownPath  = java.nio.file.Paths.get("test.xyz")

    FileType.fromPath(scalaPath) shouldBe FileType.Scala
    FileType.fromPath(javaPath) shouldBe FileType.Java
    FileType.fromPath(jsPath) shouldBe FileType.JavaScript
    FileType.fromPath(markdownPath) shouldBe FileType.Markdown
    FileType.fromPath(docPath) shouldBe FileType.WordDocument
    FileType.fromPath(docxPath) shouldBe FileType.WordOpenXmlDocument
    FileType.fromPath(odtPath) shouldBe FileType.OpenDocumentText
    FileType.fromPath(rtfPath) shouldBe FileType.RichText
    FileType.fromPath(unknownPath) shouldBe FileType.Unknown
  }

  "DocumentFormat" should "classify text, markdown, source, structured, and rich document formats" in {
    DocumentFormat.fromPath(Path.of("draft.txt")) shouldBe DocumentFormat.PlainText
    DocumentFormat.fromPath(Path.of("draft.md")) shouldBe DocumentFormat.Markdown
    DocumentFormat.fromPath(Path.of("draft.scala")) shouldBe DocumentFormat.SourceCode
    DocumentFormat.fromPath(Path.of("draft.json")) shouldBe DocumentFormat.StructuredText
    DocumentFormat.fromPath(Path.of("draft.docx")) shouldBe DocumentFormat.RichTextDocument
    DocumentFormat.fromPath(Path.of("draft.odt")) shouldBe DocumentFormat.RichTextDocument
  }

  it should "report only implemented document operation capabilities" in {
    DocumentFormat.capabilities(DocumentFormat.PlainText) shouldBe DocumentFormatCapabilities(
      canOpen = true,
      canSave = true,
      canRender = true,
      canEdit = true,
      preservesRichFormatting = false
    )
    DocumentFormat.capabilities(DocumentFormat.Markdown) shouldBe DocumentFormatCapabilities(
      canOpen = true,
      canSave = true,
      canRender = true,
      canEdit = true,
      preservesRichFormatting = false
    )
    DocumentFormat.capabilities(DocumentFormat.RichTextDocument) shouldBe DocumentFormatCapabilities(
      canOpen = true,
      canSave = true,
      canRender = true,
      canEdit = true,
      preservesRichFormatting = true
    )
    DocumentFormat.capabilities(FileType.RichText) shouldBe DocumentFormatCapabilities(
      canOpen = true,
      canSave = true,
      canRender = true,
      canEdit = true,
      preservesRichFormatting = true
    )
    DocumentFormat.capabilities(FileType.OpenDocumentText) shouldBe DocumentFormatCapabilities(
      canOpen = true,
      canSave = true,
      canRender = true,
      canEdit = true,
      preservesRichFormatting = true
    )
    DocumentFormat.capabilities(FileType.WordOpenXmlDocument) shouldBe DocumentFormatCapabilities(
      canOpen = true,
      canSave = true,
      canRender = true,
      canEdit = true,
      preservesRichFormatting = true
    )
    DocumentFormat.capabilities(FileType.WordDocument) shouldBe DocumentFormatCapabilities(
      canOpen = false,
      canSave = false,
      canRender = false,
      canEdit = false,
      preservesRichFormatting = false
    )
  }

  "FileManager" should "create and manage buffers" in {
    val fileManager = new FileManager()

    val newBuffer = fileManager.createNewBuffer(BufferId(42)).unsafeRunSync()
    newBuffer.id shouldBe BufferId(42)
    newBuffer.content.collect() shouldBe ""
    newBuffer.filePath shouldBe None
    newBuffer.isDirty shouldBe false
  }

  it should "raise a clear error instead of opening rich documents as plain text" in {
    val fileManager = new FileManager()
    val docFile     = Files.createTempFile("serenity-rich-open", ".doc")

    try
      Files.writeString(docFile, "not a real doc")

      val result = fileManager.loadFile(docFile, BufferId(99)).attempt.unsafeRunSync()

      result.left.map(_.getMessage) shouldBe Left(
        "Unsupported document format for open: Legacy Word Document (.doc, unsupported)"
      )
    finally Files.deleteIfExists(docFile)
  }

  it should "raise a clear error instead of saving text buffers as rich documents" in {
    val fileManager = new FileManager()
    val docFile     = Files.createTempDirectory("serenity-rich-save").resolve("draft.doc")
    val buffer      = Buffer.fromString(BufferId(99), "plain text")

    try
      val result = fileManager.saveBuffer(buffer, docFile).attempt.unsafeRunSync()

      result.left.map(_.getMessage) shouldBe Left(
        "Unsupported document format for save: Legacy Word Document (.doc, unsupported)"
      )
      Files.exists(docFile) shouldBe false
    finally Files.deleteIfExists(docFile.getParent)
  }

  it should "open RTF files as editable plain text with rich document metadata" in {
    val fileManager = new FileManager()
    val rtfFile     = Files.createTempFile("serenity-rich-open", ".rtf")

    try
      Files.writeString(rtfFile, """{\rtf1\ansi plain \b bold\b0\par}""")

      val buffer = fileManager.loadFile(rtfFile, BufferId(99)).unsafeRunSync()

      buffer.content.collect() shouldBe "plain bold"
      buffer.richTextDocument.map(_.plainText) shouldBe Some("plain bold")
      buffer.richTextDocument
        .flatMap(_.paragraphs.headOption)
        .map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      buffer.filePath shouldBe Some(rtfFile)
      buffer.isDirty shouldBe false
    finally Files.deleteIfExists(rtfFile)
  }

  it should "save clean RTF buffers without dropping rich formatting" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-rich-source", ".rtf")
    val savedFile   = Files.createTempFile("serenity-rich-saved", ".rtf")

    try
      Files.writeString(sourceFile, """{\rtf1\ansi plain \b bold\b0\par}""")

      val buffer = fileManager.loadFile(sourceFile, BufferId(100)).unsafeRunSync()
      fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()

      val saved = RtfDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "plain bold"
      saved.paragraphs.headOption.map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedFile)
  }

  it should "save dirty RTF buffers from current text instead of stale rich metadata" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-rich-dirty-source", ".rtf")
    val savedFile   = Files.createTempFile("serenity-rich-dirty-saved", ".rtf")

    try
      Files.writeString(sourceFile, """{\rtf1\ansi plain \b bold\b0\par}""")

      val buffer = fileManager
        .loadFile(sourceFile, BufferId(101))
        .unsafeRunSync()
        .copy(content = com.serenity.rope.Rope("edited text"), isDirty = true)
      val savedBuffer = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()
      val saved       = RtfDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "edited text"
      saved.paragraphs.headOption.map(marksForText(_, "edited")) shouldBe Some(Set.empty)
      savedBuffer.richTextDocument.map(_.plainText) shouldBe Some("edited text")
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedFile)
  }

  it should "save dirty RTF buffers with aligned rich formatting metadata" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-rich-format-source", ".rtf")
    val savedFile   = Files.createTempFile("serenity-rich-format-saved", ".rtf")

    try
      Files.writeString(sourceFile, """{\rtf1\ansi plain bold\par}""")

      val loadedBuffer = fileManager.loadFile(sourceFile, BufferId(102)).unsafeRunSync()
      val formattedDocument = loadedBuffer.richTextDocument
        .getOrElse(fail("expected rich text metadata"))
        .applyMark(
          com.serenity.richtext.RichTextRange(
            com.serenity.richtext.RichTextPosition(0, 6),
            com.serenity.richtext.RichTextPosition(0, 10)
          ),
          InlineMark.Bold
        )
      val formattingDirtyBuffer = loadedBuffer.copy(isDirty = true, richTextDocument = Some(formattedDocument))

      val savedBuffer = fileManager.saveBuffer(formattingDirtyBuffer, savedFile).unsafeRunSync()
      val saved       = RtfDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "plain bold"
      saved.paragraphs.headOption.map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      savedBuffer.richTextDocument shouldBe Some(formattedDocument.normalized)
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedFile)
  }

  it should "open ODT files as editable plain text with rich document metadata" in {
    val fileManager = new FileManager()
    val odtFile     = Files.createTempFile("serenity-odt-open", ".odt")
    val source = com.serenity.richtext.RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun("plain "),
            com.serenity.richtext.RichTextRun(
              "bold",
              com.serenity.richtext.RichTextStyle(marks = Set(InlineMark.Bold))
            )
          )
        )
      )
    )

    try
      com.serenity.richtext.OdtDocumentCodec.write(source, odtFile).unsafeRunSync()

      val buffer = fileManager.loadFile(odtFile, BufferId(103)).unsafeRunSync()

      buffer.content.collect() shouldBe "plain bold"
      buffer.richTextDocument.map(_.plainText) shouldBe Some("plain bold")
      buffer.richTextDocument
        .flatMap(_.paragraphs.headOption)
        .map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      buffer.filePath shouldBe Some(odtFile)
      buffer.isDirty shouldBe false
    finally Files.deleteIfExists(odtFile)
  }

  it should "save ODT buffers with aligned rich formatting metadata" in {
    val fileManager = new FileManager()
    val savedFile   = Files.createTempFile("serenity-odt-save", ".odt")
    val document = com.serenity.richtext.RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun("plain "),
            com.serenity.richtext.RichTextRun(
              "bold",
              com.serenity.richtext.RichTextStyle(marks = Set(InlineMark.Bold))
            )
          ),
          alignment = com.serenity.richtext.ParagraphAlignment.Center
        )
      )
    )
    val buffer = Buffer
      .fromString(BufferId(104), "plain bold")
      .copy(isDirty = true, richTextDocument = Some(document))

    try
      val savedBuffer = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()
      val saved       = com.serenity.richtext.OdtDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "plain bold"
      saved.paragraphs.headOption.map(_.alignment) shouldBe Some(com.serenity.richtext.ParagraphAlignment.Center)
      saved.paragraphs.headOption.map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      savedBuffer.richTextDocument shouldBe Some(document.normalized)
    finally Files.deleteIfExists(savedFile)
  }

  it should "open DOCX files as editable plain text with rich document metadata" in {
    val fileManager = new FileManager()
    val docxFile    = Files.createTempFile("serenity-docx-open", ".docx")
    val source = com.serenity.richtext.RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun("plain "),
            com.serenity.richtext.RichTextRun(
              "bold",
              com.serenity.richtext.RichTextStyle(marks = Set(InlineMark.Bold))
            )
          )
        )
      )
    )

    try
      com.serenity.richtext.DocxDocumentCodec.write(source, docxFile).unsafeRunSync()

      val buffer = fileManager.loadFile(docxFile, BufferId(105)).unsafeRunSync()

      buffer.content.collect() shouldBe "plain bold"
      buffer.richTextDocument.map(_.plainText) shouldBe Some("plain bold")
      buffer.richTextDocument
        .flatMap(_.paragraphs.headOption)
        .map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      buffer.filePath shouldBe Some(docxFile)
      buffer.isDirty shouldBe false
    finally Files.deleteIfExists(docxFile)
  }

  it should "save DOCX buffers with aligned rich formatting metadata" in {
    val fileManager = new FileManager()
    val savedFile   = Files.createTempFile("serenity-docx-save", ".docx")
    val document = com.serenity.richtext.RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun("plain "),
            com.serenity.richtext.RichTextRun(
              "bold",
              com.serenity.richtext.RichTextStyle(marks = Set(InlineMark.Bold))
            )
          ),
          alignment = com.serenity.richtext.ParagraphAlignment.Center
        )
      )
    )
    val buffer = Buffer
      .fromString(BufferId(106), "plain bold")
      .copy(isDirty = true, richTextDocument = Some(document))

    try
      val savedBuffer = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()
      val saved       = com.serenity.richtext.DocxDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "plain bold"
      saved.paragraphs.headOption.map(_.alignment) shouldBe Some(com.serenity.richtext.ParagraphAlignment.Center)
      saved.paragraphs.headOption.map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      savedBuffer.richTextDocument shouldBe Some(document.normalized)
    finally Files.deleteIfExists(savedFile)
  }

  "FileUtils" should "handle file operations" in {
    // Create a temporary file
    val tempFile = Files.createTempFile("test", ".txt")

    try
      // Test writing and reading
      val content = "test content\nwith multiple lines"
      FileUtils.writeFileContent(tempFile, content).unsafeRunSync()

      FileUtils.isReadableFile(tempFile) shouldBe true

      val readContent = FileUtils.readFileContent(tempFile).unsafeRunSync()
      readContent shouldBe content

    finally Files.deleteIfExists(tempFile)
  }

  private def marksForText(paragraph: RichTextParagraph, text: String): Set[InlineMark] =
    paragraph.runs
      .find(_.text.contains(text))
      .map(_.style.marks)
      .getOrElse(Set.empty)

  it should "raise a clear error when reading a missing file" in {
    val missingFile = Files.createTempDirectory("serenity-missing-file").resolve("missing.txt")

    try
      val result = FileUtils.readFileContent(missingFile).attempt.unsafeRunSync()

      result.left.map(_.getMessage) shouldBe Left(s"File not readable: $missingFile")
    finally Files.deleteIfExists(missingFile.getParent)
  }

  "FileBrowser" should "list directory contents" in {
    val fileBrowser = new FileBrowser()

    val currentDir = fileBrowser.getCurrentDirectory.unsafeRunSync()
    currentDir should not be null

    val entries = fileBrowser.listCurrentDirectory.unsafeRunSync()
    entries should not be null
    // Should contain at least some entries (directories or files)
  }

  it should "raise a clear error when changing to a missing directory" in {
    val fileBrowser      = new FileBrowser()
    val missingDirectory = Files.createTempDirectory("serenity-missing-directory").resolve("missing")

    try
      val result = fileBrowser.changeDirectory(missingDirectory).attempt.unsafeRunSync()

      result.left.map(_.getMessage) shouldBe Left(s"Directory does not exist: $missingDirectory")
    finally Files.deleteIfExists(missingDirectory.getParent)
  }

  "StateManager" should "handle save file events through the active editor pane" in {
    // Create a temporary file with content
    val tempFile       = Files.createTempFile("test", ".scala")
    val initialContent = "val x = 42"

    try
      Files.writeString(tempFile, initialContent)

      // Create a buffer from the file
      val fileManager = new FileManager()
      val buffer      = fileManager.loadFile(tempFile, BufferId(42)).unsafeRunSync()
      buffer.id shouldBe BufferId(42)

      // Modify buffer content
      val modifiedBuffer = buffer.copy(
        content = com.serenity.rope.Rope("val x = 43"),
        isDirty = true
      )

      val stateManager = createStateManager()
      stateManager
        .updateState { state =>
          state.copy(
            buffers = state.buffers + (modifiedBuffer.id -> modifiedBuffer),
            layout = state.layout.copy(
              editorPanes = state.layout.editorPanes.updated(
                PaneId(0),
                state.layout.editorPanes(PaneId(0)).copy(bufferId = Some(modifiedBuffer.id))
              )
            ),
            focus = Focus.EditorPane(PaneId(0))
          )
        }
        .unsafeRunSync()

      stateManager.applyEvent(SaveFile).unsafeRunSync()

      // Verify file was saved
      val savedContent = Files.readString(tempFile)
      savedContent shouldBe "val x = 43"
      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.buffers(modifiedBuffer.id).isDirty shouldBe false

    finally Files.deleteIfExists(tempFile)
  }
