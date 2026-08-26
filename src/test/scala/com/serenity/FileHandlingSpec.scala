package com.serenity

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.*
import com.serenity.keystroke.events.SaveFile
import com.serenity.richtext.*
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

  "StorageLocation" should "discover local paths as supported storage" in {
    val path = Path.of("notes.md")

    StorageLocation.parse(path.toString).shouldBe(Right(StorageLocation.Local(path)))
    StorageLocation.fromPath(path).canOpenWithCurrentStorage.shouldBe(true)
    StorageLocation.fromPath(path).canSaveWithCurrentStorage.shouldBe(true)
  }

  it should "treat file URIs as local storage" in {
    val path = Files.createTempFile("serenity-storage-location", ".txt")

    try StorageLocation.parse(path.toUri.toString).shouldBe(Right(StorageLocation.Local(path)))
    finally Files.deleteIfExists(path)
  }

  it should "discover remote URI storage without marking it supported by current file IO" in {
    val location = StorageLocation.parse("https://example.com/docs/notes.md")

    location.map(_.isRemote).shouldBe(Right(true))
    location.map(_.canOpenWithCurrentStorage).shouldBe(Right(false))
    location.map(_.canSaveWithCurrentStorage).shouldBe(Right(false))
  }

  it should "keep Windows absolute paths local instead of treating drive letters as URI schemes" in
    StorageLocation
      .parse("C:\\Users\\barna\\notes.md")
      .shouldBe(
        Right(
          StorageLocation.Local(Path.of("C:\\Users\\barna\\notes.md"))
        )
      )

  "LocalDocumentStorageProvider" should "open, list, save, and copy local documents through the provider boundary" in {
    val directory   = Files.createTempDirectory("serenity-document-storage")
    val source      = directory.resolve("source.txt")
    val destination = directory.resolve("copy.txt")
    val provider    = new LocalDocumentStorageProvider

    try
      Files.writeString(source, "initial")

      val opened = provider.open(StorageLocation.Local(source)).unsafeRunSync()
      opened.map(_.content) shouldBe Right("initial")

      val listed = provider.list(StorageLocation.Local(directory)).compile.toList.unsafeRunSync()
      listed.collect { case Right(metadata) => metadata.location } should contain(StorageLocation.Local(source))

      val saved =
        opened.flatMap(document => provider.save(document.location, "updated", document.revision).unsafeRunSync())
      saved.map(_.content) shouldBe Right("updated")

      val copied = provider.copy(StorageLocation.Local(source), StorageLocation.Local(destination)).unsafeRunSync()
      copied.map(_.content) shouldBe Right("updated")
    finally
      Files.deleteIfExists(destination)
      Files.deleteIfExists(source)
      Files.deleteIfExists(directory)
  }

  it should "reject stale local saves with a conflict at the provider boundary" in {
    val path     = Files.createTempFile("serenity-document-storage-conflict", ".txt")
    val provider = new LocalDocumentStorageProvider

    try
      Files.writeString(path, "initial")
      val opened = provider.open(StorageLocation.Local(path)).unsafeRunSync()
      Files.writeString(path, "remote change")

      val result =
        opened.flatMap(document => provider.save(document.location, "local change", document.revision).unsafeRunSync())

      result shouldBe Left(DocumentStorageError.Conflict(StorageLocation.Local(path)))
      Files.readString(path) shouldBe "remote change"
    finally Files.deleteIfExists(path)
  }

  it should "leave remote locations unsupported until a provider is installed" in {
    val location = StorageLocation.Remote(java.net.URI.create("https://example.com/documents/notes.txt"))
    val provider = new LocalDocumentStorageProvider

    provider.open(location).unsafeRunSync() shouldBe Left(DocumentStorageError.UnsupportedLocation(location))
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
      preservesRichFormatting = false
    )
    DocumentFormat.capabilities(FileType.WordOpenXmlDocument) shouldBe DocumentFormatCapabilities(
      canOpen = true,
      canSave = true,
      canRender = true,
      canEdit = true,
      preservesRichFormatting = false
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
    newBuffer.document.content.collect() shouldBe ""
    newBuffer.document.filePath shouldBe None
    newBuffer.document.isDirty shouldBe false
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

      buffer.document.content.collect() shouldBe "plain bold"
      buffer.richText.richTextDocument.map(_.plainText) shouldBe Some("plain bold")
      buffer.richText.richTextDocument
        .flatMap(_.paragraphs.headOption)
        .map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      buffer.document.filePath shouldBe Some(rtfFile)
      buffer.document.isDirty shouldBe false
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

      val loadedBuffer = fileManager
        .loadFile(sourceFile, BufferId(101))
        .unsafeRunSync()
      val buffer = loadedBuffer.copy(document =
        loadedBuffer.document.copy(content = com.serenity.rope.Rope("edited text"), isDirty = true)
      )
      val savedBuffer = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()
      val saved       = RtfDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "edited text"
      saved.paragraphs.headOption.map(marksForText(_, "edited")) shouldBe Some(Set.empty)
      savedBuffer.richText.richTextDocument.map(_.plainText) shouldBe Some("edited text")
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
      val formattedDocument = loadedBuffer.richText.richTextDocument
        .getOrElse(fail("expected rich text metadata"))
        .applyMark(
          com.serenity.richtext.RichTextRange(
            com.serenity.richtext.RichTextPosition(0, 6),
            com.serenity.richtext.RichTextPosition(0, 10)
          ),
          InlineMark.Bold
        )
      val formattingDirtyBuffer = loadedBuffer.copy(
        document = loadedBuffer.document.copy(isDirty = true),
        richText = loadedBuffer.richText.copy(richTextDocument = Some(formattedDocument))
      )

      val savedBuffer = fileManager.saveBuffer(formattingDirtyBuffer, savedFile).unsafeRunSync()
      val saved       = RtfDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "plain bold"
      saved.paragraphs.headOption.map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      savedBuffer.richText.richTextDocument shouldBe Some(formattedDocument.normalized)
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

      buffer.document.content.collect() shouldBe "plain bold"
      buffer.richText.richTextDocument.map(_.plainText) shouldBe Some("plain bold")
      buffer.richText.richTextDocument
        .flatMap(_.paragraphs.headOption)
        .map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      buffer.document.filePath shouldBe Some(odtFile)
      buffer.document.isDirty shouldBe false
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
    val plainBuffer = Buffer.fromString(BufferId(104), "plain bold")
    val buffer = plainBuffer.copy(
      document = plainBuffer.document.copy(isDirty = true),
      richText = plainBuffer.richText.copy(richTextDocument = Some(document))
    )

    try
      val savedBuffer = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()
      val saved       = com.serenity.richtext.OdtDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "plain bold"
      saved.paragraphs.headOption.map(_.alignment) shouldBe Some(com.serenity.richtext.ParagraphAlignment.Center)
      saved.paragraphs.headOption.map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      savedBuffer.richText.richTextDocument shouldBe Some(document.normalized)
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

      buffer.document.content.collect() shouldBe "plain bold"
      buffer.richText.richTextDocument.map(_.plainText) shouldBe Some("plain bold")
      buffer.richText.richTextDocument
        .flatMap(_.paragraphs.headOption)
        .map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      buffer.document.filePath shouldBe Some(docxFile)
      buffer.document.isDirty shouldBe false
    finally Files.deleteIfExists(docxFile)
  }

  it should "require Save As before replacing a DOCX import with unsupported content" in {
    val fileManager = new FileManager()
    val sourceFile  = Files.createTempFile("serenity-lossy-source", ".docx")
    val savedFile   = Files.createTempFile("serenity-lossy-copy", ".docx")

    try
      Files.write(sourceFile, docxBytesFromFixture("docx-unsupported-table.xml"))
      val sourceBytes = Files.readAllBytes(sourceFile)

      val buffer = fileManager.loadFile(sourceFile, BufferId(108)).unsafeRunSync()

      buffer.richText.richTextFidelity.exists(!_.isLossless) shouldBe true
      fileManager.saveBuffer(buffer).attempt.unsafeRunSync().left.map(_.getMessage) shouldBe Left(
        s"Saving $sourceFile would discard unsupported rich document content. Use Save As to write a new file."
      )
      Files.readAllBytes(sourceFile) shouldBe sourceBytes

      val saved = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()

      Files.exists(savedFile) shouldBe true
      saved.document.filePath shouldBe Some(savedFile)
      saved.richText.richTextFidelity shouldBe None
    finally
      Files.deleteIfExists(sourceFile)
      Files.deleteIfExists(savedFile)
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
    val plainBuffer = Buffer.fromString(BufferId(106), "plain bold")
    val buffer = plainBuffer.copy(
      document = plainBuffer.document.copy(isDirty = true),
      richText = plainBuffer.richText.copy(richTextDocument = Some(document))
    )

    try
      val savedBuffer = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()
      val saved       = com.serenity.richtext.DocxDocumentCodec.read(savedFile).unsafeRunSync()

      saved.plainText shouldBe "plain bold"
      saved.paragraphs.headOption.map(_.alignment) shouldBe Some(com.serenity.richtext.ParagraphAlignment.Center)
      saved.paragraphs.headOption.map(marksForText(_, "bold")) shouldBe Some(Set(InlineMark.Bold))
      savedBuffer.richText.richTextDocument shouldBe Some(document.normalized)
    finally Files.deleteIfExists(savedFile)
  }

  it should "export rich text headings and inline marks to Markdown" in {
    val fileManager = new FileManager()
    val savedFile   = Files.createTempFile("serenity-markdown-save", ".md")
    val document = RichTextDocument(
      List(
        RichTextParagraph.plain("Title", role = ParagraphRole.Heading(1)),
        RichTextParagraph(
          List(
            RichTextRun("bold", RichTextStyle(marks = Set(InlineMark.Bold))),
            RichTextRun(" "),
            RichTextRun("italic", RichTextStyle(marks = Set(InlineMark.Italic))),
            RichTextRun(" "),
            RichTextRun("underlined", RichTextStyle(marks = Set(InlineMark.Underline)))
          )
        )
      )
    )
    val plainBuffer = Buffer.fromString(BufferId(107), document.plainText)
    val buffer = plainBuffer.copy(richText = plainBuffer.richText.copy(richTextDocument = Some(document)))

    try
      val saved = fileManager.saveBuffer(buffer, savedFile).unsafeRunSync()

      Files.readString(savedFile) shouldBe "# Title\n**bold** *italic* <u>underlined</u>"
      saved.document.language shouldBe Some(com.serenity.lsp.config.LanguageId.Markdown)
      saved.richText.richTextDocument shouldBe None
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

  private def docxBytesFromFixture(name: String): Array[Byte] =
    val source = scala.io.Source.fromResource(s"richtext/$name")
    val xml =
      try source.mkString
      finally source.close()
    val output = java.io.ByteArrayOutputStream()
    val zip    = ZipOutputStream(output)
    try
      zip.putNextEntry(ZipEntry("word/document.xml"))
      zip.write(xml.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      zip.closeEntry()
    finally zip.close()
    output.toByteArray

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
        document = buffer.document.copy(
          content = com.serenity.rope.Rope("val x = 43"),
          isDirty = true
        )
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
      updatedState.buffers(modifiedBuffer.id).document.isDirty shouldBe false

    finally Files.deleteIfExists(tempFile)
  }
