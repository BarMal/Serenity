package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.io.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `LocalDocumentStorageProvider` and `FileManager`'s revision-capture/conflict-detection behavior added for #1623
  * external-change detection. Split out of `FileHandlingSpec.scala` purely to keep both files under this repo's
  * architecture-ratchet file-length limit -- no coverage changed by this split.
  */
class FileHandlingDocumentStorageSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def bytes(content: String): Array[Byte] = content.getBytes(java.nio.charset.StandardCharsets.UTF_8)

  "LocalDocumentStorageProvider" should "open, list, save, and copy local documents through the provider boundary" in {
    val directory   = Files.createTempDirectory("serenity-document-storage")
    val source      = directory.resolve("source.txt")
    val destination = directory.resolve("copy.txt")
    val provider    = LocalDocumentStorageProvider()

    try
      Files.writeString(source, "initial")

      val opened = provider.open(StorageLocation.Local(source)).unsafeRunSync()
      opened.map(_.content.toSeq) shouldBe Right(bytes("initial").toSeq)

      val listed = provider.list(StorageLocation.Local(directory)).compile.toList.unsafeRunSync()
      listed.collect { case Right(metadata) => metadata.location } should contain(StorageLocation.Local(source))

      val saved =
        opened.flatMap(document =>
          provider.save(document.location, bytes("updated"), document.revision).unsafeRunSync()
        )
      saved.map(_.content.toSeq) shouldBe Right(bytes("updated").toSeq)

      val copied = provider.copy(StorageLocation.Local(source), StorageLocation.Local(destination)).unsafeRunSync()
      copied.map(_.content.toSeq) shouldBe Right(bytes("updated").toSeq)
    finally
      Files.deleteIfExists(destination)
      Files.deleteIfExists(source)
      Files.deleteIfExists(directory)
  }

  it should "reject stale local saves with a conflict at the provider boundary" in {
    val path     = Files.createTempFile("serenity-document-storage-conflict", ".txt")
    val provider = LocalDocumentStorageProvider()

    try
      Files.writeString(path, "initial")
      val opened = provider.open(StorageLocation.Local(path)).unsafeRunSync()
      Files.writeString(path, "remote change")

      val result =
        opened.flatMap(document =>
          provider.save(document.location, bytes("local change"), document.revision).unsafeRunSync()
        )

      result shouldBe Left(DocumentStorageError.Conflict(StorageLocation.Local(path)))
      Files.readString(path) shouldBe "remote change"
    finally Files.deleteIfExists(path)
  }

  it should "carry binary content that is not valid UTF-8 through open, save, and copy unmodified" in {
    val directory   = Files.createTempDirectory("serenity-document-storage-binary")
    val source      = directory.resolve("source.bin")
    val destination = directory.resolve("copy.bin")
    val provider    = LocalDocumentStorageProvider()
    val binary      = Array[Byte](0x50, 0x4b, 0x03, 0x04, -1, -128, 0)

    try
      Files.write(source, binary)

      val opened = provider.open(StorageLocation.Local(source)).unsafeRunSync()
      opened.map(_.content.toSeq) shouldBe Right(binary.toSeq)

      val copied = provider.copy(StorageLocation.Local(source), StorageLocation.Local(destination)).unsafeRunSync()
      copied.map(_.content.toSeq) shouldBe Right(binary.toSeq)
    finally
      Files.deleteIfExists(destination)
      Files.deleteIfExists(source)
      Files.deleteIfExists(directory)
  }

  it should "not conflict-check a save when no expected revision is supplied" in {
    val path     = Files.createTempFile("serenity-document-storage-no-check", ".txt")
    val provider = LocalDocumentStorageProvider()

    try
      Files.writeString(path, "existing")

      val result = provider.save(StorageLocation.Local(path), bytes("overwritten"), None).unsafeRunSync()

      result.isRight shouldBe true
      Files.readString(path) shouldBe "overwritten"
    finally Files.deleteIfExists(path)
  }

  it should "leave remote locations unsupported until a provider is installed" in {
    val location = StorageLocation.Remote(java.net.URI.create("https://example.com/documents/notes.txt"))
    val provider = LocalDocumentStorageProvider()

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

  "FileManager" should "capture a revision on load and refresh it on save, for #1623 external-change detection" in {
    val fileManager = new FileManager()
    val file        = Files.createTempFile("serenity-revision-capture", ".md")

    try
      Files.writeString(file, "original")

      val opened = fileManager.loadFile(file, BufferId(99)).unsafeRunSync()
      opened.document.revision shouldBe defined

      val edited =
        opened.copy(document = opened.document.copy(content = com.serenity.rope.Rope("edited"), isDirty = true))
      val saved = fileManager.saveBuffer(edited, file).unsafeRunSync()

      saved.document.revision shouldBe defined
      saved.document.revision should not be opened.document.revision
    finally Files.deleteIfExists(file)
  }

  it should "reject a save with a conflict instead of silently overwriting a file that changed on disk since it was opened" in {
    val fileManager = new FileManager()
    val file        = Files.createTempFile("serenity-revision-conflict", ".md")

    try
      Files.writeString(file, "original")
      val opened = fileManager.loadFile(file, BufferId(99)).unsafeRunSync()

      // Simulate another program (another editor, a git checkout) changing the file after Serenity opened it.
      Files.writeString(file, "changed externally")

      val edited =
        opened.copy(document = opened.document.copy(content = com.serenity.rope.Rope("my local edit"), isDirty = true))
      val result = fileManager.saveBuffer(edited, file).attempt.unsafeRunSync()

      result shouldBe Left(
        FileManagerError.ExternalConflict(StorageLocation.Local(file))
      )
      Files.readString(file) shouldBe "changed externally"
    finally Files.deleteIfExists(file)
  }

  it should "not conflict-check a Save As to a different path, even when the buffer carries a revision from its original file" in {
    val fileManager = new FileManager()
    val original    = Files.createTempFile("serenity-revision-save-as-source", ".md")
    val destination = Files.createTempFile("serenity-revision-save-as-dest", ".md")

    try
      Files.writeString(original, "original")
      Files.writeString(destination, "unrelated pre-existing content")
      val opened = fileManager.loadFile(original, BufferId(99)).unsafeRunSync()

      val result = fileManager.saveBuffer(opened, destination).attempt.unsafeRunSync()

      result.isRight shouldBe true
      Files.readString(destination) shouldBe "original"
    finally
      Files.deleteIfExists(original)
      Files.deleteIfExists(destination)
  }
