package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.*
import com.serenity.keystroke.events.SaveFile
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
    val scalaPath   = java.nio.file.Paths.get("test.scala")
    val javaPath    = java.nio.file.Paths.get("test.java")
    val jsPath      = java.nio.file.Paths.get("test.js")
    val unknownPath = java.nio.file.Paths.get("test.xyz")

    FileType.fromPath(scalaPath) shouldBe FileType.Scala
    FileType.fromPath(javaPath) shouldBe FileType.Java
    FileType.fromPath(jsPath) shouldBe FileType.JavaScript
    FileType.fromPath(unknownPath) shouldBe FileType.Unknown
  }

  "FileManager" should "create and manage buffers" in {
    val fileManager = new FileManager()

    val newBuffer = fileManager.createNewBuffer(BufferId(42)).unsafeRunSync()
    newBuffer.id shouldBe BufferId(42)
    newBuffer.content.collect() shouldBe ""
    newBuffer.filePath shouldBe None
    newBuffer.isDirty shouldBe false
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
