package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.{FileManager, FileBrowser, FileUtils, FileType}
import com.serenity.keystroke.events.{SaveFile, OpenFile}
import com.serenity.state.components.EditorPaneComponent
import com.serenity.state.models.*
import com.serenity.rope.Balance
import com.serenity.ui.layout.Layout
import com.serenity.state.components.ComponentResult
import java.nio.file.{Files, Path}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileHandlingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "FileType detection" should "work correctly for common extensions" in {
    val scalaPath = java.nio.file.Paths.get("test.scala")
    val javaPath = java.nio.file.Paths.get("test.java") 
    val jsPath = java.nio.file.Paths.get("test.js")
    val unknownPath = java.nio.file.Paths.get("test.xyz")
    
    FileType.fromPath(scalaPath) shouldBe FileType.Scala
    FileType.fromPath(javaPath) shouldBe FileType.Java
    FileType.fromPath(jsPath) shouldBe FileType.JavaScript
    FileType.fromPath(unknownPath) shouldBe FileType.Unknown
  }

  "FileManager" should "create and manage buffers" in {
    val fileManager = new FileManager()
    
    val newBuffer = fileManager.createNewBuffer.unsafeRunSync()
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
      
    finally
      Files.deleteIfExists(tempFile)
  }

  "FileBrowser" should "list directory contents" in {
    val fileBrowser = new FileBrowser()
    
    val currentDir = fileBrowser.getCurrentDirectory.unsafeRunSync()
    currentDir should not be null
    
    val entries = fileBrowser.listCurrentDirectory.unsafeRunSync()
    entries should not be null
    // Should contain at least some entries (directories or files)
  }

  "EditorPaneComponent" should "handle save file events" in {
    // Create a temporary file with content
    val tempFile = Files.createTempFile("test", ".scala")
    val initialContent = "val x = 42"
    
    try
      Files.writeString(tempFile, initialContent)
      
      // Create a buffer from the file
      val fileManager = new FileManager()
      val buffer = fileManager.loadFile(tempFile).unsafeRunSync()
      
      // Modify buffer content
      val modifiedBuffer = buffer.copy(
        content = com.serenity.rope.Rope("val x = 43"), 
        isDirty = true
      )
      
      val paneId = PaneId(1)
      val cursor = CursorPosition(0, 0)
      val pane = EditorPane(paneId, Some(modifiedBuffer.id), Viewport.default, List(cursor), 0)
      val state = AppState.empty.copy(
        buffers = Map(modifiedBuffer.id -> modifiedBuffer),
        layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
      )
      
      val component = new EditorPaneComponent(paneId)
      val result = component.processEvent(SaveFile, state)
      
      result should not be ComponentResult.noChange
      
      // Verify file was saved
      val savedContent = Files.readString(tempFile)
      savedContent shouldBe "val x = 43"
      
    finally
      Files.deleteIfExists(tempFile)
  }

