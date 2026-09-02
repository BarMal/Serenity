package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Enter, InsertChar, ModalCreateDirectory, OpenFile, SaveAsFile, SaveFile, TabKey}
import com.serenity.richtext.{
  InlineMark,
  RichTextDocument,
  RichTextFidelity,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle
}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class FileWorkflowStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("FileWorkflowStateManagerSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def currentWorkflow(stateManager: StateManager): FileWorkflowState =
    stateManager.getCurrentState
      .unsafeRunSync()
      .modalSurface
      .flatMap {
        _.content match
          case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some(workflow)
          case _                                                          => None
      }
      .getOrElse(fail("Expected active file workflow modal"))

  "StateManager.applyEvent" should "refresh file workflow suggestions after path edits" in {
    val tempRoot   = Files.createTempDirectory("workflow-suggestions")
    val projectDir = Files.createDirectory(tempRoot.resolve("project"))
    Files.createDirectory(tempRoot.resolve("private"))

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              filename = "notes.scala",
              path = tempRoot.resolve("p").toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('r')).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow shouldBe a[OpenFileWorkflowState]
      workflow.path shouldBe tempRoot.resolve("pr").toString
      workflow.suggestions.map(_.value) should contain(projectDir.toString)
      workflow.selectedSuggestionIndex shouldBe 0
    finally
      Files.deleteIfExists(projectDir)
      Files.deleteIfExists(tempRoot.resolve("private"))
      Files.deleteIfExists(tempRoot)
  }

  it should "mark missing directories and require confirmation before save-as creates them" in {
    val tempRoot   = Files.createTempDirectory("workflow-save")
    val targetDir  = tempRoot.resolve("new").resolve("nested")
    val targetFile = targetDir.resolve("notes.scala")
    val bufferId   = BufferId(0)
    val bufferText = "object Notes"

    try
      val stateManager = createStateManager()
      stateManager
        .updateState { state =>
          val existing = state.persisted.buffers(bufferId)
          val buffer = existing
            .copy(document = existing.document.copy(content = com.serenity.rope.Rope(bufferText), isDirty = true))
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
        }
        .unsafeRunSync()

      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.SaveAs,
              filename = "notes.scala",
              path = targetDir.toString
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(TabKey).unsafeRunSync()

      val refreshed = currentWorkflow(stateManager)
      refreshed shouldBe a[SaveAsFileWorkflowState]
      refreshed.missingPathSegments shouldBe List("new", "nested")
      refreshed.confirmCreateDirectories shouldBe false

      stateManager.applyEvent(Enter).unsafeRunSync()

      val awaitingConfirmation = currentWorkflow(stateManager)
      awaitingConfirmation.confirmCreateDirectories shouldBe true
      Files.exists(targetFile) shouldBe false

      stateManager.applyEvent(Enter).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      Files.readString(targetFile) shouldBe bufferText
      updatedState.persisted.buffers(bufferId).document.filePath shouldBe Some(targetFile)
      updatedState.persisted.buffers(bufferId).document.isDirty shouldBe false
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(targetDir)
      Files.deleteIfExists(tempRoot.resolve("new"))
      Files.deleteIfExists(tempRoot)
  }

  it should "create missing directories and save in one step via the explicit create-directory action" in {
    val tempRoot   = Files.createTempDirectory("workflow-save-explicit")
    val targetDir  = tempRoot.resolve("new").resolve("nested")
    val targetFile = targetDir.resolve("notes.scala")
    val bufferId   = BufferId(0)
    val bufferText = "object Notes"

    try
      val stateManager = createStateManager()
      stateManager
        .updateState { state =>
          val existing = state.persisted.buffers(bufferId)
          val buffer = existing
            .copy(document = existing.document.copy(content = com.serenity.rope.Rope(bufferText), isDirty = true))
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
        }
        .unsafeRunSync()

      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.SaveAs,
              filename = "notes.scala",
              path = targetDir.toString
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(TabKey).unsafeRunSync()

      val refreshed = currentWorkflow(stateManager)
      refreshed.missingPathSegments shouldBe List("new", "nested")

      // A single ModalCreateDirectory -- not a second submit -- both creates the directories and completes the save.
      stateManager.applyEvent(ModalCreateDirectory).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      Files.readString(targetFile) shouldBe bufferText
      updatedState.persisted.buffers(bufferId).document.filePath shouldBe Some(targetFile)
      updatedState.persisted.buffers(bufferId).document.isDirty shouldBe false
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(targetDir)
      Files.deleteIfExists(tempRoot.resolve("new"))
      Files.deleteIfExists(tempRoot)
  }

  it should "carry the buffer's current rich formatting into the save-as workflow it opens" in {
    val bufferId = BufferId(0)
    val richDocument = RichTextDocument(
      List(RichTextParagraph(List(RichTextRun("bold text", RichTextStyle(marks = Set(InlineMark.Bold))))))
    )

    val stateManager = createStateManager()
    stateManager
      .updateState { state =>
        val existing = state.persisted.buffers(bufferId)
        val buffer = existing.copy(
          document = existing.document.copy(content = com.serenity.rope.Rope("bold text"), isDirty = true),
          richText = existing.richText.copy(richTextDocument = Some(richDocument))
        )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(SaveAsFile).unsafeRunSync()

    val workflow = currentWorkflow(stateManager)
    workflow shouldBe a[SaveAsFileWorkflowState]
    workflow.bufferHasRichFormatting shouldBe true
    // A brand-new/never-saved buffer's Save As has no filename to derive an extension from -- defaults to plain
    // text rather than "Unknown" -- and plain text can't preserve rich formatting either, so the warning already
    // applies at the default (empty) filename.
    workflow.detectedFileType shouldBe com.serenity.io.FileType.Text
    workflow.wouldLoseFormatting shouldBe true
  }

  it should "open a file from the workflow modal into a new focused buffer" in {
    val tempRoot   = Files.createTempDirectory("workflow-open")
    val targetFile = tempRoot.resolve("notes.scala")
    Files.writeString(targetFile, "val answer = 42")

    try
      val stateManager = createStateManager()
      val initialState = stateManager.getCurrentState.unsafeRunSync()

      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              filename = "notes.scala",
              path = tempRoot.toString
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(Enter).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      updatedState.persisted.bufferOrder should have size (initialState.persisted.bufferOrder.size + 1)
      val openedBufferId = updatedState.persisted.bufferOrder.last
      updatedState.focusedBufferId shouldBe Some(openedBufferId)
      updatedState.persisted.buffers(openedBufferId).document.filePath shouldBe Some(targetFile)
      updatedState.persisted.buffers(openedBufferId).document.content.collect() shouldBe "val answer = 42"
      updatedState.persisted.recentFiles shouldBe List(targetFile)
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(tempRoot)
  }

  it should "append a trailing separator when accepting a directory suggestion in the path field with tab" in {
    val tempRoot   = Files.createTempDirectory("workflow-directory-accept")
    val projectDir = Files.createDirectory(tempRoot.resolve("project"))

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempRoot.resolve("pro").toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('j')).unsafeRunSync()
      stateManager.applyEvent(TabKey).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.path shouldBe projectDir.toString + java.io.File.separator
    finally
      Files.deleteIfExists(projectDir)
      Files.deleteIfExists(tempRoot)
  }

  it should "accept a filename suggestion with tab in open workflow mode" in {
    val tempRoot   = Files.createTempDirectory("workflow-file-accept")
    val targetFile = tempRoot.resolve("notes.scala")
    Files.writeString(targetFile, "val answer = 42")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempRoot.toString,
              activeField = FileWorkflowField.Filename
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('n')).unsafeRunSync()
      stateManager.applyEvent(TabKey).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.filename shouldBe "notes.scala"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(tempRoot)
  }

  it should "offer matching readable files when open workflow focus is on the filename field" in {
    val tempRoot   = Files.createTempDirectory("workflow-open-files")
    val targetFile = tempRoot.resolve("notes.scala")
    val otherFile  = tempRoot.resolve("draft.txt")
    Files.writeString(targetFile, "val answer = 42")
    Files.writeString(otherFile, "draft")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempRoot.toString,
              activeField = FileWorkflowField.Filename
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('n')).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.filename shouldBe "n"
      workflow.suggestions.map(_.value) should contain("notes.scala")
      workflow.suggestions.map(_.value) should not contain "draft.txt"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(otherFile)
      Files.deleteIfExists(tempRoot)
  }

  it should "avoid filesystem suggestions while editing a remote storage URI" in {
    val stateManager = createStateManager()
    stateManager
      .showModal(
        Modal.FileWorkflow(
          FileWorkflowState(
            mode = FileWorkflowMode.Open,
            path = "https://example.com/docs/note",
            activeField = FileWorkflowField.Path
          )
        )
      )
      .unsafeRunSync()

    stateManager.applyEvent(InsertChar('s')).unsafeRunSync()

    val workflow = currentWorkflow(stateManager)
    workflow.path shouldBe "https://example.com/docs/notes"
    workflow.suggestions shouldBe Nil
    workflow.missingPathSegments shouldBe Nil
  }

  it should "keep the modal open and surface a visible status when open target is missing" in {
    val tempRoot = Files.createTempDirectory("workflow-open-missing")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              filename = "missing.scala",
              path = tempRoot.toString
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(Enter).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.statusMessage shouldBe Some(s"File not found: ${tempRoot.resolve("missing.scala")}")
    finally Files.deleteIfExists(tempRoot)
  }

  it should "keep the modal open and surface a visible status when open target is remote storage" in {
    val stateManager = createStateManager()
    stateManager
      .showModal(
        Modal.FileWorkflow(
          FileWorkflowState(
            mode = FileWorkflowMode.Open,
            path = "https://example.com/docs/notes.md"
          )
        )
      )
      .unsafeRunSync()

    stateManager.applyEvent(Enter).unsafeRunSync()

    val workflow = currentWorkflow(stateManager)
    workflow.statusMessage shouldBe Some("Remote storage is not supported yet: https://example.com/docs/notes.md")
    stateManager.getCurrentState.unsafeRunSync().persisted.recentFiles shouldBe Nil
  }

  it should "keep the modal open and surface a visible status when save-as target is remote storage" in {
    val bufferId = BufferId(0)

    val stateManager = createStateManager()
    stateManager
      .updateState { state =>
        val existing = state.persisted.buffers(bufferId)
        val buffer = existing
          .copy(document = existing.document.copy(content = com.serenity.rope.Rope("remote draft"), isDirty = true))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()
    stateManager
      .showModal(
        Modal.FileWorkflow(
          FileWorkflowState(
            mode = FileWorkflowMode.SaveAs,
            filename = "notes.md",
            path = "s3://serenity-docs/drafts"
          )
        )
      )
      .unsafeRunSync()

    stateManager.applyEvent(Enter).unsafeRunSync()

    val workflow = currentWorkflow(stateManager)
    workflow.statusMessage shouldBe Some("Remote storage is not supported yet: s3://serenity-docs/drafts/notes.md")
    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.buffers(bufferId).document.filePath shouldBe None
    state.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  it should "keep the modal open and surface a visible status when save-as targets an unwritable path" in {
    val bufferId = BufferId(0)
    // A regular file standing in for the target "directory" -- writing notes.md underneath it fails regardless of
    // filesystem permissions or which user runs the test, unlike a permission-bit-based fixture.
    val blockingFile = Files.createTempFile("workflow-unwritable", "")

    try
      val stateManager = createStateManager()
      stateManager
        .updateState { state =>
          val existing = state.persisted.buffers(bufferId)
          val buffer = existing
            .copy(document = existing.document.copy(content = com.serenity.rope.Rope("undeliverable"), isDirty = true))
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
        }
        .unsafeRunSync()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.SaveAs,
              filename = "notes.md",
              path = blockingFile.toString
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(Enter).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.statusMessage shouldBe defined
      workflow.statusMessage.get should startWith("Could not save:")
      val state = stateManager.getCurrentState.unsafeRunSync()
      state.persisted.buffers(bufferId).document.filePath shouldBe None
      state.persisted.buffers(bufferId).document.isDirty shouldBe true
    finally Files.deleteIfExists(blockingFile)
  }

  it should "block a lossy rich-document overwrite and open Save As with a visible reason" in {
    val sourceFile = Files.createTempFile("workflow-lossy-save", ".docx")
    val reason = s"Saving $sourceFile would discard unsupported rich document content. Use Save As to write a new file."

    try
      val stateManager = createStateManager()
      stateManager
        .updateState { state =>
          val existing = state.persisted.buffers(BufferId(0))
          val buffer = existing.copy(
            document = existing.document.copy(
              content = Rope("edited text"),
              filePath = Some(sourceFile),
              isDirty = true
            ),
            richText = existing.richText.copy(
              richTextFidelity = Some(RichTextFidelity(unsupportedElements = Set("tbl")))
            )
          )
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(BufferId(0), buffer)))
        }
        .unsafeRunSync()

      stateManager.applyEvent(SaveFile).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow shouldBe a[SaveAsFileWorkflowState]
      workflow.statusMessage shouldBe Some(reason)
      stateManager.getCurrentState.unsafeRunSync().persisted.buffers(BufferId(0)).document.filePath shouldBe Some(
        sourceFile
      )
      stateManager.getCurrentState.unsafeRunSync().persisted.buffers(BufferId(0)).document.isDirty shouldBe true
    finally Files.deleteIfExists(sourceFile)
  }

  it should "begin in the Path field when the open dialog is opened in TUI mode (no native dialog)" in {
    val stateManager = createStateManager()
    stateManager.applyEvent(OpenFile).unsafeRunSync()

    val workflow = currentWorkflow(stateManager)
    workflow shouldBe a[OpenFileWorkflowState]
    workflow.activeField shouldBe FileWorkflowField.Path
  }

  it should "include readable files alongside directories in the open-dialog Path field suggestions" in {
    val tempDir  = Files.createTempDirectory("open-path-dir-and-file")
    val subDir   = Files.createDirectory(tempDir.resolve("documents"))
    val textFile = Files.createTempFile(tempDir, "notes", ".txt")
    Files.writeString(textFile, "hello")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempDir.toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('/')).unsafeRunSync()

      val workflow        = currentWorkflow(stateManager)
      val suggestionPaths = workflow.suggestions.map(_.value)
      suggestionPaths should contain(subDir.toString)
      suggestionPaths should contain(textFile.toString)
      workflow.suggestions.find(_.value == subDir.toString).map(_.isDirectory) shouldBe Some(true)
      workflow.suggestions.find(_.value == textFile.toString).map(_.isDirectory) shouldBe Some(false)
      workflow.suggestions.indexWhere(_.value == subDir.toString) should be <
        workflow.suggestions.indexWhere(_.value == textFile.toString)
    finally
      Files.deleteIfExists(textFile)
      Files.deleteIfExists(subDir)
      Files.deleteIfExists(tempDir)
  }

  it should "open a file immediately when its path suggestion is accepted with Tab in the open dialog" in {
    val tempDir  = Files.createTempDirectory("open-tab-file-path")
    val textFile = Files.createTempFile(tempDir, "notes", ".txt")
    Files.writeString(textFile, "tab-opened content")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = tempDir.toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(InsertChar('/')).unsafeRunSync()

      val beforeTab = currentWorkflow(stateManager)
      beforeTab.suggestions.map(_.value) should contain(textFile.toString)

      stateManager.applyEvent(TabKey).unsafeRunSync()

      val finalState   = stateManager.getCurrentState.unsafeRunSync()
      val openedBuffer = finalState.persisted.buffers.values.find(_.document.filePath.contains(textFile))
      finalState.modalSurface shouldBe None
      openedBuffer.map(_.document.content.collect()) shouldBe Some("tab-opened content")
    finally
      Files.deleteIfExists(textFile)
      Files.deleteIfExists(tempDir)
  }

  it should "navigate into a directory when Enter is pressed on a path that resolves to a directory" in {
    val tempDir = Files.createTempDirectory("open-enter-dir")
    val subDir  = Files.createDirectory(tempDir.resolve("inner"))
    val subFile = Files.createTempFile(subDir, "notes", ".txt")
    Files.writeString(subFile, "inner content")

    try
      val stateManager = createStateManager()
      stateManager
        .showModal(
          Modal.FileWorkflow(
            FileWorkflowState(
              mode = FileWorkflowMode.Open,
              path = subDir.toString,
              activeField = FileWorkflowField.Path
            )
          )
        )
        .unsafeRunSync()

      stateManager.applyEvent(Enter).unsafeRunSync()

      val workflow = currentWorkflow(stateManager)
      workflow.statusMessage shouldBe None
      workflow.path should startWith(subDir.toString)
    finally
      Files.deleteIfExists(subFile)
      Files.deleteIfExists(subDir)
      Files.deleteIfExists(tempDir)
  }
