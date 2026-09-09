package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.io.FileManager
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Exercises [[StateManagerFileWorkflow]] on its own, without a composed `StateManager`: every collaborator is a plain
  * closure, so a submit can be observed as "what did it ask its owner to do" rather than inferred from end-to-end
  * state.
  */
class StateManagerFileWorkflowSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val surfaceId = SurfaceId("file-workflow")
  private val bufferId  = BufferId(7)
  private val paneId    = PaneId(1)

  private def stateWith(workflow: FileWorkflowState, focusedPane: Boolean = true): AppState =
    val base = AppState.initial
    val layout =
      if focusedPane then
        com.serenity.ui.layout.Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        )
      else com.serenity.ui.layout.Layout(editorPanes = Map.empty, activeEditorPaneId = None)
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(bufferId -> Buffer.fromString(bufferId, "hello")),
        bufferOrder = List(bufferId),
        layout = layout
      ),
      runtime = base.runtime.copy(modalStack =
        List(ModalDialog(surfaceId, Modal.FileWorkflow(workflow), ModalPlacement.Centered))
      )
    )

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val saved: Ref[IO, List[(BufferId, Path)]],
      val continued: Ref[IO, List[(SurfaceId, BufferId)]],
      val committed: Ref[IO, List[AppState]],
      val fileWorkflow: StateManagerFileWorkflow
  ):

    def currentWorkflow: FileWorkflowState =
      stateRef.get
        .unsafeRunSync()
        .runtime
        .modalStack
        .find(_.id == surfaceId)
        .collect { case ModalDialog(_, Modal.FileWorkflow(workflow), _) => workflow }
        .getOrElse(fail("Expected the file workflow dialog to still be present"))

  private def harness(workflow: FileWorkflowState, saveResult: IO[Unit] = IO.unit, focusedPane: Boolean = true) =
    val stateRef  = Ref.of[IO, AppState](stateWith(workflow, focusedPane)).unsafeRunSync()
    val saved     = Ref.of[IO, List[(BufferId, Path)]](Nil).unsafeRunSync()
    val continued = Ref.of[IO, List[(SurfaceId, BufferId)]](Nil).unsafeRunSync()
    val committed = Ref.of[IO, List[AppState]](Nil).unsafeRunSync()

    def updateSurface(id: SurfaceId, updated: FileWorkflowState): IO[Unit] =
      stateRef.update { state =>
        state.copy(runtime = state.runtime.copy(modalStack = state.runtime.modalStack.map {
          case dialog if dialog.id == id => dialog.copy(modal = Modal.FileWorkflow(updated))
          case other                     => other
        }))
      }

    def workflowSurface(state: AppState, id: SurfaceId): Option[FileWorkflowState] =
      state.runtime.modalStack.find(_.id == id).collect {
        case ModalDialog(_, Modal.FileWorkflow(current), _) => current
      }

    new Harness(
      stateRef,
      saved,
      continued,
      committed,
      new StateManagerFileWorkflow(
        stateRef,
        NoOpLogger.impl[IO],
        new FileManager(),
        (newState, _) => committed.update(_ :+ newState) >> stateRef.set(newState),
        updateSurface,
        workflowSurface,
        state =>
          state.persisted.layout.activeEditorPaneId
            .flatMap(state.persisted.layout.editorPanes.get)
            .flatMap(_.bufferId),
        (id, path) => saved.update(_ :+ (id, path)) >> saveResult,
        (id, buffer) => continued.update(_ :+ (id, buffer))
      )
    )

  "StateManagerFileWorkflow" should "save to the resolved target path and hand off to its owner exactly once" in {
    val directory = Files.createTempDirectory("file-workflow-save-as")
    try
      val fixture =
        harness(SaveAsFileWorkflowState(filename = "notes.txt", path = directory.toString))

      fixture.fileWorkflow.submitFileWorkflowEffect(surfaceId).unsafeRunSync()

      fixture.saved.get.unsafeRunSync() shouldBe List(bufferId -> directory.resolve("notes.txt").normalize())
      fixture.continued.get.unsafeRunSync() shouldBe List(surfaceId -> bufferId)
    finally Files.deleteIfExists(directory)
  }

  it should "ask for confirmation instead of saving when the target directory does not exist yet" in {
    val directory = Files.createTempDirectory("file-workflow-missing-dirs")
    try
      val target = directory.resolve("reports").resolve("2026")
      val fixture = harness(
        SaveAsFileWorkflowState(
          filename = "notes.txt",
          path = target.toString,
          missingPathSegments = List("reports", "2026")
        )
      )

      fixture.fileWorkflow.submitFileWorkflowEffect(surfaceId).unsafeRunSync()

      fixture.saved.get.unsafeRunSync() shouldBe Nil
      fixture.continued.get.unsafeRunSync() shouldBe Nil
      fixture.currentWorkflow.confirmCreateDirectories shouldBe true
    finally Files.deleteIfExists(directory)
  }

  it should "save on an explicit create-directories request without a second submit" in {
    val directory = Files.createTempDirectory("file-workflow-create-dirs")
    try
      val target = directory.resolve("reports")
      val fixture = harness(
        SaveAsFileWorkflowState(
          filename = "notes.txt",
          path = target.toString,
          missingPathSegments = List("reports")
        )
      )

      fixture.fileWorkflow.createFileWorkflowDirectoriesEffect(surfaceId).unsafeRunSync()

      fixture.saved.get.unsafeRunSync() shouldBe List(bufferId -> target.resolve("notes.txt").normalize())
      fixture.continued.get.unsafeRunSync() shouldBe List(surfaceId -> bufferId)
    finally Files.deleteIfExists(directory)
  }

  it should "report an unsupported remote target rather than saving to it" in {
    val fixture = harness(SaveAsFileWorkflowState(filename = "notes.txt", path = "s3://bucket/drafts"))

    fixture.fileWorkflow.submitFileWorkflowEffect(surfaceId).unsafeRunSync()

    fixture.saved.get.unsafeRunSync() shouldBe Nil
    fixture.continued.get.unsafeRunSync() shouldBe Nil
    fixture.currentWorkflow.statusMessage shouldBe Some(
      "Remote storage is not supported yet: s3://bucket/drafts/notes.txt"
    )
  }

  it should "surface a save failure and not hand off to its owner" in {
    val directory = Files.createTempDirectory("file-workflow-save-failure")
    try
      val fixture = harness(
        SaveAsFileWorkflowState(filename = "notes.txt", path = directory.toString),
        saveResult = IO.raiseError(new RuntimeException("disk full"))
      )

      fixture.fileWorkflow.submitFileWorkflowEffect(surfaceId).unsafeRunSync()

      fixture.continued.get.unsafeRunSync() shouldBe Nil
      fixture.currentWorkflow.statusMessage shouldBe Some("Could not save: disk full")
    finally Files.deleteIfExists(directory)
  }

  it should "neither save nor hand off when no editor pane holds a buffer" in {
    val directory = Files.createTempDirectory("file-workflow-no-buffer")
    try
      val fixture = harness(
        SaveAsFileWorkflowState(filename = "notes.txt", path = directory.toString),
        focusedPane = false
      )

      fixture.fileWorkflow.submitFileWorkflowEffect(surfaceId).unsafeRunSync()

      fixture.saved.get.unsafeRunSync() shouldBe Nil
      fixture.continued.get.unsafeRunSync() shouldBe Nil
    finally Files.deleteIfExists(directory)
  }

  it should "commit a loaded buffer when an Open workflow targets a readable file" in {
    val directory = Files.createTempDirectory("file-workflow-open")
    val target    = Files.writeString(directory.resolve("notes.txt"), "opened content")
    try
      val fixture = harness(OpenFileWorkflowState(filename = "notes.txt", path = directory.toString))

      fixture.fileWorkflow.submitFileWorkflowEffect(surfaceId).unsafeRunSync()

      val committedStates = fixture.committed.get.unsafeRunSync()
      committedStates.size shouldBe 1
      committedStates.head.persisted.buffers.values.flatMap(_.document.filePath).toList should contain(target)
      committedStates.head.persisted.recentFiles should contain(target)
      fixture.saved.get.unsafeRunSync() shouldBe Nil
      fixture.continued.get.unsafeRunSync() shouldBe Nil
    finally
      Files.deleteIfExists(target)
      Files.deleteIfExists(directory)
  }

  it should "report a missing file rather than committing anything when an Open target does not exist" in {
    val directory = Files.createTempDirectory("file-workflow-open-missing")
    try
      val fixture = harness(OpenFileWorkflowState(filename = "absent.txt", path = directory.toString))

      fixture.fileWorkflow.submitFileWorkflowEffect(surfaceId).unsafeRunSync()

      fixture.committed.get.unsafeRunSync() shouldBe Nil
      fixture.currentWorkflow.statusMessage shouldBe Some(
        s"File not found: ${directory.resolve("absent.txt").normalize()}"
      )
    finally Files.deleteIfExists(directory)
  }

  it should "descend into a directory rather than failing when an Open target is a directory" in {
    val directory = Files.createTempDirectory("file-workflow-open-directory")
    val child     = Files.createDirectory(directory.resolve("nested"))
    try
      val fixture = harness(OpenFileWorkflowState(filename = "nested", path = directory.toString))

      fixture.fileWorkflow.submitFileWorkflowEffect(surfaceId).unsafeRunSync()

      fixture.committed.get.unsafeRunSync() shouldBe Nil
      fixture.currentWorkflow.path shouldBe child.normalize().toString + java.io.File.separator
      fixture.currentWorkflow.statusMessage shouldBe None
    finally
      Files.deleteIfExists(child)
      Files.deleteIfExists(directory)
  }
