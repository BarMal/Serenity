package com.serenity.state.manager

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.io.FileManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[StateManagerEffectHandlers]]'s external file-change detection (#1623):
  * `checkExternalChangesOnFocusEffect`, `checkBufferForExternalChangesEffect`, and `openBufferPathsEffect`. Split out
  * of `StateManagerEffectHandlersSpec`; both specs share [[StateManagerEffectHandlersHarness]].
  */
class StateManagerExternalChangeEffectHandlersSpec
    extends AnyFlatSpec
    with Matchers
    with StateManagerEffectHandlersHarness:

  private def focusedBufferState(buffer: Buffer): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> com.serenity.state.models.EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

  "checkExternalChangesOnFocusEffect" should "silently reload a clean buffer whose file changed on disk (#1623)" in {
    val path = Files.createTempFile("focus-check-clean", ".md")
    Files.writeString(path, "original")
    val opened  = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val fixture = harness(focusedBufferState(opened))

    Files.writeString(path, "changed externally")

    fixture.handlers.checkExternalChangesOnFocusEffect.unsafeRunSync()

    fixture.calls.get.unsafeRunSync() should contain(s"reloadBuffer:$bufferId")
    fixture.calls.get.unsafeRunSync().exists(_.startsWith("openReloadConflictModal")) shouldBe false
  }

  it should "prompt instead of reloading a dirty buffer whose file changed on disk" in {
    val path = Files.createTempFile("focus-check-dirty", ".md")
    Files.writeString(path, "original")
    val opened = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val dirty =
      opened.copy(document = opened.document.copy(content = com.serenity.rope.Rope("my edit"), isDirty = true))
    val fixture = harness(focusedBufferState(dirty))

    Files.writeString(path, "changed externally")

    fixture.handlers.checkExternalChangesOnFocusEffect.unsafeRunSync()

    fixture.calls.get.unsafeRunSync() should contain(s"openReloadConflictModal:$bufferId:${path.getFileName}")
    fixture.calls.get.unsafeRunSync().exists(_.startsWith("reloadBuffer")) shouldBe false
  }

  it should "do nothing when the focused buffer's file has not changed" in {
    val path = Files.createTempFile("focus-check-unchanged", ".md")
    Files.writeString(path, "original")
    val opened  = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val fixture = harness(focusedBufferState(opened))

    fixture.handlers.checkExternalChangesOnFocusEffect.unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe Nil
  }

  it should "do nothing when no buffer is focused" in {
    val fixture = harness()

    fixture.handlers.checkExternalChangesOnFocusEffect.unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe Nil
  }

  "checkBufferForExternalChangesEffect" should "check any given buffer, not only the focused one (#1623)" in {
    val path = Files.createTempFile("watch-check-unfocused", ".md")
    Files.writeString(path, "original")
    val opened = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val dirty =
      opened.copy(document = opened.document.copy(content = com.serenity.rope.Rope("my edit"), isDirty = true))
    // No focus wiring at all -- state.focusedBufferId is None, unlike focusedBufferState.
    val state   = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> dirty)))
    val fixture = harness(state)

    Files.writeString(path, "changed externally")

    fixture.handlers.checkBufferForExternalChangesEffect(bufferId).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() should contain(s"openReloadConflictModal:$bufferId:${path.getFileName}")
  }

  it should "not stack a second reload-conflict prompt when one is already open (code review finding, PR #1664)" in {
    val path = Files.createTempFile("watch-check-already-prompted", ".md")
    Files.writeString(path, "original")
    val opened = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val dirty =
      opened.copy(document = opened.document.copy(content = com.serenity.rope.Rope("my edit"), isDirty = true))
    val stateWithOpenPrompt = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> dirty)),
      runtime = AppState.initial.runtime.copy(
        modalStack = List(
          ModalDialog(
            SurfaceId("existing-conflict"),
            Modal.ReloadConflict(ReloadConflictState(bufferId, "watch-check-already-prompted.md")),
            ModalPlacement.Centered
          )
        )
      )
    )
    val fixture = harness(stateWithOpenPrompt)

    Files.writeString(path, "changed externally, again")

    fixture.handlers.checkBufferForExternalChangesEffect(bufferId).unsafeRunSync()

    fixture.calls.get.unsafeRunSync().exists(_.startsWith("openReloadConflictModal")) shouldBe false
  }

  "openBufferPathsEffect" should "report every open buffer's file path keyed by its buffer id" in {
    val pathA   = Files.createTempFile("open-paths-a", ".md")
    val pathB   = Files.createTempFile("open-paths-b", ".md")
    val bufferA = Buffer.fromFile(BufferId(1), pathA, "a")
    val bufferB = Buffer.fromFile(BufferId(2), pathB, "b")
    val unsaved = Buffer.fromString(BufferId(3), "no path yet")
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(buffers = Map(bufferA.id -> bufferA, bufferB.id -> bufferB, unsaved.id -> unsaved))
    )
    val fixture = harness(state)

    fixture.handlers.openBufferPathsEffect.unsafeRunSync() shouldBe Map(pathA -> bufferA.id, pathB -> bufferB.id)
  }
