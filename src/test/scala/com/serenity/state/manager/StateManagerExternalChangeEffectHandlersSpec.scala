package com.serenity.state.manager

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.io.FileManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[StateManagerEffectHandlers]]'s external file-change detection (#1623): the off-dispatcher
  * `observe*ExternalRevisionEffect` reads, the on-dispatcher `resolveExternalRevisionEffect` decision, and
  * `openBufferPathsEffect`. Split out of `StateManagerEffectHandlersSpec`; both specs share
  * [[StateManagerEffectHandlersHarness]].
  */
class StateManagerExternalChangeEffectHandlersSpec
    extends AnyFlatSpec
    with Matchers
    with StateManagerEffectHandlersHarness:

  private def checkFocused(handlers: StateManagerEffectHandlers): IO[Unit] =
    handlers.observeFocusedExternalRevisionEffect.flatMap(_.traverse_(handlers.resolveExternalRevisionEffect))

  private def checkBuffer(handlers: StateManagerEffectHandlers, id: BufferId): IO[Unit] =
    handlers.observeExternalRevisionEffect(id).flatMap(_.traverse_(handlers.resolveExternalRevisionEffect))

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

  "The focus-gain external-change check" should "silently reload a clean buffer whose file changed on disk (#1623)" in {
    val path = Files.createTempFile("focus-check-clean", ".md")
    Files.writeString(path, "original")
    val opened  = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val fixture = harness(focusedBufferState(opened))

    Files.writeString(path, "changed externally")

    checkFocused(fixture.handlers).unsafeRunSync()

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

    checkFocused(fixture.handlers).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() should contain(s"openReloadConflictModal:$bufferId:${path.getFileName}")
    fixture.calls.get.unsafeRunSync().exists(_.startsWith("reloadBuffer")) shouldBe false
  }

  it should "do nothing when the focused buffer's file has not changed" in {
    val path = Files.createTempFile("focus-check-unchanged", ".md")
    Files.writeString(path, "original")
    val opened  = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val fixture = harness(focusedBufferState(opened))

    checkFocused(fixture.handlers).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe Nil
  }

  it should "do nothing when no buffer is focused" in {
    val fixture = harness()

    checkFocused(fixture.handlers).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe Nil
  }

  "The watcher external-change check" should "check any given buffer, not only the focused one (#1623)" in {
    val path = Files.createTempFile("watch-check-unfocused", ".md")
    Files.writeString(path, "original")
    val opened = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val dirty =
      opened.copy(document = opened.document.copy(content = com.serenity.rope.Rope("my edit"), isDirty = true))
    // No focus wiring at all -- state.focusedBufferId is None, unlike focusedBufferState.
    val state   = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> dirty)))
    val fixture = harness(state)

    Files.writeString(path, "changed externally")

    checkBuffer(fixture.handlers, bufferId).unsafeRunSync()

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

    checkBuffer(fixture.handlers, bufferId).unsafeRunSync()

    fixture.calls.get.unsafeRunSync().exists(_.startsWith("openReloadConflictModal")) shouldBe false
  }

  it should "drop an observation made before the buffer recorded a newer revision, such as its own save's (#1564)" in {
    val path = Files.createTempFile("watch-check-stale", ".md")
    Files.writeString(path, "original")
    val opened = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    val dirty =
      opened.copy(document = opened.document.copy(content = com.serenity.rope.Rope("my edit"), isDirty = true))
    val fixture = harness(focusedBufferState(dirty))

    Files.writeString(path, "saved by this process")
    val observed = fixture.handlers.observeExternalRevisionEffect(bufferId).unsafeRunSync()
    val saved    = new FileManager().loadFile(path, bufferId).unsafeRunSync()
    fixture.stateRef.set(focusedBufferState(saved)).unsafeRunSync()
    observed.traverse_(fixture.handlers.resolveExternalRevisionEffect).unsafeRunSync()

    observed.map(_.bufferRevision) shouldBe Some(dirty.document.revision)
    fixture.calls.get.unsafeRunSync() shouldBe Nil
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
