package com.serenity.ui.host

import java.nio.file.Paths

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WindowTitleSyncSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private def stateShowing(name: String, dirty: Boolean): AppState =
    val buffer = Buffer(BufferId(1), Document(Rope("text"), filePath = Some(Paths.get(name)), isDirty = dirty))
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

  "WindowTitleSync" should "set the title only when it differs from the one last set" in {
    val program = for
      shown <- Ref.of[IO, List[String]](Nil)
      sync  <- WindowTitleSync.create(title => shown.update(_ :+ title))
      _     <- sync.sync(stateShowing("notes.md", dirty = false))
      _     <- sync.sync(stateShowing("notes.md", dirty = false))
      _     <- sync.sync(stateShowing("notes.md", dirty = true))
      _     <- sync.sync(stateShowing("notes.md", dirty = true))
      _     <- sync.sync(stateShowing("notes.md", dirty = false))
      titles <- shown.get
    yield titles

    program.unsafeRunSync() shouldBe List("notes.md — Serenity", "notes.md* — Serenity", "notes.md — Serenity")
  }
