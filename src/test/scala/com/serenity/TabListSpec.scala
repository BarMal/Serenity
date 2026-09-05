package com.serenity

import java.nio.file.Paths

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The tab list (issue #1307) shows every open buffer's display name and unsaved-changes state, derived from the same
  * ordering/focus data the editor itself uses -- no new state to keep in sync.
  */
class TabListSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithBuffers(buffers: List[Buffer], order: List[BufferId], focused: BufferId): AppState =
    val pane = EditorPane.withBuffer(PaneId(0), focused)
    AppState(
      persisted = Persisted(
        layout = Layout(editorPanes = Map(PaneId(0) -> pane), activeEditorPaneId = Some(PaneId(0))),
        buffers = buffers.map(b => b.id -> b).toMap,
        focus = Focus.EditorPane(PaneId(0)),
        bufferOrder = order
      )
    )

  "TabListContent.build" should "list open buffers in bufferOrder with a file-name title and dirty flag" in {
    val clean = Buffer
      .fromString(BufferId(0), "alpha")
      .copy(document =
        Buffer.fromString(BufferId(0), "alpha").document.copy(filePath = Some(Paths.get("/tmp/clean.txt")))
      )
    val dirty = Buffer
      .fromString(BufferId(1), "beta")
      .copy(document =
        Buffer
          .fromString(BufferId(1), "beta")
          .document
          .copy(filePath = Some(Paths.get("/tmp/dirty.txt")), isDirty = true)
      )
    val state = stateWithBuffers(List(clean, dirty), List(BufferId(0), BufferId(1)), BufferId(1))

    val content = TabListContent.build(state)

    content.entries shouldBe List(
      TabListEntry(BufferId(0), "clean.txt", isDirty = false),
      TabListEntry(BufferId(1), "dirty.txt", isDirty = true)
    )
    content.activeBufferId shouldBe Some(BufferId(1))
  }

  it should "title an unsaved-to-disk buffer by its buffer id" in {
    val unsaved = Buffer.fromString(BufferId(0), "alpha")
    val state   = stateWithBuffers(List(unsaved), List(BufferId(0)), BufferId(0))

    TabListContent.build(state).entries shouldBe List(TabListEntry(BufferId(0), "Buffer 0", isDirty = false))
  }

  it should "skip buffer-order entries that no longer resolve to a live buffer" in {
    val buffer = Buffer.fromString(BufferId(0), "alpha")
    val state  = stateWithBuffers(List(buffer), List(BufferId(0), BufferId(99)), BufferId(0))

    TabListContent.build(state).entries.map(_.bufferId) shouldBe List(BufferId(0))
  }
