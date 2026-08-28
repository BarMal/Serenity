package com.serenity.state.models

import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Structural invariants beyond focused-object existence and pane-to-buffer references: active-pane coherence,
  * stale/duplicate order entries, duplicate surfaces, next-ID collisions, and document-position bounds. See #858.
  */
class AppStateInvariantsSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "AppState.empty" should "be valid by its own invariants" in {
    AppState.empty.isValid shouldBe true
    AppState.empty.validationErrors shouldBe empty
  }

  "AppState.initial" should "be valid by its own invariants" in {
    AppState.initial.isValid shouldBe true
    AppState.initial.validationErrors shouldBe empty
  }

  behavior of "active-pane coherence"

  it should "reject an active editor pane that does not exist" in {
    val base = AppState.initial
    val invalid = base.copy(persisted =
      base.persisted.copy(layout = base.persisted.layout.copy(activeEditorPaneId = Some(PaneId(999))))
    )

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Active editor pane does not exist: 999")
  }

  behavior of "pane order"

  it should "reject duplicate pane-order entries" in {
    val base = AppState.initial
    val invalid = base.copy(persisted =
      base.persisted.copy(layout = base.persisted.layout.copy(paneOrder = List(PaneId(0), PaneId(0))))
    )

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Pane order contains duplicate entries: 0")
  }

  it should "reject pane-order entries referencing a pane that no longer exists" in {
    val base = AppState.initial
    val invalid = base.copy(persisted =
      base.persisted.copy(layout = base.persisted.layout.copy(paneOrder = List(PaneId(0), PaneId(7))))
    )

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Pane order references non-existent panes: 7")
  }

  behavior of "buffer order"

  it should "reject duplicate buffer-order entries" in {
    val base    = AppState.initial
    val invalid = base.copy(persisted = base.persisted.copy(bufferOrder = List(BufferId(0), BufferId(0))))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Buffer order contains duplicate entries: 0")
  }

  it should "reject buffer-order entries referencing a buffer that no longer exists" in {
    val base    = AppState.initial
    val invalid = base.copy(persisted = base.persisted.copy(bufferOrder = List(BufferId(0), BufferId(7))))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Buffer order references non-existent buffers: 7")
  }

  behavior of "surfaces"

  it should "reject duplicate UI surface IDs" in {
    val base = AppState.initial
    val surface = UiSurface(
      id = SurfaceId("dup"),
      content = SurfaceContent.ContextMenu(ContextMenu("menu", Focus.EditorPane(PaneId(0)), Nil)),
      presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val invalid =
      base.copy(runtime = base.runtime.copy(uiSurfaces = List(surface, surface)))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Duplicate UI surfaces: dup")
  }

  behavior of "next-ID allocation"

  it should "reject a next buffer ID that collides with an existing buffer" in {
    val base    = AppState.initial
    val invalid = base.copy(runtime = base.runtime.copy(nextBufferId = BufferId(0)))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Next buffer ID collides with an existing buffer: 0")
  }

  it should "reject a next pane ID that collides with an existing pane" in {
    val base    = AppState.initial
    val invalid = base.copy(runtime = base.runtime.copy(nextPaneId = PaneId(0)))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Next pane ID collides with an existing pane: 0")
  }

  // Deliberately no next-surface-ID collision check: surface IDs are also assigned by hand from fixed string
  // literals (not only via the `nextSurfaceId` counter), so a numeric coincidence there isn't evidence of a bug --
  // see the comment on `validationErrors`.

  behavior of "document-position bounds"

  private def bufferWith(
    content: String,
    cursors: List[CursorPosition] = Nil,
    selection: Option[Selection] = None,
    bookmarks: List[CursorPosition] = Nil,
    comments: List[DocumentComment] = Nil
  ): Buffer =
    val base = Buffer.fromString(BufferId(0), content)
    base.copy(
      editing = base.editing.copy(
        cursors = if cursors.isEmpty then base.editing.cursors else cursors,
        selection = selection
      ),
      annotations = Annotations(bookmarks = bookmarks, documentComments = comments)
    )

  private def stateWithBuffer(buffer: Buffer): AppState =
    val base = AppState.initial
    base.copy(persisted = base.persisted.copy(buffers = base.persisted.buffers.updated(buffer.id, buffer)))

  it should "reject a cursor beyond the buffer's line count" in {
    val invalid = stateWithBuffer(bufferWith("one line", cursors = List(CursorPosition(5, 0))))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Buffer 0 has out-of-bounds cursor(s): CursorPosition(5,0)")
  }

  it should "reject a cursor with a negative line or column" in {
    val invalid = stateWithBuffer(bufferWith("one line", cursors = List(CursorPosition(0, -1))))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Buffer 0 has out-of-bounds cursor(s): CursorPosition(0,-1)")
  }

  // Deliberately no upper column bound: this codebase routinely carries a cursor/selection column past end-of-line
  // between an edit and the next clamp (`Rope.lineColumnToOffset` clamps on read rather than rejecting), so only the
  // line -- which document position this even is -- is checked; see the comment on `validationErrors`.
  it should "accept a cursor column past its line's length, pending the next clamp" in {
    val valid = stateWithBuffer(bufferWith("abc", cursors = List(CursorPosition(0, 4))))

    valid.isValid shouldBe true
  }

  it should "reject a selection anchor/focus outside document bounds" in {
    val invalid = stateWithBuffer(
      bufferWith("abc\ndef", selection = Some(Selection(CursorPosition(0, 0), CursorPosition(9, 0))))
    )

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Buffer 0 has out-of-bounds selection position(s): CursorPosition(9,0)")
  }

  it should "reject a bookmark outside document bounds" in {
    val invalid = stateWithBuffer(bufferWith("abc", bookmarks = List(CursorPosition(3, 0))))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Buffer 0 has out-of-bounds bookmark(s): CursorPosition(3,0)")
  }

  it should "reject a document comment anchored outside document bounds" in {
    val invalid = stateWithBuffer(
      bufferWith(
        "abc",
        comments = List(DocumentComment(CursorPosition(0, 0), CursorPosition(5, 0), "note"))
      )
    )

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Buffer 0 has out-of-bounds comment position(s): CursorPosition(5,0)")
  }
