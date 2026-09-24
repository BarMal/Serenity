package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.SplitAxis
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[EditorTransitions]]: focus switching, pane operations and buffer creation as plain functions of `AppState`, each
  * result also checked against `AppStateValidation` -- the checks the editor shell commits them through.
  */
class EditorTransitionsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def valid(state: AppState): AppState =
    AppStateValidation.validationErrors(state) shouldBe Nil
    state

  private val twoPanes: AppState =
    com.serenity.state.core.EditorState.splitFocusedPane(AppState.initial, SplitAxis.Horizontal)

  "focused" should "move focus to the requested target" in {
    valid(EditorTransitions.focused(twoPanes, Focus.EditorPane(PaneId(0)))).persisted.focus shouldBe
      Focus.EditorPane(PaneId(0))
  }

  "paneSwitched" should "activate and focus an existing pane" in {
    val start = EditorTransitions.focused(twoPanes, Focus.EditorPane(PaneId(0)))

    val switched = EditorTransitions.paneSwitched(start, PaneId(1)).map(valid)

    switched.map(_.persisted.layout.activeEditorPaneId) shouldBe Some(Some(PaneId(1)))
    switched.map(_.persisted.focus) shouldBe Some(Focus.EditorPane(PaneId(1)))
  }

  it should "decline a pane that does not exist" in {
    EditorTransitions.paneSwitched(AppState.initial, PaneId(42)) shouldBe None
  }

  "paneInserted" should "split a new pane after the requested one, focus it and advance the pane counter" in {
    val (inserted, paneId) =
      EditorTransitions.paneInserted(AppState.initial, Some(PaneId(0)), Some(BufferId(0)), SplitAxis.Horizontal)

    valid(inserted)
    paneId shouldBe AppState.initial.runtime.nextPaneId
    inserted.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId) shouldBe Some(BufferId(0))
    inserted.persisted.layout.workspaceTree.exists(_.paneIds.contains(paneId)) shouldBe true
    inserted.persisted.focus shouldBe Focus.EditorPane(paneId)
    inserted.runtime.nextPaneId shouldBe PaneId(paneId.value + 1)
  }

  "bufferCreated" should "append a buffer under the next id, with a fallback that has only moved the id on" in {
    val path     = Path.of("notes.md")
    val creation = EditorTransitions.bufferCreated(AppState.initial, "hello", Some(path))

    creation.bufferId shouldBe AppState.initial.runtime.nextBufferId
    valid(creation.created).persisted.bufferOrder shouldBe AppState.initial.persisted.bufferOrder :+ creation.bufferId
    creation.created.persisted.buffers.get(creation.bufferId).map(_.document.content.toString) shouldBe Some("hello")
    creation.created.persisted.buffers.get(creation.bufferId).flatMap(_.document.filePath) shouldBe Some(path)
    valid(creation.idAdvanced) shouldBe AppState.initial.copy(runtime =
      AppState.initial.runtime.copy(nextBufferId = BufferId(creation.bufferId.value + 1))
    )
  }

  it should "leave a fallback past a drifted id that collides with a live buffer" in {
    val drifted  = AppState.initial.copy(runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(0)))
    val creation = EditorTransitions.bufferCreated(drifted, "", None)

    AppStateValidation.validationErrors(creation.created) should not be empty
    valid(creation.idAdvanced).runtime.nextBufferId shouldBe BufferId(1)
  }

  "bufferContentReplaced" should "replace the content, mark the buffer dirty and name the LSP document change" in {
    val path = Path.of("/tmp/notes.md")
    val withFile = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = AppState.initial.persisted.buffers.map { (id, buffer) =>
        id -> buffer.copy(document = buffer.document.copy(filePath = Some(path), language = Some(LanguageId.Markdown)))
      })
    )

    val replaced = EditorTransitions.bufferContentReplaced(withFile, BufferId(0), "# title")

    replaced.map(r => valid(r.state).persisted.buffers.get(BufferId(0))) shouldBe replaced.map(r => Some(r.buffer))
    replaced.map(_.buffer.document.isDirty) shouldBe Some(true)
    replaced.flatMap(_.documentChange) shouldBe Some((path.toUri.toString, LanguageId.Markdown, "# title"))
  }

  it should "name no LSP document change when the content is unchanged" in {
    EditorTransitions.bufferContentReplaced(AppState.initial, BufferId(0), "").flatMap(_.documentChange) shouldBe None
  }

  it should "decline a buffer that does not exist" in {
    EditorTransitions.bufferContentReplaced(AppState.initial, BufferId(42), "text") shouldBe None
  }
