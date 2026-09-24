package com.serenity.state.manager

import com.serenity.DockedPanelFixtures
import com.serenity.command.{CommandRegistry, PanelKind}
import com.serenity.config.CommentDisplayMode
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEventReducer, CommandRunnerPanelSelections, PanelStateReducer}
import com.serenity.state.undo.{HistoryEntry, UndoState}
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[PanelTransitions]] as plain values. The pinned-panel shell used to commit several of these unvalidated -- the
  * markdown-preview unpin rewrote focus, the workspace tree and the maximised node with no check (#1183) -- so every
  * committed state here is also checked against `AppStateValidation`.
  */
class PanelTransitionsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def valid(state: AppState): AppState =
    AppStateValidation.validationErrors(state) shouldBe Nil
    state

  private val previewId = SurfaceId("preview")

  /** A pinned markdown preview that is focused and maximised -- the three things removing it has to unwind. */
  private val focusedMaximisedPreview: AppState =
    val docked = DockedPanelFixtures.dock(
      AppState.initial,
      previewId,
      SurfaceContent.MarkdownPreview(BufferId(0), "notes.md"),
      PanelPosition.Right,
      40
    )
    val expanded = DockedPanelFixtures.expand(docked, previewId)
    expanded.copy(persisted = expanded.persisted.copy(focus = Focus.Surface(previewId)))

  private def kindOf(state: AppState, kind: PanelKind): List[UiSurface] =
    state.runtime.uiSurfaces.filter(surface => PanelTransitions.panelKindOf(surface.content).contains(kind))

  "removePanelKind" should "unwind focus, the workspace tree and the maximised node together with the surface" in {
    val removed = PanelTransitions.removePanelKind(PanelKind.MarkdownPreview)(valid(focusedMaximisedPreview))

    valid(removed)
    kindOf(removed, PanelKind.MarkdownPreview) shouldBe Nil
    removed.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    removed.persisted.layout.maximizedWorkspaceNodeId shouldBe None
    removed.persisted.layout.workspaceTree.flatMap(_.positionForSurface(previewId)) shouldBe None
  }

  it should "leave a state without that kind unchanged" in {
    PanelTransitions.removePanelKind(PanelKind.Outline)(AppState.initial) shouldBe AppState.initial
  }

  "upsertPanelKind" should "dock a new panel of the kind at the requested edge" in {
    val pinned = PanelTransitions.upsertPanelKind(
      PanelKind.Diagnostics,
      SurfaceContent.Diagnostics(Nil),
      PanelPosition.Bottom,
      10
    )(AppState.initial)

    valid(pinned)
    kindOf(pinned, PanelKind.Diagnostics).flatMap(surface =>
      pinned.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id))
    ) shouldBe List(PanelPosition.Bottom)
  }

  "panelChange" should "record the pre-change panel layout as an undo entry in the same model" in {
    val model = Model(focusedMaximisedPreview, UndoState(), Map.empty)

    val next = PanelTransitions.panelChange(
      model,
      PanelTransitions.removePanelKind(PanelKind.MarkdownPreview),
      refreshSelections = false
    )

    valid(next.app)
    kindOf(next.app, PanelKind.MarkdownPreview) shouldBe Nil
    next.undo.undoStack shouldBe Vector(HistoryEntry.PanelChange.capture(focusedMaximisedPreview))
  }

  it should "record nothing when the change is a no-op" in {
    val model = Model(AppState.initial, UndoState(), Map.empty)

    val next = PanelTransitions.panelChange(
      model,
      PanelTransitions.removePanelKind(PanelKind.Outline),
      refreshSelections = false
    )

    next shouldBe model
  }

  it should "refresh an open command runner's panel selections when asked" in {
    val withRunner = AppEventReducer.reduce(ToggleCommandRunner, AppState.initial, CommandRegistry.withToggleUI).state
    val model      = Model(withRunner, UndoState(), Map.empty)

    val next = PanelTransitions.panelChange(
      model,
      PanelTransitions.upsertPanelKind(PanelKind.Diagnostics, SurfaceContent.Diagnostics(Nil), PanelPosition.Left, 30),
      refreshSelections = true
    )

    val runner = valid(next.app).runtime.uiSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.CommandPalette(runner), _, _) => runner
    }
    val expected = CommandRunnerPanelSelections.fromState(next.app)
    runner.map(r => expected.forall((id, index) => r.optionSelections.get(id).contains(index))) shouldBe Some(true)
  }

  "pinPlan" should "commit a docked diagnostics panel at its default size" in {
    PanelTransitions.pinPlan(PanelKind.Diagnostics, PanelPosition.Bottom, AppState.initial) match
      case PanelPinPlan.Commit(update) =>
        val pinned = valid(update(AppState.initial))
        kindOf(pinned, PanelKind.Diagnostics).flatMap(surface =>
          PanelStateReducer.currentSize(surface.id, pinned)
        ) shouldBe List(10)
      case other => fail(s"Expected a commit, got $other")
  }

  it should "ask for the explorer root to be loaded when no explorer is pinned yet" in {
    PanelTransitions.pinPlan(PanelKind.Explorer, PanelPosition.Left, AppState.initial) shouldBe
      PanelPinPlan.LoadExplorerRoot(30)
  }

  it should "report that comments are hidden instead of pinning them when comment display is off" in {
    val off = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(config = AppState.initial.persisted.config.withCommentDisplayMode(CommentDisplayMode.Off))
    )

    PanelTransitions.pinPlan(PanelKind.Comments, PanelPosition.Right, off) shouldBe
      PanelPinPlan.Report("Comments are hidden -- comment display is turned off in Settings.")
  }

  it should "ignore a markdown preview request without a focused Markdown buffer" in {
    PanelTransitions.pinPlan(PanelKind.MarkdownPreview, PanelPosition.Right, AppState.initial) match
      case PanelPinPlan.Ignore(_) => succeed
      case other                  => fail(s"Expected the request to be ignored, got $other")
  }

  it should "commit a preview of the focused Markdown buffer" in {
    val markdown = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = AppState.initial.persisted.buffers.map { (id, buffer) =>
        id -> buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Markdown)))
      })
    )

    PanelTransitions.pinPlan(PanelKind.MarkdownPreview, PanelPosition.Right, markdown) match
      case PanelPinPlan.Commit(update) =>
        kindOf(valid(update(markdown)), PanelKind.MarkdownPreview).map(_.content) shouldBe
          List(SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"))
      case other => fail(s"Expected a commit, got $other")
  }

  "withMarkdownPreviewWindowBuffer" should "record which buffer the preview window follows" in {
    val following = PanelTransitions.withMarkdownPreviewWindowBuffer(AppState.initial, Some(BufferId(0)))

    valid(following).runtime.markdownPreviewWindowBuffer shouldBe Some(BufferId(0))
    valid(
      PanelTransitions.withMarkdownPreviewWindowBuffer(following, None)
    ).runtime.markdownPreviewWindowBuffer shouldBe
      None
  }
