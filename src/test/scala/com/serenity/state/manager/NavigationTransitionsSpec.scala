package com.serenity.state.manager

import com.serenity.animation.{AnimationConfig, AnimationOwner}
import com.serenity.command.{CommentsIntent, NavigationIntent}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.MotionPreset
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AnimationEffect, AppEffect, ReducerResult}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[NavigationTransitions]] as plain functions of `AppState`: every applied result is also checked against
  * `AppStateValidation`, since the shell used to commit several of these writes without validating them (#1183).
  */
class NavigationTransitionsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithBuffer(
    content: String,
    cursor: CursorPosition = CursorPosition(0, 0),
    bookmarks: List[CursorPosition] = Nil,
    comments: List[DocumentComment] = Nil
  ): AppState =
    val buffer = Buffer
      .fromString(BufferId(0), content)
      .copy(
        editing = EditingState(List(cursor)),
        annotations = Annotations(bookmarks = bookmarks, documentComments = comments)
      )
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(BufferId(0) -> buffer)))

  private def withoutActiveEditor(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(layout = state.persisted.layout.copy(activeEditorPaneId = None)))

  private def withUiTransitions(state: AppState, enabled: Boolean): AppState =
    val config =
      if enabled then
        state.persisted.config.withMotionPreset(MotionPreset.Smooth).withUiAnimation(AnimationConfig.subtle)
      else state.persisted.config.withUiAnimation(None)
    state.copy(persisted = state.persisted.copy(config = config))

  private def withHistory(
    state: AppState,
    backStack: List[NavigationPoint] = Nil,
    forwardStack: List[NavigationPoint] = Nil
  ): AppState =
    state.copy(runtime = state.runtime.copy(navigation = NavigationHistory(backStack, forwardStack)))

  private def validApplied(outcome: NavigationOutcome): ReducerResult =
    outcome match
      case NavigationOutcome.Applied(result) =>
        AppStateValidation.validationErrors(result.state) shouldBe Nil
        result
      case NavigationOutcome.Ignored(reason) => fail(s"Expected an applied transition, got Ignored($reason)")

  private def buffer0(state: AppState): Buffer =
    state.persisted.buffers.getOrElse(BufferId(0), fail("expected buffer 0"))

  private def commentLensTargets(state: AppState): List[Option[DocumentComment]] =
    state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.CommentLens(lens) => lens.target }

  private val lines    = "line0\nline1\nline2"
  private val sections = "Section One\n\nSection Two\n\nSection Three"

  // --- Goto line -----------------------------------------------------------------------------------------------

  "NavigationTransitions" should "open the goto-line modal as a valid state" in {
    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.OpenGotoLine, AppState.initial))

    result.state.runtime.uiSurfaces.map(_.content) shouldBe List(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    result.effects shouldBe Nil
  }

  // --- Bookmarks -----------------------------------------------------------------------------------------------

  it should "add a bookmark at the cursor, keeping bookmarks sorted" in {
    val state = stateWithBuffer(lines, cursor = CursorPosition(1, 0), bookmarks = List(CursorPosition(2, 0)))

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.ToggleBookmark, state))

    buffer0(result.state).annotations.bookmarks shouldBe List(CursorPosition(1, 0), CursorPosition(2, 0))
    result.effects shouldBe Nil
  }

  it should "remove the bookmark at the cursor" in {
    val cursor = CursorPosition(1, 0)
    val state  = stateWithBuffer(lines, cursor = cursor, bookmarks = List(cursor))

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.ToggleBookmark, state))

    buffer0(result.state).annotations.bookmarks shouldBe Nil
  }

  it should "ignore a bookmark toggle without an active editor buffer" in {
    val state = withoutActiveEditor(stateWithBuffer(lines))

    NavigationTransitions.navigation(NavigationIntent.ToggleBookmark, state) shouldBe
      NavigationOutcome.Ignored(Some("[CMD] Toggle bookmark requested without an active editor buffer"))
  }

  it should "jump to the next bookmark, recording the origin in the back stack and clearing the forward stack" in {
    val first  = CursorPosition(0, 0)
    val second = CursorPosition(2, 0)
    val state = withHistory(
      stateWithBuffer(lines, cursor = first, bookmarks = List(first, second)),
      forwardStack = List(NavigationPoint(PaneId(0), BufferId(0), second))
    )

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.NextBookmark, state))

    result.state.activeCursorPosition shouldBe Some(second)
    result.state.runtime.navigation shouldBe NavigationHistory(
      backStack = List(NavigationPoint(PaneId(0), BufferId(0), first)),
      forwardStack = Nil
    )
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "jump to the previous bookmark" in {
    val first  = CursorPosition(0, 0)
    val second = CursorPosition(2, 0)
    val state  = stateWithBuffer(lines, cursor = second, bookmarks = List(first, second))

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.PreviousBookmark, state))

    result.state.activeCursorPosition shouldBe Some(first)
  }

  it should "restart the target buffer's UI transitions when UI transitions are enabled" in {
    val first  = CursorPosition(0, 0)
    val second = CursorPosition(2, 0)
    val state  = withUiTransitions(stateWithBuffer(lines, cursor = first, bookmarks = List(first, second)), true)
    state.persisted.config.scaledUiAnimation shouldBe defined

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.NextBookmark, state))

    result.effects match
      case List(AppEffect.Animation(AnimationEffect.RestartUiTransitions(bufferId, cells))) =>
        bufferId shouldBe BufferId(0)
        cells should not be empty
        cells.values.map(_.owner).toSet shouldBe Set(AnimationOwner.UiTransitions)
      case other => fail(s"Expected a single UI-transition restart, got $other")
  }

  it should "emit no animation when UI transitions are disabled" in {
    val first  = CursorPosition(0, 0)
    val second = CursorPosition(2, 0)
    val state  = withUiTransitions(stateWithBuffer(lines, cursor = first, bookmarks = List(first, second)), false)
    state.persisted.config.scaledUiAnimation shouldBe None

    validApplied(NavigationTransitions.navigation(NavigationIntent.NextBookmark, state)).effects shouldBe Nil
  }

  it should "ignore bookmark navigation when there are no bookmarks" in {
    NavigationTransitions.navigation(NavigationIntent.NextBookmark, stateWithBuffer(lines)) shouldBe
      NavigationOutcome.Ignored(Some("[CMD] Bookmark navigation requested without a target"))
  }

  it should "ignore bookmark navigation to the bookmark the cursor is already on" in {
    val only  = CursorPosition(1, 0)
    val state = stateWithBuffer(lines, cursor = only, bookmarks = List(only))

    NavigationTransitions.navigation(NavigationIntent.NextBookmark, state) shouldBe
      NavigationOutcome.Ignored(Some("[CMD] Bookmark navigation requested for the current location"))
  }

  // --- Document symbols ------------------------------------------------------------------------------------------

  it should "jump to the next document symbol" in {
    val state = stateWithBuffer(sections, cursor = CursorPosition(0, 0))

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.NextDocumentSymbol, state))

    result.state.activeCursorPosition shouldBe Some(CursorPosition(2, 0))
  }

  it should "jump to the previous document symbol" in {
    val state = stateWithBuffer(sections, cursor = CursorPosition(2, 0))

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.PreviousDocumentSymbol, state))

    result.state.activeCursorPosition shouldBe Some(CursorPosition(0, 0))
  }

  it should "ignore document-symbol navigation in a buffer with no outline" in {
    val state = stateWithBuffer("just one paragraph, no blank-line sections")

    NavigationTransitions.navigation(NavigationIntent.NextDocumentSymbol, state) shouldBe
      NavigationOutcome.Ignored(Some("[CMD] Document symbol navigation requested without a target"))
  }

  // --- Back/forward history --------------------------------------------------------------------------------------

  it should "navigate back, pushing the current point onto the forward stack" in {
    val current = CursorPosition(2, 0)
    val target  = CursorPosition(0, 0)
    val state = withHistory(
      stateWithBuffer(lines, cursor = current),
      backStack = List(NavigationPoint(PaneId(0), BufferId(0), target))
    )

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.NavigateBack, state))

    result.state.activeCursorPosition shouldBe Some(target)
    result.state.runtime.navigation shouldBe NavigationHistory(
      backStack = Nil,
      forwardStack = List(NavigationPoint(PaneId(0), BufferId(0), current))
    )
  }

  it should "navigate forward, pushing the current point onto the back stack without duplicating its head" in {
    val current = CursorPosition(0, 0)
    val target  = CursorPosition(2, 0)
    val here    = NavigationPoint(PaneId(0), BufferId(0), current)
    val state = withHistory(
      stateWithBuffer(lines, cursor = current),
      backStack = List(here),
      forwardStack = List(NavigationPoint(PaneId(0), BufferId(0), target))
    )

    val result = validApplied(NavigationTransitions.navigation(NavigationIntent.NavigateForward, state))

    result.state.activeCursorPosition shouldBe Some(target)
    result.state.runtime.navigation shouldBe NavigationHistory(backStack = List(here), forwardStack = Nil)
  }

  it should "silently ignore back and forward navigation with empty stacks" in {
    val state = stateWithBuffer(lines)

    NavigationTransitions.navigation(NavigationIntent.NavigateBack, state) shouldBe NavigationOutcome.Ignored(None)
    NavigationTransitions.navigation(NavigationIntent.NavigateForward, state) shouldBe NavigationOutcome.Ignored(None)
  }

  it should "silently ignore history navigation without an active editor buffer" in {
    val state = withHistory(
      withoutActiveEditor(stateWithBuffer(lines)),
      backStack = List(NavigationPoint(PaneId(0), BufferId(0), CursorPosition(1, 0)))
    )

    NavigationTransitions.navigation(NavigationIntent.NavigateBack, state) shouldBe NavigationOutcome.Ignored(None)
  }

  // --- Comment lens ----------------------------------------------------------------------------------------------

  private val comment = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "Needs work")

  it should "open the comment lens on the active comment as a valid state" in {
    val state = stateWithBuffer("hello world", cursor = CursorPosition(0, 2), comments = List(comment))

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.ToggleCommentLens, state))

    commentLensTargets(result.state) shouldBe List(Some(comment))
  }

  it should "dismiss an open comment lens and restore the prior focus" in {
    val state  = stateWithBuffer("hello world", cursor = CursorPosition(0, 2), comments = List(comment))
    val opened = validApplied(NavigationTransitions.comments(CommentsIntent.ToggleCommentLens, state)).state

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.ToggleCommentLens, opened))

    result.state.runtime.uiSurfaces shouldBe Nil
    result.state.persisted.focus shouldBe state.persisted.focus
  }

  it should "ignore a comment-lens toggle with no active comment" in {
    val state = stateWithBuffer("hello world", cursor = CursorPosition(0, 2))

    NavigationTransitions.comments(CommentsIntent.ToggleCommentLens, state) shouldBe
      NavigationOutcome.Ignored(Some("[CMD] Comment lens requested without an active comment"))
  }

  // --- Document comments -----------------------------------------------------------------------------------------

  it should "add a comment at the cursor and mark the buffer dirty" in {
    val state = stateWithBuffer("hello world")

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.AddDocumentComment("A note"), state))

    buffer0(result.state).annotations.documentComments shouldBe
      List(DocumentComment(CursorPosition(0, 0), CursorPosition(0, 0), "A note"))
    buffer0(result.state).document.isDirty shouldBe true
  }

  it should "comment the selected range" in {
    val state = stateWithBuffer("hello world")
    val selected = state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          BufferId(0),
          buffer0(state).copy(editing =
            com.serenity.testkit.EditingStateFixtures(
              cursors = List(CursorPosition(0, 5)),
              selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5)))
            )
          )
        )
      )
    )

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.AddDocumentComment("Hi"), selected))

    buffer0(result.state).annotations.documentComments shouldBe
      List(DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "Hi"))
  }

  it should "replace the text of the comment at the cursor instead of adding another" in {
    val state = stateWithBuffer("hello world", cursor = CursorPosition(0, 2), comments = List(comment))

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.AddDocumentComment("New"), state))

    buffer0(result.state).annotations.documentComments shouldBe List(comment.copy(text = "New"))
  }

  it should "default blank comment text to the placeholder" in {
    val result =
      validApplied(NavigationTransitions.comments(CommentsIntent.AddDocumentComment("  "), stateWithBuffer("hi")))

    buffer0(result.state).annotations.documentComments.map(_.text) shouldBe List("Comment")
  }

  it should "delete the comment at the cursor and mark the buffer dirty" in {
    val state = stateWithBuffer("hello world", cursor = CursorPosition(0, 2), comments = List(comment))

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.DeleteDocumentComment, state))

    buffer0(result.state).annotations.documentComments shouldBe Nil
    buffer0(result.state).document.isDirty shouldBe true
  }

  it should "leave the buffer clean when there is no comment at the cursor to delete" in {
    val state = stateWithBuffer("hello world", cursor = CursorPosition(0, 8), comments = List(comment))

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.DeleteDocumentComment, state))

    buffer0(result.state).annotations.documentComments shouldBe List(comment)
    buffer0(result.state).document.isDirty shouldBe false
  }

  it should "ignore comment edits without an active editor buffer" in {
    val state = withoutActiveEditor(stateWithBuffer("hello world"))

    NavigationTransitions.comments(CommentsIntent.AddDocumentComment("x"), state) shouldBe
      NavigationOutcome.Ignored(Some("[CMD] Add document comment requested without an active editor buffer"))
    NavigationTransitions.comments(CommentsIntent.DeleteDocumentComment, state) shouldBe
      NavigationOutcome.Ignored(Some("[CMD] Delete document comment requested without an active editor buffer"))
  }

  // --- Comment navigation ------------------------------------------------------------------------------------------

  it should "jump to the next comment and open the lens on it" in {
    val first  = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "First")
    val second = DocumentComment(CursorPosition(2, 0), CursorPosition(2, 5), "Second")
    val state  = stateWithBuffer("aaaaa\nbbbbb\nccccc", comments = List(first, second))

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.NextDocumentComment, state))

    result.state.activeCursorPosition shouldBe Some(CursorPosition(2, 0))
    commentLensTargets(result.state) shouldBe List(Some(second))
  }

  it should "open the lens without moving when the only comment is already under the cursor (#1183)" in {
    val only  = DocumentComment(CursorPosition(1, 0), CursorPosition(1, 5), "Only")
    val state = stateWithBuffer("aaaaa\nbbbbb\nccccc", cursor = CursorPosition(1, 0), comments = List(only))

    val result = validApplied(NavigationTransitions.comments(CommentsIntent.NextDocumentComment, state))

    result.state.activeCursorPosition shouldBe Some(CursorPosition(1, 0))
    result.state.runtime.navigation shouldBe state.runtime.navigation
    commentLensTargets(result.state) shouldBe List(Some(only))
    result.effects shouldBe Nil
  }
