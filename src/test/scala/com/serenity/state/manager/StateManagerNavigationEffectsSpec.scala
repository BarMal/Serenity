package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{CommentsIntent, NavigationIntent}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Exercises [[StateManagerNavigationEffects]] on its own: bookmark/document-symbol/comment navigation, back/forward
  * navigation history, and the comment-lens/document-comment mutations, each asserted through the state it lands rather
  * than through a fully composed `StateManager`.
  */
class StateManagerNavigationEffectsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]],
      val nav: StateManagerNavigationEffects
  ):
    def currentState: AppState = stateRef.get.unsafeRunSync()
    def currentBuffer: Buffer  = currentState.persisted.buffers(BufferId(0))

  private def harness(initialState: AppState): Harness =
    val stateRef = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val bufferAnimationsRef =
      Ref.of[IO, Map[BufferId, com.serenity.animation.AnimationState]](Map.empty).unsafeRunSync()
    val validateAndUpdateState: (AppState, AppState) => IO[Unit] =
      (newState, fallbackState) => stateRef.set(AppStateValidation.validated(newState).getOrElse(fallbackState))
    new Harness(
      stateRef,
      bufferAnimationsRef,
      new StateManagerNavigationEffects(stateRef, bufferAnimationsRef, NoOpLogger.impl[IO], validateAndUpdateState)
    )

  /** A state with a single editor pane/buffer -- the buffer built from `content`, with the given cursor and annotations
    * wired in directly (bypassing the editor's own insertion path, since these tests are about navigation/annotation
    * dispatch, not text editing).
    */
  private def stateWithBuffer(
    content: String,
    cursor: CursorPosition = CursorPosition(0, 0),
    bookmarks: List[CursorPosition] = Nil,
    comments: List[DocumentComment] = Nil
  ): AppState =
    val buffer = Buffer
      .fromString(BufferId(0), content)
      .copy(
        editing = EditingState(cursors = List(cursor)),
        annotations = Annotations(bookmarks = bookmarks, documentComments = comments)
      )
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(BufferId(0) -> buffer)))

  private def withoutActiveEditor(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(layout = state.persisted.layout.copy(activeEditorPaneId = None)))

  private val sections = "Section One\n\nSection Two\n\nSection Three"

  // --- Bookmarks -----------------------------------------------------------------------------------------------

  "StateManagerNavigationEffects" should "add a bookmark at the cursor when toggled on" in {
    val state   = stateWithBuffer("line0\nline1\nline2", cursor = CursorPosition(1, 0))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.ToggleBookmark, state).unsafeRunSync()

    fixture.currentBuffer.annotations.bookmarks shouldBe List(CursorPosition(1, 0))
  }

  it should "remove an existing bookmark at the cursor when toggled off" in {
    val cursor  = CursorPosition(1, 0)
    val state   = stateWithBuffer("line0\nline1\nline2", cursor = cursor, bookmarks = List(cursor))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.ToggleBookmark, state).unsafeRunSync()

    fixture.currentBuffer.annotations.bookmarks shouldBe Nil
  }

  it should "do nothing toggling a bookmark without an active editor buffer" in {
    val state   = withoutActiveEditor(stateWithBuffer("line0"))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.ToggleBookmark, state).unsafeRunSync()

    fixture.currentState shouldBe state
  }

  it should "navigate to the next bookmark and record the jump in navigation history" in {
    val first   = CursorPosition(0, 0)
    val second  = CursorPosition(2, 0)
    val state   = stateWithBuffer("line0\nline1\nline2", cursor = first, bookmarks = List(first, second))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NextBookmark, state).unsafeRunSync()

    val after = fixture.currentState
    after.activeCursorPosition shouldBe Some(second)
    after.runtime.navigation.backStack shouldBe List(NavigationPoint(PaneId(0), BufferId(0), first))
    after.runtime.navigation.forwardStack shouldBe Nil
  }

  it should "navigate to the previous bookmark" in {
    val first   = CursorPosition(0, 0)
    val second  = CursorPosition(2, 0)
    val state   = stateWithBuffer("line0\nline1\nline2", cursor = second, bookmarks = List(first, second))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.PreviousBookmark, state).unsafeRunSync()

    fixture.currentState.activeCursorPosition shouldBe Some(first)
  }

  it should "do nothing navigating bookmarks when there are none" in {
    val state   = stateWithBuffer("line0\nline1")
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NextBookmark, state).unsafeRunSync()

    fixture.currentState shouldBe state
  }

  it should "do nothing navigating to the only bookmark when the cursor is already there" in {
    val only    = CursorPosition(1, 0)
    val state   = stateWithBuffer("line0\nline1\nline2", cursor = only, bookmarks = List(only))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NextBookmark, state).unsafeRunSync()

    fixture.currentState shouldBe state
  }

  // --- Document symbols ------------------------------------------------------------------------------------------

  it should "navigate to the next document (outline) symbol" in {
    val state   = stateWithBuffer(sections, cursor = CursorPosition(0, 0))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NextDocumentSymbol, state).unsafeRunSync()

    fixture.currentState.activeCursorPosition shouldBe Some(CursorPosition(2, 0))
  }

  it should "navigate to the previous document (outline) symbol" in {
    val state   = stateWithBuffer(sections, cursor = CursorPosition(2, 0))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.PreviousDocumentSymbol, state).unsafeRunSync()

    fixture.currentState.activeCursorPosition shouldBe Some(CursorPosition(0, 0))
  }

  it should "wrap around to the first document symbol when past the last one" in {
    val state   = stateWithBuffer(sections, cursor = CursorPosition(4, 0))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NextDocumentSymbol, state).unsafeRunSync()

    fixture.currentState.activeCursorPosition shouldBe Some(CursorPosition(0, 0))
  }

  it should "do nothing navigating document symbols in a buffer with no outline" in {
    val state   = stateWithBuffer("just one paragraph, no blank-line sections")
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NextDocumentSymbol, state).unsafeRunSync()

    fixture.currentState shouldBe state
  }

  // --- Back/forward navigation history ----------------------------------------------------------------------------

  it should "navigate back through history and push the current point onto the forward stack" in {
    val current = CursorPosition(2, 0)
    val target  = CursorPosition(0, 0)
    val state = stateWithBuffer("line0\nline1\nline2", cursor = current)
      .pipeCopyNavigation(backStack = List(NavigationPoint(PaneId(0), BufferId(0), target)))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NavigateBack, state).unsafeRunSync()

    val after = fixture.currentState
    after.activeCursorPosition shouldBe Some(target)
    after.runtime.navigation.backStack shouldBe Nil
    after.runtime.navigation.forwardStack shouldBe List(NavigationPoint(PaneId(0), BufferId(0), current))
  }

  it should "navigate forward through history and push the current point back onto the back stack" in {
    val current = CursorPosition(0, 0)
    val target  = CursorPosition(2, 0)
    val state = stateWithBuffer("line0\nline1\nline2", cursor = current)
      .pipeCopyNavigation(forwardStack = List(NavigationPoint(PaneId(0), BufferId(0), target)))
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NavigateForward, state).unsafeRunSync()

    val after = fixture.currentState
    after.activeCursorPosition shouldBe Some(target)
    after.runtime.navigation.forwardStack shouldBe Nil
    after.runtime.navigation.backStack shouldBe List(NavigationPoint(PaneId(0), BufferId(0), current))
  }

  it should "do nothing navigating back with an empty back stack" in {
    val state   = stateWithBuffer("line0")
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NavigateBack, state).unsafeRunSync()

    fixture.currentState shouldBe state
  }

  it should "do nothing navigating forward with an empty forward stack" in {
    val state   = stateWithBuffer("line0")
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.NavigateForward, state).unsafeRunSync()

    fixture.currentState shouldBe state
  }

  // --- Goto line -----------------------------------------------------------------------------------------------

  it should "open the goto-line modal" in {
    val state   = AppState.initial
    val fixture = harness(state)

    fixture.nav.interpretNavigation(NavigationIntent.OpenGotoLine, state).unsafeRunSync()

    fixture.currentState.runtime.uiSurfaces.map(_.content) match
      case List(SurfaceContent.ModalWorkflow(Modal.GotoLine(input))) => input shouldBe ""
      case other => fail(s"Expected a single GotoLine modal, got $other")
  }

  // --- Comment lens ----------------------------------------------------------------------------------------------

  it should "open the comment lens above the cursor when a document comment is active there" in {
    val cursor  = CursorPosition(0, 2)
    val comment = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "Needs work")
    val state   = stateWithBuffer("hello world", cursor = cursor, comments = List(comment))
    val fixture = harness(state)

    fixture.nav.interpretComments(CommentsIntent.ToggleCommentLens, state).unsafeRunSync()

    fixture.currentState.runtime.uiSurfaces.map(_.content) match
      case List(SurfaceContent.CommentLens(lensState)) => lensState.target shouldBe Some(comment)
      case other                                       => fail(s"Expected a single CommentLens surface, got $other")
  }

  it should "dismiss an already-open comment lens on a second toggle" in {
    val cursor  = CursorPosition(0, 2)
    val comment = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "Needs work")
    val state   = stateWithBuffer("hello world", cursor = cursor, comments = List(comment))
    val fixture = harness(state)
    fixture.nav.interpretComments(CommentsIntent.ToggleCommentLens, state).unsafeRunSync()
    val withLens = fixture.currentState

    fixture.nav.interpretComments(CommentsIntent.ToggleCommentLens, withLens).unsafeRunSync()

    fixture.currentState.runtime.uiSurfaces shouldBe Nil
  }

  it should "do nothing toggling the comment lens with no active comment" in {
    val state   = stateWithBuffer("hello world", cursor = CursorPosition(0, 2))
    val fixture = harness(state)

    fixture.nav.interpretComments(CommentsIntent.ToggleCommentLens, state).unsafeRunSync()

    fixture.currentState shouldBe state
  }

  // --- Document comments -----------------------------------------------------------------------------------------

  it should "add a new document comment at the cursor" in {
    val state   = stateWithBuffer("hello world", cursor = CursorPosition(0, 0))
    val fixture = harness(state)

    fixture.nav.interpretComments(CommentsIntent.AddDocumentComment("A note"), state).unsafeRunSync()

    fixture.currentBuffer.annotations.documentComments.map(_.text) shouldBe List("A note")
    fixture.currentBuffer.document.isDirty shouldBe true
  }

  it should "update the text of an existing document comment at the cursor rather than duplicating it" in {
    val cursor   = CursorPosition(0, 2)
    val existing = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "Old text")
    val state    = stateWithBuffer("hello world", cursor = cursor, comments = List(existing))
    val fixture  = harness(state)

    fixture.nav.interpretComments(CommentsIntent.AddDocumentComment("New text"), state).unsafeRunSync()

    fixture.currentBuffer.annotations.documentComments.map(_.text) shouldBe List("New text")
  }

  it should "default an empty document comment's text to the placeholder" in {
    val state   = stateWithBuffer("hello world", cursor = CursorPosition(0, 0))
    val fixture = harness(state)

    fixture.nav.interpretComments(CommentsIntent.AddDocumentComment("   "), state).unsafeRunSync()

    fixture.currentBuffer.annotations.documentComments.map(_.text) shouldBe List("Comment")
  }

  it should "delete the document comment at the cursor" in {
    val cursor   = CursorPosition(0, 2)
    val existing = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "Old text")
    val state    = stateWithBuffer("hello world", cursor = cursor, comments = List(existing))
    val fixture  = harness(state)

    fixture.nav.interpretComments(CommentsIntent.DeleteDocumentComment, state).unsafeRunSync()

    fixture.currentBuffer.annotations.documentComments shouldBe Nil
  }

  it should "navigate to the next document comment and open the comment lens there" in {
    val first   = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "First")
    val second  = DocumentComment(CursorPosition(2, 0), CursorPosition(2, 5), "Second")
    val state   = stateWithBuffer("aaaaa\nbbbbb\nccccc", cursor = CursorPosition(0, 0), comments = List(first, second))
    val fixture = harness(state)

    fixture.nav.interpretComments(CommentsIntent.NextDocumentComment, state).unsafeRunSync()

    val after = fixture.currentState
    after.activeCursorPosition shouldBe Some(CursorPosition(2, 0))
    after.runtime.uiSurfaces.map(_.content) match
      case List(SurfaceContent.CommentLens(lensState)) => lensState.target shouldBe Some(second)
      case other                                       => fail(s"Expected a single CommentLens surface, got $other")
  }

  it should "navigate to the previous document comment" in {
    val first   = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "First")
    val second  = DocumentComment(CursorPosition(2, 0), CursorPosition(2, 5), "Second")
    val state   = stateWithBuffer("aaaaa\nbbbbb\nccccc", cursor = CursorPosition(2, 0), comments = List(first, second))
    val fixture = harness(state)

    fixture.nav.interpretComments(CommentsIntent.PreviousDocumentComment, state).unsafeRunSync()

    fixture.currentState.activeCursorPosition shouldBe Some(CursorPosition(0, 0))
  }

  extension (state: AppState)

    private def pipeCopyNavigation(
      backStack: List[NavigationPoint] = Nil,
      forwardStack: List[NavigationPoint] = Nil
    ): AppState =
      state.copy(runtime = state.runtime.copy(navigation = NavigationHistory(backStack, forwardStack)))
