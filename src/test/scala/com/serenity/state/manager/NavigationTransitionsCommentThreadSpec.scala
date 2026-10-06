package com.serenity.state.manager

import java.time.Instant

import com.serenity.command.CommentsIntent
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Comment threads (#1903): who wrote a comment and when, replies, and resolving and reopening -- all of them reached
  * through a comment's id rather than its place in the list.
  */
class NavigationTransitionsCommentThreadSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val wrote   = Instant.parse("2026-10-06T10:00:00Z")
  private val replied = Instant.parse("2026-10-06T11:30:00Z")
  private val author  = "Ada"

  private val first  = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "First", id = CommentId(1))
  private val second = DocumentComment(CursorPosition(2, 0), CursorPosition(2, 5), "Second", id = CommentId(2))

  private def stateWith(
    comments: List[DocumentComment],
    cursor: CursorPosition,
    showResolved: Boolean = false,
    authorName: Option[String] = Some(author)
  ): AppState =
    val buffer = Buffer
      .fromString(BufferId(0), "aaaaa\nbbbbb\nccccc")
      .copy(
        editing = EditingState(List(cursor)),
        annotations = Annotations(documentComments = comments)
      )
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(BufferId(0) -> buffer),
        config = AppState.initial.persisted.config.withCommentAuthor(authorName)
      ),
      runtime = AppState.initial.runtime.copy(resolvedCommentsVisible = showResolved)
    )

  private def run(intent: CommentsIntent, state: AppState, at: Instant = replied): AppState =
    NavigationTransitions.comments(intent, state, at) match
      case NavigationOutcome.Applied(result) =>
        AppStateValidation.validationErrors(result.state) shouldBe Nil
        result.state
      case NavigationOutcome.Ignored(reason) => fail(s"Expected an applied transition, got Ignored($reason)")

  private def withCursor(state: AppState, cursor: CursorPosition): AppState =
    val buffer = state.persisted.buffers(BufferId(0))
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(BufferId(0), buffer.copy(editing = EditingState(List(cursor))))
      )
    )

  private def comments(state: AppState): List[DocumentComment] =
    state.persisted.buffers(BufferId(0)).annotations.documentComments

  private def lensTargets(state: AppState): List[Option[CommentLensTarget]] =
    state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.CommentLens(lens) => lens.target }

  "Adding a comment" should "record who wrote it and when, under the configured author" in {
    val after = run(CommentsIntent.AddDocumentComment("Hello"), stateWith(Nil, CursorPosition(0, 1)), wrote)

    comments(after).map(c => (c.id, c.author, c.createdAt, c.editedAt)) shouldBe
      List((CommentId(1), Some(author), Some(wrote), None))
  }

  it should "write as the operating system's user when the config names no author" in {
    val after = run(
      CommentsIntent.AddDocumentComment("Hello"),
      stateWith(Nil, CursorPosition(0, 1), authorName = None),
      wrote
    )

    comments(after).map(_.author) shouldBe List(Some(System.getProperty("user.name")))
  }

  it should "give each added comment its own id, however the list is ordered" in {
    val later   = run(CommentsIntent.AddDocumentComment("Later"), stateWith(Nil, CursorPosition(2, 0)), wrote)
    val earlier = run(CommentsIntent.AddDocumentComment("Earlier"), withCursor(later, CursorPosition(0, 0)), wrote)

    comments(earlier).map(c => c.text -> c.id) shouldBe List("Earlier" -> CommentId(2), "Later" -> CommentId(1))
  }

  it should "keep the id, author, replies and resolved state when it rewrites the comment at the cursor" in {
    val thread = first.copy(
      author = Some("Grace"),
      createdAt = Some(wrote),
      replies = List(CommentReply("Ada", wrote, "Yes"))
    )

    val after = run(CommentsIntent.AddDocumentComment("Rewritten"), stateWith(List(thread), CursorPosition(0, 2)))

    comments(after) shouldBe List(thread.copy(text = "Rewritten", editedAt = Some(replied)))
  }

  "Replying" should "append the reply to the comment at the cursor, written by the configured author" in {
    val state = stateWith(List(first, second), CursorPosition(2, 1))

    val after = run(CommentsIntent.ReplyToDocumentComment("  Agreed  "), state)

    comments(after) shouldBe List(first, second.copy(replies = List(CommentReply(author, replied, "Agreed"))))
    after.persisted.buffers(BufferId(0)).document.isDirty shouldBe true
  }

  it should "keep replies in the order they were written" in {
    val once  = run(CommentsIntent.ReplyToDocumentComment("One"), stateWith(List(first), CursorPosition(0, 1)), wrote)
    val twice = run(CommentsIntent.ReplyToDocumentComment("Two"), once, replied)

    comments(twice).flatMap(_.replies).map(reply => reply.text -> reply.at) shouldBe
      List("One" -> wrote, "Two" -> replied)
  }

  it should "be ignored when the cursor is not on a comment" in {
    NavigationTransitions.comments(
      CommentsIntent.ReplyToDocumentComment("Hm"),
      stateWith(List(first), CursorPosition(1, 0)),
      replied
    ) shouldBe NavigationOutcome.Ignored(
      Some("[CMD] Reply to document comment requested without a document comment at the cursor")
    )
  }

  it should "be ignored when the reply has no text" in {
    NavigationTransitions.comments(
      CommentsIntent.ReplyToDocumentComment("   "),
      stateWith(List(first), CursorPosition(0, 1)),
      replied
    ) shouldBe NavigationOutcome.Ignored(Some("[CMD] Reply to document comment requested without any text"))
  }

  "Resolving" should "mark only the comment at the cursor, leaving its thread in place" in {
    val threaded = second.copy(replies = List(CommentReply("Grace", wrote, "Fixed")))
    val state    = stateWith(List(first, threaded), CursorPosition(2, 1))

    val after = run(CommentsIntent.ResolveDocumentComment, state)

    comments(after) shouldBe List(first, threaded.copy(resolved = true))
  }

  it should "be ignored for a comment that is already resolved" in {
    val state = stateWith(List(first.copy(resolved = true)), CursorPosition(0, 1), showResolved = true)

    NavigationTransitions.comments(CommentsIntent.ResolveDocumentComment, state, replied) shouldBe
      NavigationOutcome.Ignored(
        Some("[CMD] Resolve document comment requested without a document comment at the cursor")
      )
  }

  "Reopening" should "clear the resolved flag of the resolved comment at the cursor" in {
    val state = stateWith(List(first.copy(resolved = true)), CursorPosition(0, 1))

    val after = run(CommentsIntent.ReopenDocumentComment, state)

    comments(after) shouldBe List(first)
  }

  it should "reach a resolved comment even while resolved comments are hidden" in {
    val state = stateWith(List(first.copy(resolved = true)), CursorPosition(0, 1), showResolved = false)

    comments(run(CommentsIntent.ReopenDocumentComment, state)).map(_.resolved) shouldBe List(false)
  }

  "Showing resolved comments" should "toggle on and off" in {
    val state = stateWith(Nil, CursorPosition(0, 0))

    val shown  = run(CommentsIntent.ToggleResolvedComments, state)
    val hidden = run(CommentsIntent.ToggleResolvedComments, shown)

    shown.runtime.resolvedCommentsVisible shouldBe true
    hidden.runtime.resolvedCommentsVisible shouldBe false
  }

  "The comment lens" should "not open on a resolved comment by default" in {
    val state = stateWith(List(first.copy(resolved = true)), CursorPosition(0, 1))

    NavigationTransitions.comments(CommentsIntent.ToggleCommentLens, state, replied) shouldBe
      NavigationOutcome.Ignored(Some("[CMD] Comment lens requested without an active comment"))
  }

  it should "open on a resolved comment once resolved comments are shown" in {
    val resolved = first.copy(resolved = true)
    val state    = stateWith(List(resolved), CursorPosition(0, 1), showResolved = true)

    lensTargets(run(CommentsIntent.ToggleCommentLens, state)) shouldBe
      List(Some(CommentLensTarget(resolved.id, resolved)))
  }

  it should "close when its comment is resolved" in {
    val opened = run(CommentsIntent.ToggleCommentLens, stateWith(List(first), CursorPosition(0, 1)))

    val after = run(CommentsIntent.ResolveDocumentComment, opened)

    after.commentLensSurface shouldBe None
  }

  it should "close when resolved comments are hidden again while it shows one" in {
    val resolved = first.copy(resolved = true)
    val opened =
      run(CommentsIntent.ToggleCommentLens, stateWith(List(resolved), CursorPosition(0, 1), showResolved = true))

    val after = run(CommentsIntent.ToggleResolvedComments, opened)

    after.commentLensSurface shouldBe None
  }

  "Comment navigation" should "skip resolved comments by default" in {
    val state = stateWith(List(first, second.copy(resolved = true)), CursorPosition(0, 0))

    val after = run(CommentsIntent.NextDocumentComment, state)

    after.activeCursorPosition shouldBe Some(CursorPosition(0, 0))
    lensTargets(after) shouldBe List(Some(CommentLensTarget(first.id, first)))
  }

  it should "include resolved comments once they are shown" in {
    val resolved = second.copy(resolved = true)
    val state    = stateWith(List(first, resolved), CursorPosition(0, 0), showResolved = true)

    val after = run(CommentsIntent.NextDocumentComment, state)

    after.activeCursorPosition shouldBe Some(CursorPosition(2, 0))
    lensTargets(after) shouldBe List(Some(CommentLensTarget(resolved.id, resolved)))
  }

  "Deleting a comment" should "leave a hidden resolved comment at the same spot alone" in {
    val resolved = second.copy(anchor = first.anchor, focus = first.focus, resolved = true)
    val state    = stateWith(List(first, resolved), CursorPosition(0, 1))

    val after = run(CommentsIntent.DeleteDocumentComment, state)

    comments(after) shouldBe List(resolved)
  }

  "Saving a lens draft" should "change the text of the comment with that id and stamp the edit" in {
    val state = stateWith(List(first, second), CursorPosition(1, 0))

    val after = run(CommentsIntent.SaveCommentDraft(second.id, "  Rewritten "), state)

    comments(after) shouldBe List(first, second.copy(text = "Rewritten", editedAt = Some(replied)))
    after.persisted.buffers(BufferId(0)).document.isDirty shouldBe true
  }

  it should "change only the targeted one of two identical comments" in {
    val twin  = first.copy(id = CommentId(2))
    val state = stateWith(List(first, twin), CursorPosition(1, 0))

    comments(run(CommentsIntent.SaveCommentDraft(twin.id, "Changed"), state)).map(_.text) shouldBe
      List("First", "Changed")
  }

  it should "keep the id, author, replies and resolved state of the comment" in {
    val thread = first.copy(
      author = Some(author),
      replies = List(CommentReply("Grace", wrote, "Agreed")),
      resolved = true
    )

    val after =
      run(CommentsIntent.SaveCommentDraft(thread.id, "Changed"), stateWith(List(thread), CursorPosition(1, 0)))

    comments(after) shouldBe List(thread.copy(text = "Changed", editedAt = Some(replied)))
  }

  it should "reach a comment whose range an edit shifted or whose list position moved" in {
    val shifted = second.copy(anchor = CursorPosition(1, 1), focus = CursorPosition(1, 3))
    val state   = stateWith(List(shifted, first), CursorPosition(1, 0))

    comments(run(CommentsIntent.SaveCommentDraft(second.id, "Changed"), state)).map(c => c.id -> c.text) shouldBe
      List(second.id -> "Changed", first.id -> "First")
  }

  it should "delete the comment when the draft is empty" in {
    val state = stateWith(List(first, second), CursorPosition(1, 0))

    val after = run(CommentsIntent.SaveCommentDraft(first.id, "   "), state)

    comments(after) shouldBe List(second)
    after.persisted.buffers(BufferId(0)).document.isDirty shouldBe true
  }

  it should "leave the buffer clean when the text did not change" in {
    val state = stateWith(List(first), CursorPosition(1, 0))

    val after = run(CommentsIntent.SaveCommentDraft(first.id, "First"), state)

    comments(after) shouldBe List(first)
    after.persisted.buffers(BufferId(0)).document.isDirty shouldBe false
  }

  it should "be ignored for a comment the buffer no longer holds" in {
    NavigationTransitions.comments(
      CommentsIntent.SaveCommentDraft(CommentId(9), "Changed"),
      stateWith(List(first), CursorPosition(1, 0)),
      replied
    ) shouldBe NavigationOutcome.Ignored(
      Some("[CMD] Save comment draft requested for a comment that is no longer in the buffer")
    )
  }
