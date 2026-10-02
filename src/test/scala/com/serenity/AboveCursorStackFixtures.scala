package com.serenity

import com.serenity.command.CommandRunner
import com.serenity.document.CommentRendering
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.PeekStateReducer
import com.serenity.ui.layout.{PeekContent, ViewportSize}

/** A comment lens and an LSP peek open over the same cursor: the two above-cursor surfaces that used to evict and
  * overlap one another.
  */
object AboveCursorStackFixtures:

  private given Balance = Balance.default

  val StackViewport: ViewportSize = ViewportSize(100, 30)
  val PeekText: String            = "map[B](f: A => B): List[B]"

  def cursorAt(line: Int): CursorPosition = CursorPosition(line, 3)

  def editorWithComment(line: Int, viewport: ViewportSize = StackViewport): AppState =
    val base   = AppState.initial
    val text   = List.fill(40)("abcdefghijklmnopqrstuvwxyz").mkString("\n")
    val buffer = Buffer.fromString(BufferId(0), text)
    val comment =
      DocumentComment(CursorPosition(line, 0), CursorPosition(line, 10), "Explain why this line exists")
    base.copy(
      persisted = base.persisted.copy(buffers =
        Map(
          BufferId(0) -> buffer.copy(
            annotations = buffer.annotations.copy(documentComments = List(comment)),
            editing = EditingState(List(cursorAt(line)))
          )
        )
      ),
      runtime = base.runtime.copy(viewportSize = Some(viewport))
    )

  def withLens(line: Int, mode: CommentLensMode = CommentLensMode.Editable): AppState =
    CommentRendering.openLensAtCursor(editorWithComment(line), mode)

  def withPeek(state: AppState, line: Int): AppState =
    PeekStateReducer.show(PeekContent.QuickInfo(PeekText), cursorAt(line), state).state

  def lensAndPeek(line: Int, mode: CommentLensMode = CommentLensMode.Editable): AppState =
    withPeek(withLens(line, mode), line)

  def lensId(state: AppState): SurfaceId =
    state.commentLensSurface.map(_.id).getOrElse(throw new NoSuchElementException("Expected a comment lens"))

  def peekId(state: AppState): SurfaceId =
    state.peekSurface.map(_.id).getOrElse(throw new NoSuchElementException("Expected a peek"))

  def withRunnerCursorPeek(state: AppState, line: Int): AppState =
    val runnerPeek = UiSurface(
      SurfaceId.CursorPeek,
      SurfaceContent.CommandRunnerPeek(CommandRunner.empty),
      SurfacePresentation.Floating(Some(cursorAt(line)), SurfacePlacement.AboveCursor)
    )
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces :+ runnerPeek))
