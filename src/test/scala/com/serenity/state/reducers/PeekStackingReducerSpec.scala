package com.serenity.state.reducers

import java.nio.file.Paths

import com.serenity.AboveCursorStackFixtures.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{PanelPosition, PeekContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A peek is recognised by what it shows, not by where it floats: other surfaces anchored above the cursor -- the
  * comment lens, the command runner's cursor peek -- are not peeks, and showing or dismissing one leaves them alone.
  */
class PeekStackingReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val line = 20

  private def valid(state: AppState): Boolean = AppStateValidation.validated(state).isRight

  "PeekStateReducer.show" should "keep an open comment lens while focusing the new peek" in {
    val lensOpen = withLens(line)
    val shown    = lensAndPeek(line)

    shown.commentLensSurface shouldBe lensOpen.commentLensSurface
    shown.peekSurface.map(_.content) shouldBe Some(SurfaceContent.QuickInfo(PeekText))
    shown.persisted.focus shouldBe Focus.Surface(peekId(shown))
    valid(shown) shouldBe true
  }

  it should "replace an existing peek, and only the peek" in {
    val first  = lensAndPeek(line)
    val second = PeekStateReducer.show(PeekContent.QuickInfo("second"), cursorAt(line), first).state

    second.runtime.uiSurfaces.map(_.content).collect { case quickInfo: SurfaceContent.QuickInfo => quickInfo } shouldBe
      List(SurfaceContent.QuickInfo("second"))
    second.commentLensSurface shouldBe first.commentLensSurface
  }

  it should "leave the command runner's above-cursor peek in place" in {
    val withRunnerPeek = withRunnerCursorPeek(editorWithComment(line), line)
    val shown          = withPeek(withRunnerPeek, line)

    shown.surfaceById(SurfaceId.CursorPeek) shouldBe withRunnerPeek.surfaceById(SurfaceId.CursorPeek)
  }

  it should "leave a peek that was pinned as a docked panel in place" in {
    val listing = PeekStateReducer
      .show(PeekContent.DirectoryListing(Paths.get("/repo"), Nil), cursorAt(line), editorWithComment(line))
      .state
    val pinned   = PanelStateReducer.pinPeekOverlay(PanelPosition.Right, listing).state
    val pinnedId = pinned.pinnedSurfaces.map(_.id)

    val shown = withPeek(pinned, line)

    shown.pinnedSurfaces.map(_.id) shouldBe pinnedId
    shown.peekSurface.map(_.content) shouldBe Some(SurfaceContent.QuickInfo(PeekText))
  }

  "PeekStateReducer.dismiss" should "remove only the peek and hand focus back to the lens that held it" in {
    val shown     = lensAndPeek(line)
    val dismissed = PeekStateReducer.dismiss(shown).state

    dismissed.peekSurface shouldBe None
    dismissed.commentLensSurface shouldBe shown.commentLensSurface
    dismissed.persisted.focus shouldBe Focus.Surface(lensId(shown))
    valid(dismissed) shouldBe true
  }

  it should "leave the command runner's above-cursor peek in place" in {
    val shown     = withPeek(withRunnerCursorPeek(editorWithComment(line), line), line)
    val dismissed = PeekStateReducer.dismiss(shown).state

    dismissed.peekSurface shouldBe None
    dismissed.surfaceById(SurfaceId.CursorPeek) shouldBe shown.surfaceById(SurfaceId.CursorPeek)
  }

  it should "leave the comment lens alone when no peek is open" in {
    val lensOpen = withLens(line)

    PeekStateReducer.dismiss(lensOpen).state shouldBe lensOpen
  }

  "AppState.peekSurface" should "find the peek rather than a comment lens listed before it" in {
    val shown = lensAndPeek(line)

    shown.runtime.uiSurfaces.headOption.map(_.content) should matchPattern {
      case Some(SurfaceContent.CommentLens(_)) =>
    }
    shown.peekSurface.map(_.content) shouldBe Some(SurfaceContent.QuickInfo(PeekText))
  }
