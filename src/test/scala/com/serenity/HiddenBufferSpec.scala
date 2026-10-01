package com.serenity

import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.Balance
import com.serenity.session.given
import com.serenity.session.{SessionBuffer, SessionState}
import com.serenity.state.components.{ComponentResult, FileSearchComponent}
import com.serenity.state.models.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A hidden buffer holds text the user authors (a chapter note) without being a document of its own: it lives in
  * `Persisted.buffers` so an editor pane can show and edit it, but stays out of every list of open documents.
  */
class HiddenBufferSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val visibleId = BufferId(0)
  private val hiddenId  = BufferId(5)

  private def hiddenNote(text: String): Buffer = Buffer.fromString(hiddenId, text).copy(hidden = true)

  private def stateWithHiddenNote(text: String): AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = initial.persisted.buffers.updated(hiddenId, hiddenNote(text)),
        bufferOrder = List(visibleId)
      )
    )

  "A hidden buffer" should "not count as having unsaved changes however much text it holds" in {
    hiddenNote("an outline").hasUnsavedChanges shouldBe false
    Buffer.fromString(hiddenId, "an outline").hasUnsavedChanges shouldBe true
  }

  it should "keep its text in the session even when unsaved text is not otherwise persisted" in {
    SessionBuffer.fromBuffer(hiddenNote("an outline"), persistUnsaved = false).unsavedContent shouldBe Some(
      "an outline"
    )
  }

  it should "round-trip through the session with its flag and text" in {
    val restored = SessionBuffer.toBuffer(SessionBuffer.fromBuffer(hiddenNote("an outline")))

    restored.hidden shouldBe true
    restored.document.content.collect() shouldBe "an outline"
  }

  it should "stay out of the tab order when a session is restored" in {
    val state    = stateWithHiddenNote("an outline")
    val restored = SessionState.toAppState(SessionState.fromAppState(state), Theme.default)

    restored.persisted.bufferOrder shouldBe List(visibleId)
    restored.persisted.buffers.get(hiddenId).map(_.hidden) shouldBe Some(true)
  }

  it should "not appear in the tab list" in {
    TabListContent.build(stateWithHiddenNote("an outline")).entries.map(_.bufferId) shouldBe List(visibleId)
  }

  it should "not be searched by project file search" in {
    val withNeedle = stateWithHiddenNote("needle in the note")
    val withVisible = withNeedle.copy(persisted =
      withNeedle.persisted.copy(buffers =
        withNeedle.persisted.buffers.updated(visibleId, Buffer.fromString(visibleId, "needle in the manuscript"))
      )
    )
    val (s1, surfaceId) = withVisible.allocateSurfaceId
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.FileSearch(FileSearchState("needl", Nil, 0)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val searching = s1.copy(
      runtime = s1.runtime.copy(uiSurfaces = List(surface)),
      persisted = s1.persisted.copy(focus = Focus.Surface(surfaceId))
    )

    new FileSearchComponent().processEvent(InsertChar('e'), searching) match
      case ComponentResult.StateChange(update) =>
        update(searching).fileSearchSurface.map(_.content) match
          case Some(SurfaceContent.FileSearch(search)) =>
            search.results.map(_.bufferId).distinct shouldBe List(visibleId)
          case other => fail(s"Expected FileSearch, got $other")
      case other => fail(s"Expected StateChange, got $other")
  }

  "A session buffer written before hidden buffers existed" should "decode as an ordinary buffer" in {
    val decoded = _root_.io.circe.parser
      .parse(
        """{"id":1,"filePath":null,"isDirty":false,"language":null,"isNewEmpty":false,
          |"cursors":[{"line":0,"column":0}],
          |"viewport":{"leftColumn":0,"topLine":0,"visibleColumns":80,"visibleLines":24}}""".stripMargin
      )
      .flatMap(_.as[SessionBuffer])

    decoded.map(_.hidden) shouldBe Right(false)
  }
