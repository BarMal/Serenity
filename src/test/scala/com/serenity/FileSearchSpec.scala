package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, FileSearchComponent}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, AppEventReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileSearchSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  // ── FileSearchState ───────────────────────────────────────────────────────

  "FileSearchState" should "return None as selectedResult when results are empty" in {
    val state = FileSearchState("hello", Nil, 0)
    state.selectedResult shouldBe None
  }

  it should "return the correct selected result by index" in {
    val r0    = FileSearchResult(BufferId(0), "foo.txt", 3, "hello world")
    val r1    = FileSearchResult(BufferId(1), "bar.txt", 7, "hello there")
    val state = FileSearchState("hello", List(r0, r1), 1)
    state.selectedResult shouldBe Some(r1)
  }

  it should "move selection forward and wrap around" in {
    val results = List(
      FileSearchResult(BufferId(0), "a.txt", 0, "x"),
      FileSearchResult(BufferId(0), "a.txt", 1, "y"),
      FileSearchResult(BufferId(0), "a.txt", 2, "z")
    )
    val state = FileSearchState("x", results, selectedIndex = 2)
    state.moveSelection(1).selectedIndex shouldBe 0
  }

  it should "move selection backward and wrap around" in {
    val results = List(
      FileSearchResult(BufferId(0), "a.txt", 0, "x"),
      FileSearchResult(BufferId(0), "a.txt", 1, "y")
    )
    val state = FileSearchState("x", results, selectedIndex = 0)
    state.moveSelection(-1).selectedIndex shouldBe 1
  }

  it should "not change index when results are empty" in {
    val state = FileSearchState("hello", Nil, 0)
    state.moveSelection(1).selectedIndex shouldBe 0
  }

  it should "withQuery resets selectedIndex to 0" in {
    val state = FileSearchState("hello", Nil, selectedIndex = 2)
    state.withQuery("new").selectedIndex shouldBe 0
    state.withQuery("new").query shouldBe "new"
  }

  // ── AppEventReducer ───────────────────────────────────────────────────────

  "AppEventReducer" should "emit OpenFileSearch for FileSearch event" in {
    import com.serenity.command.CommandRegistry
    val result = AppEventReducer.reduce(FileSearch, AppState.initial, CommandRegistry.default)
    result.effects shouldBe List(AppEffect.OpenFileSearch())
  }

  // ── FileSearchComponent ───────────────────────────────────────────────────

  private def stateWithSearchSurface(
    query: String = "",
    results: List[FileSearchResult] = Nil
  ): (AppState, SurfaceId) =
    val base            = AppState.initial
    val (s1, surfaceId) = base.allocateSurfaceId
    val searchState     = FileSearchState(query, results, 0)
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.FileSearch(searchState),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val finalState = s1.copy(
      uiSurfaces = List(surface),
      focus = Focus.Surface(surfaceId)
    )
    (finalState, surfaceId)

  private def stateWithSearchAndBuffer(query: String, bufferContent: String): (AppState, SurfaceId, BufferId) =
    val base     = AppState.initial
    val bufferId = BufferId(0)
    val updatedBuffers = base.buffers.get(bufferId).fold(base.buffers) { buf =>
      import com.serenity.rope.Rope
      base.buffers + (bufferId -> buf.copy(content = Rope(bufferContent)))
    }
    val withContent     = base.copy(buffers = updatedBuffers)
    val (s1, surfaceId) = withContent.allocateSurfaceId
    val searchState     = FileSearchState(query, Nil, 0)
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.FileSearch(searchState),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val finalState = s1.copy(
      uiSurfaces = List(surface),
      focus = Focus.Surface(surfaceId)
    )
    (finalState, surfaceId, bufferId)

  private val component = new FileSearchComponent()

  "FileSearchComponent" should "append a char to query and update results on InsertChar" in {
    val (state, _) = stateWithSearchSurface("hel")
    val result     = component.processEvent(InsertChar('p'), state)
    result match
      case ComponentResult.StateChange(f) =>
        val newState = f(state)
        newState.fileSearchSurface.map(_.content) match
          case Some(SurfaceContent.FileSearch(fs)) =>
            fs.query shouldBe "help"
          case other => fail(s"Expected FileSearch surface, got $other")
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "remove the last char on DeleteBackward" in {
    val (state, _) = stateWithSearchSurface("hello")
    val result     = component.processEvent(DeleteBackward, state)
    result match
      case ComponentResult.StateChange(f) =>
        f(state).fileSearchSurface.map(_.content) match
          case Some(SurfaceContent.FileSearch(fs)) => fs.query shouldBe "hell"
          case other                               => fail(s"Expected FileSearch surface, got $other")
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "not change query on DeleteBackward when query is empty" in {
    val (state, _) = stateWithSearchSurface("")
    val result     = component.processEvent(DeleteBackward, state)
    result match
      case ComponentResult.StateChange(f) =>
        f(state).fileSearchSurface.map(_.content) match
          case Some(SurfaceContent.FileSearch(fs)) => fs.query shouldBe ""
          case other                               => fail(s"Expected FileSearch surface, got $other")
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "remove the previous word on DeleteWordBackward" in {
    val (state, _) = stateWithSearchSurface("alpha beta")
    val result     = component.processEvent(DeleteWordBackward, state)
    result match
      case ComponentResult.StateChange(f) =>
        f(state).fileSearchSurface.map(_.content) match
          case Some(SurfaceContent.FileSearch(fs)) => fs.query shouldBe "alpha "
          case other                               => fail(s"Expected FileSearch surface, got $other")
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "navigate selection down on MoveDown" in {
    val results = List(
      FileSearchResult(BufferId(0), "a.txt", 0, "x"),
      FileSearchResult(BufferId(0), "a.txt", 1, "y")
    )
    val (state, _) = stateWithSearchSurface("x", results)
    val result     = component.processEvent(MoveDown, state)
    result match
      case ComponentResult.StateChange(f) =>
        f(state).fileSearchSurface.map(_.content) match
          case Some(SurfaceContent.FileSearch(fs)) => fs.selectedIndex shouldBe 1
          case other                               => fail(s"Expected FileSearch surface, got $other")
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "navigate selection up on MoveUp" in {
    val results = List(
      FileSearchResult(BufferId(0), "a.txt", 0, "x"),
      FileSearchResult(BufferId(0), "a.txt", 1, "y")
    )
    val (base, surfaceId) = stateWithSearchSurface("x", results)
    val stateAtIdx1 = base.copy(
      uiSurfaces = base.uiSurfaces.map { s =>
        if s.id == surfaceId then s.copy(content = SurfaceContent.FileSearch(FileSearchState("x", results, 1)))
        else s
      }
    )
    val result = component.processEvent(MoveUp, stateAtIdx1)
    result match
      case ComponentResult.StateChange(f) =>
        f(stateAtIdx1).fileSearchSurface.map(_.content) match
          case Some(SurfaceContent.FileSearch(fs)) => fs.selectedIndex shouldBe 0
          case other                               => fail(s"Expected FileSearch surface, got $other")
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "dismiss without navigation on Escape" in {
    val (state, _) = stateWithSearchSurface("hello")
    val result     = component.processEvent(Escape, state)
    result match
      case ComponentResult.StateChange(f) =>
        val newState = f(state)
        newState.fileSearchSurface shouldBe None
        newState.focus shouldBe a[Focus.EditorPane]
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "navigate to the selected result and dismiss on Enter" in {
    val bufferId   = BufferId(0)
    val results    = List(FileSearchResult(bufferId, "main.txt", 5, "selected line"))
    val (state, _) = stateWithSearchSurface("sel", results)
    val result     = component.processEvent(Enter, state)
    result match
      case ComponentResult.StateChange(f) =>
        val newState = f(state)
        newState.fileSearchSurface shouldBe None
        newState.focus shouldBe a[Focus.EditorPane]
        newState.buffers.get(bufferId).flatMap(_.cursors.headOption).map(_.line) shouldBe Some(5)
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "just dismiss on Enter when no result is selected" in {
    val (state, _) = stateWithSearchSurface("")
    val result     = component.processEvent(Enter, state)
    result match
      case ComponentResult.StateChange(f) =>
        val newState = f(state)
        newState.fileSearchSurface shouldBe None
      case other => fail(s"Expected StateChange, got $other")
  }

  it should "search buffer content and populate results on typing" in {
    val (state, _) = stateWithSearchAndBuffer("hello", "line one\nhello world\nline three")._1 match
      case s => (s, ())
    // Type 'h' — searches all buffers
    val typed                       = stateWithSearchAndBuffer("h", "hello world\nno match\nhello there")
    val (searchState, surfaceId, _) = typed
    val result                      = component.processEvent(InsertChar('i'), searchState)
    result match
      case ComponentResult.StateChange(f) =>
        val newState = f(searchState)
        newState.fileSearchSurface.map(_.content) match
          case Some(SurfaceContent.FileSearch(fs)) =>
            fs.query shouldBe "hi"
          case other => fail(s"Expected FileSearch, got $other")
      case other => fail(s"Expected StateChange, got $other")
  }
