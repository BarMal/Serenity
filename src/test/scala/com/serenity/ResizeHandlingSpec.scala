package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, ViewportAxisSizing, ViewportSizing}
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ResizeHandlingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ResizeEvent" should "be created from terminal size change" in {
    val newWidth  = 120
    val newHeight = 40

    val resizeEvent = ResizeEvent(ViewportSize(newWidth, newHeight))

    resizeEvent.newSize.width shouldBe newWidth
    resizeEvent.newSize.height shouldBe newHeight
  }

  it should "trigger layout recalculation when applied to state manager" in {
    // Create state manager with initial buffer and pane
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
    val bufferId = stateManager.createBuffer("Initial content").unsafeRunSync()
    stateManager.createPane(Some(bufferId)).unsafeRunSync()

    // Get initial state and verify initial layout
    val initialState  = stateManager.getCurrentState.unsafeRunSync()
    val initialLayout = LayoutEngine.calculateLayout(initialState, ViewportSize(80, 24))

    initialLayout.editorPanelRect.width shouldBe 53
    initialLayout.editorPanelRect.height shouldBe 23

    // Apply resize event
    val newSize     = ViewportSize(120, 40)
    val resizeEvent = ResizeEvent(newSize)
    stateManager.applyEvent(resizeEvent).unsafeRunSync()

    // Get updated state and verify layout was recalculated
    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val newLayout    = LayoutEngine.calculateLayout(updatedState, newSize)

    newLayout.editorPanelRect.width shouldBe 81
    newLayout.editorPanelRect.height shouldBe 39
    val newContentRect = CursorLayout.contentRectForPane(newLayout.editorPanelRect)

    updatedState.buffers.get(bufferId) match
      case Some(buffer) =>
        buffer.viewport.visibleLines.shouldBe(newContentRect.height)
        buffer.viewport.visibleColumns.shouldBe(newContentRect.width)
      case None => fail("No buffer found in state")
  }

  it should "sync split-pane buffer viewports to each pane layout on resize" in {
    val firstPaneId  = PaneId(0)
    val secondPaneId = PaneId(1)
    val firstBuffer  = Buffer.fromString(BufferId(0), "first")
    val secondBuffer = Buffer.fromString(BufferId(1), "second")
    val initialState = com.serenity.state.models.AppState.initial.copy(
      buffers = Map(firstBuffer.id -> firstBuffer, secondBuffer.id -> secondBuffer),
      bufferOrder = List(firstBuffer.id, secondBuffer.id),
      layout = Layout(
        editorPanes = Map(
          firstPaneId  -> EditorPane.withBuffer(firstPaneId, firstBuffer.id),
          secondPaneId -> EditorPane.withBuffer(secondPaneId, secondBuffer.id)
        ),
        activeEditorPaneId = Some(firstPaneId),
        paneOrder = List(firstPaneId, secondPaneId),
        splitDirection = PaneSplitDirection.Horizontal
      ),
      focus = Focus.EditorPane(firstPaneId),
      nextBufferId = BufferId(2),
      nextPaneId = PaneId(2)
    )
    val newSize = ViewportSize(120, 40)

    val resizedState = com.serenity.state.reducers.SystemEventReducer
      .reduce(ResizeEvent(newSize), initialState)
      .state
    val calculatedLayout = LayoutEngine.calculateLayout(resizedState, newSize)
    val paneLayouts      = LayoutEngine.calculatePaneLayouts(resizedState, calculatedLayout)

    val firstContentRect  = CursorLayout.contentRectForPane(paneLayouts(firstPaneId))
    val secondContentRect = CursorLayout.contentRectForPane(paneLayouts(secondPaneId))

    resizedState.buffers(firstBuffer.id).viewport.visibleColumns shouldBe firstContentRect.width
    resizedState.buffers(firstBuffer.id).viewport.visibleLines shouldBe firstContentRect.height
    resizedState.layout.editorPanes(firstPaneId).viewport.visibleColumns shouldBe firstContentRect.width
    resizedState.layout.editorPanes(firstPaneId).viewport.visibleLines shouldBe firstContentRect.height
    resizedState.buffers(secondBuffer.id).viewport.visibleColumns shouldBe secondContentRect.width
    resizedState.buffers(secondBuffer.id).viewport.visibleLines shouldBe secondContentRect.height
    resizedState.layout.editorPanes(secondPaneId).viewport.visibleColumns shouldBe secondContentRect.width
    resizedState.layout.editorPanes(secondPaneId).viewport.visibleLines shouldBe secondContentRect.height
  }

  it should "apply configured relative and bounded viewport sizing to pane layouts" in {
    val viewportSizing = ViewportSizing(
      width = ViewportAxisSizing(percent = 0.8, maxCells = None),
      height = ViewportAxisSizing(percent = 1.0, maxCells = Some(20))
    )
    val initialState = com.serenity.state.models.AppState.initial.copy(
      config = AppConfig.default.copy(viewportSizing = viewportSizing)
    )
    val newSize = ViewportSize(120, 40)

    val resizedState = com.serenity.state.reducers.SystemEventReducer
      .reduce(ResizeEvent(newSize), initialState)
      .state
    val calculatedLayout = LayoutEngine.calculateLayout(resizedState, newSize)
    val paneId           = resizedState.layout.activeEditorPaneId.getOrElse(fail("Expected active pane"))
    val paneRect         = LayoutEngine.calculatePaneLayouts(resizedState, calculatedLayout)(paneId)
    val bufferId         = resizedState.layout.editorPanes(paneId).bufferId.getOrElse(fail("Expected active buffer"))

    val contentRect = CursorLayout.contentRectForPane(paneRect)

    resizedState.buffers(bufferId).viewport.visibleColumns shouldBe math.max(1, (contentRect.width * 0.8).toInt)
    resizedState.buffers(bufferId).viewport.visibleLines shouldBe 20
    resizedState.layout.editorPanes(paneId).viewport.visibleColumns shouldBe math.max(
      1,
      (contentRect.width * 0.8).toInt
    )
    resizedState.layout.editorPanes(paneId).viewport.visibleLines shouldBe 20
  }

  it should "sync a newly assigned buffer to the current pane viewport" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
    val viewportSize = ViewportSize(120, 40)
    stateManager.applyEvent(ResizeEvent(viewportSize)).unsafeRunSync()
    val bufferId = stateManager.createBuffer("assigned after resize").unsafeRunSync()
    val paneId = stateManager.getCurrentState
      .unsafeRunSync()
      .layout
      .activeEditorPaneId
      .getOrElse(fail("Expected active pane"))

    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val updatedState     = stateManager.getCurrentState.unsafeRunSync()
    val calculatedLayout = LayoutEngine.calculateLayout(updatedState, viewportSize)
    val paneRect         = LayoutEngine.calculatePaneLayouts(updatedState, calculatedLayout)(paneId)
    val contentRect      = CursorLayout.contentRectForPane(paneRect)
    updatedState.buffers(bufferId).viewport.visibleColumns shouldBe contentRect.width
    updatedState.buffers(bufferId).viewport.visibleLines shouldBe contentRect.height
    updatedState.layout.editorPanes(paneId).viewport.visibleColumns shouldBe contentRect.width
    updatedState.layout.editorPanes(paneId).viewport.visibleLines shouldBe contentRect.height
  }

  it should "handle text wrapping recalculation on resize" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Create a long line of text that will wrap differently at different widths
    val longText =
      "This is a very long line of text that should wrap differently when the terminal width changes and we need to test that the rope structure handles this properly"
    val bufferId = stateManager.createBuffer(longText).unsafeRunSync()
    stateManager.createPane(Some(bufferId)).unsafeRunSync()

    // Start with narrow width (40 chars)
    val initialSize  = ViewportSize(40, 20)
    val resizeEvent1 = ResizeEvent(initialSize)
    stateManager.applyEvent(resizeEvent1).unsafeRunSync()

    val state1  = stateManager.getCurrentState.unsafeRunSync()
    val layout1 = LayoutEngine.calculateLayout(state1, initialSize)

    // Now resize to wider (120 chars)
    val widerSize    = ViewportSize(120, 20)
    val resizeEvent2 = ResizeEvent(widerSize)
    stateManager.applyEvent(resizeEvent2).unsafeRunSync()

    val state2  = stateManager.getCurrentState.unsafeRunSync()
    val layout2 = LayoutEngine.calculateLayout(state2, widerSize)

    // Verify that layout dimensions changed
    layout1.editorPanelRect.width should be < layout2.editorPanelRect.width

    state2.buffers.get(bufferId) match
      case Some(buffer) =>
        val contentRect = CursorLayout.contentRectForPane(layout2.editorPanelRect)
        buffer.viewport.visibleColumns.shouldBe(contentRect.width)
      case None => fail("No buffer found in state")
  }

  it should "detect resize from terminal input" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Initial state - has default viewport dimensions
    val initialState   = stateManager.getCurrentState.unsafeRunSync()
    val activeBuffer   = initialState.buffers(initialState.bufferOrder.head)
    val initialLines   = activeBuffer.viewport.visibleLines
    val initialColumns = activeBuffer.viewport.visibleColumns
    initialLines should be > 0
    initialColumns should be > 0

    // Simulate terminal resize detection
    val newViewportSize = ViewportSize(100, 30)
    val resizeEvent     = ResizeEvent(newViewportSize)

    // Apply the resize event
    stateManager.applyEvent(resizeEvent).unsafeRunSync()

    // State should now reflect the resize
    val resizedState       = stateManager.getCurrentState.unsafeRunSync()
    val resizedBuffer      = resizedState.buffers(resizedState.bufferOrder.head)
    val resizedLayout      = LayoutEngine.calculateLayout(resizedState, newViewportSize)
    val resizedContentRect = CursorLayout.contentRectForPane(resizedLayout.editorPanelRect)
    resizedBuffer.viewport.visibleLines.shouldBe(resizedContentRect.height)
    resizedBuffer.viewport.visibleColumns.shouldBe(resizedContentRect.width)
  }

  it should "recalculate text wrapping when terminal width changes" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Create buffer with text that will wrap at narrow width
    val longLine =
      "This is a very long line that should definitely wrap when displayed in a narrow terminal window but should fit on one line in a wide terminal"
    val bufferId = stateManager.createBuffer(longLine).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Start with narrow terminal (40 chars wide)
    val narrowSize   = ViewportSize(40, 20)
    val resizeEvent1 = ResizeEvent(narrowSize)
    stateManager.applyEvent(resizeEvent1).unsafeRunSync()

    val stateAfterNarrow  = stateManager.getCurrentState.unsafeRunSync()
    val layoutAfterNarrow = LayoutEngine.calculateLayout(stateAfterNarrow, narrowSize)

    // Resize to wide terminal (120 chars wide)
    val wideSize     = ViewportSize(120, 20)
    val resizeEvent2 = ResizeEvent(wideSize)
    stateManager.applyEvent(resizeEvent2).unsafeRunSync()

    val stateAfterWide  = stateManager.getCurrentState.unsafeRunSync()
    val layoutAfterWide = LayoutEngine.calculateLayout(stateAfterWide, wideSize)

    // Verify that layout calculations reflect the size change
    layoutAfterNarrow.editorPanelRect.width.should(be < layoutAfterWide.editorPanelRect.width)

    stateAfterWide.buffers.get(bufferId) match
      case Some(buffer) =>
        val contentRect = CursorLayout.contentRectForPane(layoutAfterWide.editorPanelRect)
        buffer.viewport.visibleColumns.shouldBe(contentRect.width)
        buffer.viewport.visibleLines.shouldBe(contentRect.height)
      case None => fail("No buffer found after wide resize")
  }
