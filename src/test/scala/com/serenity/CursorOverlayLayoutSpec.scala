package com.serenity

import com.serenity.command.*
import com.serenity.config.{AppConfig, InterfaceDensity}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CursorOverlayLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState(cursor: CursorPosition = CursorPosition(6, 18)): AppState =
    val buffer = Buffer
      .fromString(
        bufferId,
        List.fill(20)("abcdefghijklmnopqrstuvwxyz").mkString("\n")
      )
      .copy(editing = EditingState(cursors = List(cursor)))
    val pane = EditorPane.withBuffer(paneId, bufferId)

    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  "LayoutEngine.calculateLayout" should "place a peek overlay above the anchored cursor when space is available" in {
    val base = baseState()
    val state = base.copy(
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("peek"),
            SurfaceContent.QuickInfo("map(value: A => B): List[B]"),
            SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.AboveCursor),
            dismissOnMove = true
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.aboveCursorOverlayRect shouldBe defined
    layout.belowCursorOverlayRect shouldBe None

    val rect          = layout.aboveCursorOverlayRect.get
    val paneRect      = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect   = CursorLayout.contentRectForPane(paneRect)
    val contentTopY   = paneRect.y + 1
    val cursorScreenY = contentTopY + 6

    rect.bottom should be <= cursorScreenY
    rect.x shouldBe contentRect.x
    rect.width shouldBe contentRect.width
    rect.right shouldBe contentRect.right
    rect.y should be >= contentTopY
  }

  it should "clamp an above-cursor peek overlay into the active pane when the cursor is near the top" in {
    val base = baseState(cursor = CursorPosition(0, 5))
    val state = base.copy(
      persisted = base.persisted.copy(config = AppState.initial.persisted.config.withUiElementGap(0.5)),
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("peek-top"),
            SurfaceContent.QuickInfo("near-top"),
            SurfacePresentation.Floating(Some(CursorPosition(0, 5)), SurfacePlacement.AboveCursor),
            dismissOnMove = true
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 20))

    layout.aboveCursorOverlayRect shouldBe defined

    val rect        = layout.aboveCursorOverlayRect.get
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)
    val contentTopY = paneRect.y + 1

    rect.y shouldBe contentTopY
    rect.bottom should be <= paneRect.bottom
    rect.x shouldBe contentRect.x
    rect.width shouldBe contentRect.width
    layout.floatingOverlayOffsetRows(SurfaceId("peek-top")) shouldBe 0.0
  }

  it should "place an active command runner below the editor cursor when there is room" in {
    val base = baseState()
    val state = base.copy(
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(
              CommandRunner(
                isActive = true,
                searchTerm = "",
                selectedIndex = 0,
                filteredCommands = List.empty
              )
            ),
            SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.aboveCursorOverlayRect shouldBe None
    layout.belowCursorOverlayRect shouldBe defined

    val rect        = layout.belowCursorOverlayRect.get
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    rect.y shouldBe contentRect.y + 8
    rect.x shouldBe contentRect.x
    rect.width shouldBe 72
    rect.right should be < contentRect.right
    rect.bottom should be <= paneRect.bottom
  }

  it should "place compact command runner overlays directly below the cursor row" in {
    val base = baseState()
    val state = base.copy(
      persisted = base.persisted.copy(config =
        AppState.initial.persisted.config.withInterfaceDensity(com.serenity.config.InterfaceDensity.Compact)
      ),
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(
              CommandRunner(
                isActive = true,
                searchTerm = "",
                selectedIndex = 0,
                filteredCommands = List.empty
              )
            ),
            SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    val rect        = layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay"))
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    rect.y shouldBe contentRect.y + 7
  }

  it should "use the explicit command runner cursor gap instead of the interface gap" in {
    val base = baseState()
    val state = base.copy(
      persisted = base.persisted
        .copy(config = AppState.initial.persisted.config.withUiElementGap(0).withCommandRunnerCursorGapRows(Some(3))),
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(
              CommandRunner(isActive = true, searchTerm = "", selectedIndex = 0, filteredCommands = Nil)
            ),
            SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout      = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val contentRect = LayoutEngine.calculateEditorWorkspaceLayout(state, layout).activeContentRect(state).get
    val rect        = layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay"))

    rect.y shouldBe contentRect.y + 10
  }

  it should "place command runner overlays immediately below a top-row cursor" in {
    val cursor = CursorPosition(0, 0)
    val base   = baseState(cursor = cursor)
    val state = base.copy(
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(
              CommandRunner(
                isActive = true,
                searchTerm = "",
                selectedIndex = 0,
                filteredCommands = List.empty
              )
            ),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    val rect        = layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay"))
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    rect.y shouldBe contentRect.y + 2
  }

  it should "place command runner overlays from the live editor cursor rather than a stale surface anchor" in {
    val cursor      = CursorPosition(0, 0)
    val staleAnchor = CursorPosition(3, 0)
    val base        = baseState(cursor = cursor)
    val state = base.copy(
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(
              CommandRunner(
                isActive = true,
                searchTerm = "",
                selectedIndex = 0,
                filteredCommands = List.empty
              )
            ),
            SurfacePresentation.Floating(Some(staleAnchor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    val rect        = layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay"))
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    rect.y shouldBe contentRect.y + 2
  }

  it should "place command runner overlays below the visible wrapped cursor row" in {
    val longLine = "a" * 260
    val cursor   = CursorPosition(0, 215)
    val buffer = Buffer
      .fromString(bufferId, longLine)
      .copy(
        editing = EditingState(cursors = List(cursor)),
        viewport = Viewport(topLine = 0, topVisualLine = 2, leftColumn = 0, visibleColumns = 80, visibleLines = 20)
      )
    val pane    = EditorPane.withBuffer(paneId, bufferId)
    val initial = AppState.initial
    val state = initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = initial.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(
              CommandRunner(
                isActive = true,
                searchTerm = "",
                selectedIndex = 0,
                filteredCommands = List.empty
              )
            ),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    val rect        = layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay"))
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    rect.y shouldBe contentRect.y + 2
  }

  it should "place command runner overlays below the visible unwrapped cursor row after scrolling past long lines" in {
    val longLine = "a" * 260
    val cursor   = CursorPosition(31, 0)
    val lines =
      (0 until 60).map {
        case 16 => longLine
        case i  => s"line $i"
      }
    val buffer = Buffer
      .fromString(bufferId, lines.mkString("\n"))
      .copy(
        editing = EditingState(cursors = List(cursor)),
        viewport = Viewport(topLine = 8, leftColumn = 0, visibleLines = 30, visibleColumns = 80)
      )
    val pane    = EditorPane.withBuffer(paneId, bufferId)
    val initial = AppState.initial
    val state = initial.copy(
      persisted = initial.persisted.copy(
        config = initial.persisted.config.withWordWrap(false),
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = initial.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(
              CommandRunner(
                isActive = true,
                searchTerm = "",
                selectedIndex = 0,
                filteredCommands = List.empty
              )
            ),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 40))

    val rect        = layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay"))
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    rect.y shouldBe contentRect.y + (cursor.line - buffer.viewport.topLine) + 2
  }

  it should "size command runner overlays from configured visible rows" in {
    val base = baseState()
    val state = base.copy(
      persisted = base.persisted.copy(config = AppState.initial.persisted.config.withCommandRunnerVisibleRows(Some(7))),
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(
              CommandRunner(
                isActive = true,
                searchTerm = "",
                selectedIndex = 0,
                filteredCommands = List.empty
              )
            ),
            SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.belowCursorOverlayRect.map(_.height) shouldBe Some(18)
  }

  it should "apply configured gaps below the cursor and between stacked overlays" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val cursor = CursorPosition(1, 2)
    val base   = baseState(cursor = cursor)
    val state = base.copy(
      persisted = base.persisted.copy(config = AppState.initial.persisted.config.withUiElementGap(2)),
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("contextual-toolbar"),
            SurfaceContent.ContextualToolbar(ContextualToolbarState()),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          ),
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout      = LayoutEngine.calculateLayout(state, ViewportSize(100, 40))
    val paneLayout  = LayoutEngine.calculateEditorPaneLayouts(state, layout)(paneId)
    val contentRect = paneLayout.contentRect
    val stack       = layout.belowCursorOverlayStack.toMap

    val toolbarRect = stack.getOrElse(SurfaceId("contextual-toolbar"), fail("Expected toolbar overlay"))
    val runnerRect  = stack.getOrElse(SurfaceId("command-runner"), fail("Expected command runner overlay"))

    toolbarRect.y shouldBe contentRect.y + cursor.line + 3
    runnerRect.y shouldBe toolbarRect.bottom + 2
  }

  it should "preserve fractional cursor and nested-surface gaps as logical-pixel offsets" in {
    val cursor = CursorPosition(1, 2)
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val base   = baseState(cursor = cursor)
    val state = base.copy(
      persisted = base.persisted.copy(config =
        AppState.initial.persisted.config
          .withUiElementGap(0.25)
          .withCommandRunnerCursorGapRows(Some(0.5))
      ),
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("contextual-toolbar"),
            SurfaceContent.ContextualToolbar(ContextualToolbarState()),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          ),
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    layout.floatingOverlayOffsetRows(SurfaceId("contextual-toolbar")) shouldBe -0.25
    layout.floatingOverlayOffsetRows(SurfaceId("command-runner")) shouldBe -0.5
  }

  it should "keep command runner cursor and submenu stack gaps independent" in {
    val registry                               = com.serenity.command.CommandRegistry.default
    given com.serenity.command.CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(com.serenity.command.CommandCategory.Settings)
      .enterSelectedGroup
    val cursor = CursorPosition(1, 2)
    val base   = baseState(cursor = cursor)
    val state = base.copy(
      persisted = base.persisted
        .copy(config = AppState.initial.persisted.config.withUiElementGap(1).withCommandRunnerCursorGapRows(Some(3))),
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          ),
          UiSurface(
            SurfaceId("command-runner-submenu"),
            SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = false),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout      = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val contentRect = LayoutEngine.calculateEditorWorkspaceLayout(state, layout).activeContentRect(state).get
    val stack       = layout.belowCursorOverlayStack.toMap

    val runnerRect  = stack.getOrElse(SurfaceId("command-runner"), fail("Expected command runner overlay"))
    val submenuRect = stack.getOrElse(SurfaceId("command-runner-submenu"), fail("Expected submenu overlay"))

    runnerRect.y shouldBe contentRect.y + cursor.line + 4
    submenuRect.y shouldBe runnerRect.bottom + 1
  }

  it should "move a command runner stack above the cursor as one unit when it cannot fit below" in {
    val registry                               = com.serenity.command.CommandRegistry.default
    given com.serenity.command.CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(com.serenity.command.CommandCategory.Settings)
      .enterSelectedGroup
    val cursor = CursorPosition(18, 4)
    val base   = baseState(cursor = cursor)
    val state = base.copy(
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          ),
          UiSurface(
            SurfaceId("command-runner-submenu"),
            SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = false),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout      = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val paneLayout  = LayoutEngine.calculateEditorPaneLayouts(state, layout)(paneId)
    val contentRect = paneLayout.contentRect
    val cursorY     = contentRect.y + cursor.line
    val stack       = layout.belowCursorOverlayStack.toMap

    val runnerRect  = stack.getOrElse(SurfaceId("command-runner"), fail("Expected command runner overlay"))
    val submenuRect = stack.getOrElse(SurfaceId("command-runner-submenu"), fail("Expected submenu overlay"))

    runnerRect.y should be >= contentRect.y
    runnerRect.bottom should be <= submenuRect.y
    submenuRect.bottom should be <= cursorY
    layout.collapsedFloatingSurfaceIds should contain(SurfaceId("command-runner"))
  }

  it should "size a find overlay to fit its header, query row, result, and footer" in {
    val base = baseState()
    val state = base.copy(
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("find"),
            SurfaceContent.ModalWorkflow(Modal.Find("needle", List(FindResult(1, 0), FindResult(3, 0)), 0)),
            SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.belowCursorOverlayRect.map(_.height) shouldBe Some(6)
  }

  it should "size a close workflow overlay to fit its text and density-aware action targets" in {
    val base = baseState()
    val state = base.copy(
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("close"),
            SurfaceContent.ModalWorkflow(
              Modal.CloseWorkflow(
                CloseWorkflowState(
                  scope = CloseScope.Current,
                  currentBufferId = BufferId(0),
                  currentBufferLabel = "Buffer 0 - unsaved"
                )
              )
            ),
            SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    layout.belowCursorOverlayRect.map(_.height) shouldBe Some(
      ModalSurfaceComposition.closeFrameHeight(
        SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
      )
    )
  }

  it should "size a replace overlay to fit fields, actions, scope, and status" in
    List(InterfaceDensity.Comfortable, InterfaceDensity.Spacious).foreach { density =>
      val modal = Modal.ReplaceWorkflow(ReplaceWorkflowState(statusMessage = Some("3 matches will be replaced")))
      val base  = baseState()
      val state = base.copy(
        persisted = base.persisted.copy(config = base.persisted.config.withInterfaceDensity(density)),
        runtime = base.runtime.copy(uiSurfaces =
          List(
            UiSurface(
              SurfaceId("replace"),
              SurfaceContent.ModalWorkflow(modal),
              SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
            )
          )
        )
      )

      val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

      layout.belowCursorOverlayRect.map(_.height) shouldBe Some(
        ModalSurfaceComposition.frameHeight(modal, targetRows = 2)
      )
    }
end CursorOverlayLayoutSpec
