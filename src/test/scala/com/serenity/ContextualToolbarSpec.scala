package com.serenity

import java.awt.Font

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.ToolbarDisplayMode
import com.serenity.keystroke.events.*
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{Renderer, SurfaceContentResolver, SurfaceRenderMode}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Focus, selection, and detail-open/close state, and the toolbar's behaviour end to end: placement, keyboard
  * navigation, mouse selection, and command execution. Pure cell-space geometry (row wrapping, widths, hit-testing) is
  * covered directly in [[ContextualToolbarLayoutSpec]].
  */
class ContextualToolbarSpec extends AnyFlatSpec with Matchers with ContextualToolbarTestSupport:

  "Contextual toolbar" should "toggle on below the cursor without stealing focus and stack above the command runner" in {
    val stateManager = createStateManager("ContextualToolbarSpec-stack")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val opened         = stateManager.getCurrentState.unsafeRunSync()
    val toolbarSurface = opened.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar"))
    opened.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    toolbarSurface.presentation shouldBe SurfacePresentation.Floating(
      opened.activeCursorPosition,
      SurfacePlacement.BelowCursor
    )
    toolbarStateFrom(opened).displayMode shouldBe ToolbarDisplayMode.IconAndText

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val withRunner = stateManager.getCurrentState.unsafeRunSync()
    val layout = LayoutEngine
      .calculateLayoutWithUI(withRunner, withRunner.runtime.viewportSize.getOrElse(fail("Expected viewport size")))
    layout.aboveCursorOverlayStack.map(_._1) shouldBe Nil
    layout.belowCursorOverlayStack.map(_._1) shouldBe List(
      toolbarSurface.id,
      withRunner.commandRunnerSurface.getOrElse(fail("Expected command runner")).id
    )

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val afterClose = stateManager.getCurrentState.unsafeRunSync()
    afterClose.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "leave editor typing active while the toolbar is open" in {
    val stateManager = createStateManager("ContextualToolbarSpec-editor-focus")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha")),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 5)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    state.persisted.buffers(bufferId).document.content.toString shouldBe "alpha!"
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "follow the active cursor while it remains open" in {
    val stateManager = createStateManager("ContextualToolbarSpec-follow-caret")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content =
                com.serenity.rope.Rope(
                  "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu\nnu xi omicron pi rho sigma tau"
                )
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    val initialRect = toolbarRect(stateManager.getCurrentState.unsafeRunSync())
    initialRect.width should be < 100
    stateManager.setCursorPosition(PaneId(0), 0, 48).unsafeRunSync()

    val movedHorizontally = toolbarRect(stateManager.getCurrentState.unsafeRunSync())
    movedHorizontally.x should be > initialRect.x

    stateManager.setCursorPosition(PaneId(0), 1, 12).unsafeRunSync()

    val movedVertically = toolbarRect(stateManager.getCurrentState.unsafeRunSync())
    movedVertically.y should be > movedHorizontally.y
  }

  it should "separate paragraph-role and alignment controls into their own compact groups" in {
    val stateManager = createStateManager("ContextualToolbarSpec-semantic-groups")

    seedToolbarDocument(stateManager)

    val items              = ContextualToolbar.itemsFor(stateManager.getCurrentState.unsafeRunSync())
    val paragraphRoleIndex = items.indexWhere(_.id == "paragraph-role")
    val paragraphRole      = items.lift(paragraphRoleIndex).getOrElse(fail("Expected paragraph role control"))
    val alignment          = items.lift(paragraphRoleIndex + 1).getOrElse(fail("Expected alignment control"))

    ContextualToolbar.hasTrailingGroupSeparator(paragraphRole, Some(alignment)) shouldBe true
  }

  it should "keep the formatted run state when the caret sits on its trailing boundary" in {
    val stateManager = createStateManager("ContextualToolbarSpec-caret-boundary-style")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val document = RichTextDocument
          .fromPlainText("alpha beta gamma")
          .applyMark(
            RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
            InlineMark.Bold
          )
          .setFontFamily(
            RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
            "Serif"
          )
          .setFontSize(
            RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
            18.0f
          )
          .setColor(
            RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
            "#336699"
          )
          .normalized
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document =
              state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta gamma")),
            editing =
              state.persisted.buffers(bufferId).editing.copy(selection = None, cursors = List(CursorPosition(0, 10))),
            richText = state.persisted.buffers(bufferId).richText.copy(richTextDocument = Some(document))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    toolbarButton(state, "bold").selected shouldBe true
    toolbarInput(state, "font-family-text").inputItem.currentValue shouldBe "Serif"
    toolbarInput(state, "font-size").inputItem.currentValue shouldBe "18"
    toolbarInput(state, "color-hex").inputItem.currentValue shouldBe "#336699"
  }

  it should "execute the focused formatting command on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-enter")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    focusToolbar(stateManager)
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.persisted.buffers(bufferId)
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .shouldBe(Some(Set(InlineMark.Bold)))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "dismiss on Escape and restore editor focus" in {
    val stateManager = createStateManager("ContextualToolbarSpec-escape")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    focusToolbar(stateManager)
    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.contextualToolbarSurface shouldBe None
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "move below the cursor line when there is no room above the selection" in {
    val stateManager = createStateManager("ContextualToolbarSpec-top-row-placement")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 20))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(
        fail("Expected active content rect")
      )
    val rect = toolbarRect(state)

    rect.y should be > contentRect.y
    rect.bottom should be <= contentRect.bottom
  }

  it should "prefer the compact palette above the editing target when that safe placement fits" in {
    val stateManager = createStateManager("ContextualToolbarSpec-above-placement")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager
      .updateState { state =>
        val bufferId = activeBufferId(state)
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(12)("toolbar target").mkString("\n"))),
            editing =
              state.persisted.buffers(bufferId).editing.copy(selection = None, cursors = List(CursorPosition(8, 4)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(fail("Expected active content rect"))
    val cursorY = contentRect.y + 8
    val rect    = toolbarRect(state)

    rect.y should be >= cursorY
    rect.y should be >= contentRect.y
  }

  it should "anchor above the start of a multi-line selection rather than its trailing caret" in {
    val stateManager = createStateManager("ContextualToolbarSpec-selection-anchor")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = activeBufferId(state)
        val selection = Selection(CursorPosition(12, 1), CursorPosition(16, 4))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(20)("toolbar selection target").mkString("\n"))),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(fail("Expected active content rect"))

    toolbarRect(state).bottom should be <= contentRect.y + 12
  }

  it should "center on the bounding box of a same-line selection" in {
    val stateManager = createStateManager("ContextualToolbarSpec-inline-selection-center")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = activeBufferId(state)
        val selection = Selection(CursorPosition(12, 60), CursorPosition(12, 100))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(20)("x" * 140).mkString("\n"))),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(fail("Expected active content rect"))

    toolbarRect(state).centerX shouldBe contentRect.x + 80
  }

  it should "place below a top-edge multi-line selection without covering its selected text" in {
    val stateManager = createStateManager("ContextualToolbarSpec-top-edge-selection-placement")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 20))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = activeBufferId(state)
        val selection = Selection(CursorPosition(0, 1), CursorPosition(5, 4))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(12)("toolbar selection target").mkString("\n"))),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contentRect = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .getOrElse(fail("Expected active content rect"))

    toolbarRect(state).y should be >= contentRect.y + 6
  }

  it should "not leave a detached toolbar visible when its anchor scrolls out of view" in {
    val stateManager = createStateManager("ContextualToolbarSpec-offscreen-anchor")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 20))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = activeBufferId(state)
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(content = com.serenity.rope.Rope(List.fill(40)("toolbar target").mkString("\n"))),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(30, 4))),
            viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 10, visibleColumns = 120)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))

    contract.overlayRect(surface.id) shouldBe None
  }

  it should "open a focused font size field with the current value prefilled, accept edits, and apply them on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-font-size")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    moveToolbarFocusTo(stateManager, "font-size")
    stateManager.applyEvent(Enter).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).detailState shouldBe
      Some(ContextualToolbarDetailState.Input("font-size", "18"))

    stateManager.applyEvent(DeleteBackward).unsafeRunSync()
    stateManager.applyEvent(DeleteBackward).unsafeRunSync()
    stateManager.applyEvent(InsertChar('2')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('0')).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.persisted.buffers(bufferId)
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.fontSize)
      .shouldBe(Some(20.0f))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "retain its intrinsic compact width when a font-family detail opens" in {
    val stateManager = createStateManager("ContextualToolbarSpec-font-family-compact-width")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val beforeWidth = toolbarRect(stateManager.getCurrentState.unsafeRunSync()).width
    moveToolbarFocusTo(stateManager, "font-family")
    stateManager.applyEvent(Enter).unsafeRunSync()

    val opened = stateManager.getCurrentState.unsafeRunSync()
    toolbarStateFrom(opened).detailState.getOrElse(fail("Expected font-family dropdown")) shouldBe
      a[ContextualToolbarDetailState.Dropdown]
    toolbarRect(opened).width shouldBe beforeWidth
  }

  it should "open a focused font family field with the current value prefilled, accept edits, and apply them on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-font-family-input")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    moveToolbarFocusTo(stateManager, "font-family-text")
    stateManager.applyEvent(Enter).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).detailState shouldBe
      Some(ContextualToolbarDetailState.Input("font-family-text", "A"))

    stateManager.applyEvent(DeleteBackward).unsafeRunSync()
    "Serif".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.persisted.buffers(bufferId)
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.fontFamily)
      .shouldBe(Some("Serif"))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "open a focused color field with the current value prefilled, accept hex edits, and apply them on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-color-input")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    moveToolbarFocusTo(stateManager, "color-hex")
    stateManager.applyEvent(Enter).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).detailState shouldBe
      Some(ContextualToolbarDetailState.Input("color-hex", "#336699"))

    (0 until 7).foreach(_ => stateManager.applyEvent(DeleteBackward).unsafeRunSync())
    "ff6600".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.persisted.buffers(bufferId)
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.color)
      .shouldBe(Some("#ff6600"))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "close an open toolbar control on Escape before dismissing the toolbar" in {
    val stateManager = createStateManager("ContextualToolbarSpec-escape-detail")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    focusToolbar(stateManager)
    moveToolbarFocusTo(stateManager, "paragraph-role")

    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(Escape).unsafeRunSync()

    val afterFirstEscape = stateManager.getCurrentState.unsafeRunSync()
    afterFirstEscape.contextualToolbarSurface should not be empty
    afterFirstEscape.persisted.focus shouldBe Focus.Surface(
      afterFirstEscape.contextualToolbarSurface.getOrElse(fail("Expected toolbar surface")).id
    )

    stateManager.applyEvent(Escape).unsafeRunSync()

    val afterSecondEscape = stateManager.getCurrentState.unsafeRunSync()
    afterSecondEscape.contextualToolbarSurface shouldBe None
    afterSecondEscape.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "select and execute toolbar items on click without stealing editor focus" in {
    val stateManager = createStateManager("ContextualToolbarSpec-mouse")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = toolbarItemPoint(before, itemId = "italic")

    stateManager.applyEvent(MousePress(point.x, point.y)).unsafeRunSync()

    val afterPress = stateManager.getCurrentState.unsafeRunSync()
    toolbarStateFrom(afterPress).focusedIndex shouldBe toolbarStateFrom(before).focusedIndex
    afterPress.persisted.focus shouldBe before.persisted.focus

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()

    val afterHover = stateManager.getCurrentState.unsafeRunSync()
    toolbarStateFrom(afterHover).focusedIndex shouldBe toolbarStateFrom(before).focusedIndex
    afterHover.persisted.focus shouldBe before.persisted.focus

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(after)
    after.persisted
      .buffers(bufferId)
      .richText
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .shouldBe(Some(Set(InlineMark.Italic)))
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "select toolbar items at their fractional code-metric pixel offset when UI fonts differ" in {
    val stateManager = createStateManager("ContextualToolbarSpec-fractional-mouse")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config
              .withUiElementGap(0.5)
              .withFontConfig(
                state.persisted.config.fontConfig.copy(
                  codeFontFamily = Font.MONOSPACED,
                  fontSize = 24.0f,
                  uiFontFamily = Font.SANS_SERIF,
                  uiFontSize = 8.0f
                )
              )
          )
        )
      )
      .unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = fractionalToolbarPoint(before, toolbarItemPoint(before, itemId = "italic"))

    stateManager
      .applyEvent(MouseClick(point.x, point.y, pixelX = Some(point.pixelX), pixelY = Some(point.pixelY)))
      .unsafeRunSync()

    val after    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(after)
    after.persisted
      .buffers(bufferId)
      .richText
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .getOrElse(fail("Expected styled beta run")) should contain(InlineMark.Italic)
  }

  it should "retain toolbar focus for a clicked text-entry control, then restore editor focus on submit" in {
    val stateManager = createStateManager("ContextualToolbarSpec-mouse-input-focus")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val inputPoint = toolbarItemPoint(stateManager.getCurrentState.unsafeRunSync(), "font-size")
    stateManager.applyEvent(MouseClick(inputPoint.x, inputPoint.y)).unsafeRunSync()

    val editing = stateManager.getCurrentState.unsafeRunSync()
    editing.persisted.focus shouldBe Focus.Surface(
      editing.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar")).id
    )
    toolbarStateFrom(editing).detailState shouldBe Some(ContextualToolbarDetailState.Input("font-size", "18"))

    stateManager.applyEvent(Enter).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "restore editor focus when a button is clicked while a toolbar detail is open" in {
    val stateManager = createStateManager("ContextualToolbarSpec-mouse-button-after-detail")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    val dropdownPoint = toolbarItemPoint(stateManager.getCurrentState.unsafeRunSync(), "paragraph-role")
    stateManager.applyEvent(MouseClick(dropdownPoint.x, dropdownPoint.y)).unsafeRunSync()

    val withOpenDetail = stateManager.getCurrentState.unsafeRunSync()
    withOpenDetail.persisted.focus shouldBe Focus.Surface(
      withOpenDetail.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar")).id
    )

    val buttonPoint = toolbarItemPoint(withOpenDetail, "bold")
    stateManager.applyEvent(MouseClick(buttonPoint.x, buttonPoint.y)).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "open a paragraph role dropdown and apply the clicked option" in {
    val stateManager = createStateManager("ContextualToolbarSpec-role-dropdown")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val triggerPoint = toolbarItemPoint(stateManager.getCurrentState.unsafeRunSync(), "paragraph-role")
    stateManager.applyEvent(MouseClick(triggerPoint.x, triggerPoint.y)).unsafeRunSync()

    val openedDropdown = stateManager.getCurrentState.unsafeRunSync()
    openedDropdown.persisted.focus shouldBe Focus.Surface(
      openedDropdown.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar")).id
    )

    val optionPoint = toolbarDetailPoint(
      openedDropdown,
      itemId = "paragraph-role",
      optionLabel = "H1"
    )
    stateManager.applyEvent(MouseClick(optionPoint.x, optionPoint.y)).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    state.persisted
      .buffers(bufferId)
      .richText
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .map(_.role)
      .shouldBe(Some(ParagraphRole.Heading(1)))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "open a paragraph role dropdown and apply heading level 4" in {
    val stateManager = createStateManager("ContextualToolbarSpec-role-dropdown-h4")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val triggerPoint = toolbarItemPoint(stateManager.getCurrentState.unsafeRunSync(), "paragraph-role")
    stateManager.applyEvent(MouseClick(triggerPoint.x, triggerPoint.y)).unsafeRunSync()

    val optionPoint = toolbarDetailPoint(
      stateManager.getCurrentState.unsafeRunSync(),
      itemId = "paragraph-role",
      optionLabel = "H4"
    )
    stateManager.applyEvent(MouseClick(optionPoint.x, optionPoint.y)).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    state.persisted
      .buffers(bufferId)
      .richText
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .map(_.role)
      .shouldBe(Some(ParagraphRole.Heading(4)))
  }

  it should "open a color dropdown and apply the clicked preset" in {
    val stateManager = createStateManager("ContextualToolbarSpec-color-dropdown")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val triggerPoint = toolbarItemPoint(stateManager.getCurrentState.unsafeRunSync(), "color")
    stateManager.applyEvent(MouseClick(triggerPoint.x, triggerPoint.y)).unsafeRunSync()

    val optionPoint = toolbarDetailPoint(
      stateManager.getCurrentState.unsafeRunSync(),
      itemId = "color",
      optionLabel = "Blue"
    )
    stateManager.applyEvent(MouseClick(optionPoint.x, optionPoint.y)).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    state.persisted
      .buffers(bufferId)
      .richText
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.color)
      .shouldBe(Some("#336699"))
  }

  it should "open with the configured display mode and refresh when the preference changes" in {
    val stateManager = createStateManager("ContextualToolbarSpec-display-mode")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).displayMode shouldBe ToolbarDisplayMode.TextOnly

    stateManager
      .executeCommand(
        Command.typed(
          "contextual-toolbar-icon-only",
          "Set contextual toolbar display to icon only",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.config.surfaceConfig.contextualToolbarDisplayMode shouldBe ToolbarDisplayMode.IconOnly
    toolbarStateFrom(state).displayMode shouldBe ToolbarDisplayMode.IconOnly
  }

  it should "render dropdowns and inputs according to the toolbar display mode" in {
    val dropdown = ContextualToolbarItem.Dropdown(
      id = "font-family",
      label = "Font",
      icon = "\ue167",
      optionItem = CommandSurfaceItem.OptionItem(
        id = "font-family",
        label = "Font",
        options = List(
          com.serenity.command
            .CommandOption("Serif", CommandIntent.RichText(RichTextIntent.SetRichTextFontFamily("Serif")))
        ),
        selectedIndex = 0,
        category = CommandCategory.Edit
      )
    )
    val input = ContextualToolbarItem.Input(
      id = "font-size",
      label = "Size",
      icon = "\ue245",
      inputItem = CommandSurfaceItem.InputItem(
        id = "font-size",
        label = "Size",
        hint = "Points",
        currentValue = "18",
        isDecimal = true,
        parse = _.toFloatOption.map(commandIntentArg =>
          CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(commandIntentArg))
        ),
        category = CommandCategory.Edit
      )
    )

    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.IconOnly) shouldBe "\ue167"
    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.TextOnly) shouldBe "Font Serif"
    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.IconAndText) shouldBe "\ue167 Font Serif"

    ContextualToolbar.displayText(input, ToolbarDisplayMode.IconOnly) shouldBe "\ue245"
    ContextualToolbar.displayText(input, ToolbarDisplayMode.TextOnly) shouldBe "Size 18"
    ContextualToolbar.displayText(input, ToolbarDisplayMode.IconAndText) shouldBe "\ue245 Size 18"
  }

  it should "use Material Icons Round code points in icon-only mode" in {
    ContextualToolbar.markdownItems.map(_.icon) shouldBe List("\uf1c5", "\ue86f", "\uf06d", "\ue8b6")
    ContextualToolbar.codeItems.map(_.icon) shouldBe List("\ue869", "\ue86c", "\ue037", "\ue868")
    ContextualToolbar.codeItems.map(_.label) shouldBe List("Build", "Test", "Run", "Run Debug Task")

    val stateManager = createStateManager("ContextualToolbarSpec-glyphs")
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val icons =
      ContextualToolbar
        .itemsFor(stateManager.getCurrentState.unsafeRunSync())
        .map(item => item.id -> item.icon)
        .toMap

    icons shouldBe Map(
      "bold"             -> "\ue238",
      "italic"           -> "\ue23f",
      "underline"        -> "\ue765",
      "font-family"      -> "\ue167",
      "font-family-text" -> "\ue262",
      "font-size"        -> "\ue245",
      "color"            -> "\ue40a",
      "color-hex"        -> "\ue9ef",
      "paragraph-role"   -> "\ue264",
      "align-left"       -> "\ue236",
      "align-center"     -> "\ue234",
      "align-right"      -> "\ue237",
      "align-justify"    -> "\ue235"
    )
  }

  it should "render every compact toolbar control as an icon-only glyph" in {
    val stateManager = createStateManager("ContextualToolbarSpec-rendered-glyphs")
    val viewport     = ViewportSize(120, 30)
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state   = stateManager.getCurrentState.unsafeRunSync()
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new MockRenderSurface(viewport.width, viewport.height)

    Renderer.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      CellMetrics.fromFont(font),
      None
    )

    val renderedText = surface.putStringCalls.map(_.s).mkString
    renderedText should include("│")
    ContextualToolbar.itemsFor(state).map(_.icon).foreach(renderedText should include(_))
    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      toolbarStateFrom(state),
      state,
      LayoutRect(0, 0, 120, 10),
      SurfaceRenderMode.Floating
    )
    resolved.rows.flatMap(_.segments).map(_.text) shouldBe ContextualToolbar.itemsFor(state).map(_.icon)
    surface.setFontCalls.map(_.getFamily) should contain(FontLoader.ToolbarIconFontFamily)
  }

  it should "visually separate semantic formatting control groups" in {
    val stateManager = createStateManager("ContextualToolbarSpec-group-separators")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val state = stateManager.getCurrentState.unsafeRunSync()
    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      ContextualToolbarState(displayMode = ToolbarDisplayMode.IconOnly),
      state,
      LayoutRect(0, 0, 120, 10),
      SurfaceRenderMode.Floating
    )

    resolved.rows.head.segments.filter(_.trailingSeparator).map(_.text) shouldBe List(
      ContextualToolbar.displayText(toolbarButton(state, "underline"), ToolbarDisplayMode.IconOnly),
      ContextualToolbar.displayText(toolbarInput(state, "font-size"), ToolbarDisplayMode.IconOnly),
      ContextualToolbar.displayText(toolbarInput(state, "color-hex"), ToolbarDisplayMode.IconOnly),
      ContextualToolbar.displayText(toolbarDropdown(state, "paragraph-role"), ToolbarDisplayMode.IconOnly)
    )
  }

  it should "ignore hover and clicks on compact toolbar separator gutters" in {
    val stateManager = createStateManager("ContextualToolbarSpec-separator-pointer")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config
              .withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly)
              .withUiElementGap(0.5)
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(78, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before         = stateManager.getCurrentState.unsafeRunSync()
    val separatorPoint = fractionalToolbarPoint(before, toolbarSeparatorPoint(before, separatorIndex = 0))
    val cursorBefore   = before.activeCursorPosition

    stateManager
      .applyEvent(
        MouseMove(
          separatorPoint.x,
          separatorPoint.y,
          pixelX = Some(separatorPoint.pixelX),
          pixelY = Some(separatorPoint.pixelY)
        )
      )
      .unsafeRunSync()
    val afterHover = stateManager.getCurrentState.unsafeRunSync()
    toolbarStateFrom(afterHover) shouldBe toolbarStateFrom(before)

    stateManager
      .applyEvent(
        MouseClick(
          separatorPoint.x,
          separatorPoint.y,
          pixelX = Some(separatorPoint.pixelX),
          pixelY = Some(separatorPoint.pixelY)
        )
      )
      .unsafeRunSync()
    val afterClick = stateManager.getCurrentState.unsafeRunSync()
    toolbarStateFrom(afterClick) shouldBe toolbarStateFrom(before)
    afterClick.activeCursorPosition shouldBe cursorBefore
  }

  it should "ignore fractional toolbar separator drags before editor targeting" in {
    val stateManager = createStateManager("ContextualToolbarSpec-fractional-separator-drag")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config
              .withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly)
              .withUiElementGap(0.5)
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(78, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before         = stateManager.getCurrentState.unsafeRunSync()
    val separatorPoint = fractionalToolbarPoint(before, toolbarSeparatorPoint(before, separatorIndex = 0))

    stateManager
      .applyEvent(
        MouseDrag(
          separatorPoint.x,
          separatorPoint.y,
          pixelX = Some(separatorPoint.pixelX),
          pixelY = Some(separatorPoint.pixelY)
        )
      )
      .unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.activeCursorPosition shouldBe before.activeCursorPosition
    after.persisted.buffers(activeBufferId(after)).primarySelection shouldBe before.persisted
      .buffers(activeBufferId(before))
      .primarySelection
  }

  it should "ignore fractional toolbar separator secondary clicks before opening an editor context menu" in {
    val stateManager = createStateManager("ContextualToolbarSpec-fractional-separator-secondary-click")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config
              .withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly)
              .withUiElementGap(0.5)
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(78, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before         = stateManager.getCurrentState.unsafeRunSync()
    val separatorPoint = fractionalToolbarPoint(before, toolbarSeparatorPoint(before, separatorIndex = 0))

    stateManager
      .applyEvent(
        MouseClick(
          separatorPoint.x,
          separatorPoint.y,
          pixelX = Some(separatorPoint.pixelX),
          pixelY = Some(separatorPoint.pixelY),
          button = MouseButton.Secondary
        )
      )
      .unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.contextMenuSurface shouldBe None
    after.activeCursorPosition shouldBe before.activeCursorPosition
  }

  it should "render icon-font glyphs alongside labels in IconAndText mode" in {
    val stateManager = createStateManager("ContextualToolbarSpec-rendered-icon-and-text")
    val viewport     = ViewportSize(120, 30)
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconAndText))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state   = stateManager.getCurrentState.unsafeRunSync()
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new MockRenderSurface(viewport.width, viewport.height)

    Renderer.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      CellMetrics.fromFont(font),
      None
    )

    val renderedText = surface.putStringCalls.map(_.s).mkString
    ContextualToolbar.itemsFor(state).foreach { item =>
      renderedText should include(item.icon)
      renderedText should include(ContextualToolbar.displayText(item, ToolbarDisplayMode.TextOnly))
    }
    surface.setFontCalls.map(_.getFamily) should contain(
      FontLoader.toolbarIconFontFamily.getOrElse(fail("Expected bundled toolbar icon font"))
    )
  }

  it should "use toolbar glyphs supported by the bundled Material Icons Round font" in {
    val stateManager = createStateManager("ContextualToolbarSpec-font-coverage")
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val glyphs =
      ContextualToolbar.markdownItems.map(_.icon) ++
        ContextualToolbar.codeItems.map(_.icon) ++
        ContextualToolbar.itemsFor(stateManager.getCurrentState.unsafeRunSync()).map(_.icon)
    val font = FontLoader.toolbarIconFont(24.0f).getOrElse(fail("Expected bundled Material Icons Round font"))

    glyphs.foreach { glyph =>
      withClue(s"Font '${font.getFontName}' cannot display toolbar glyph '$glyph': ") {
        font.canDisplayUpTo(glyph) shouldBe -1
      }
    }
  }

  it should "preserve a selected hex value at the exact compact toolbar width" in {
    val stateManager = createStateManager("ContextualToolbarSpec-compact-selected-hex")
    val viewport     = ViewportSize(78, 30)

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    moveToolbarFocusTo(stateManager, "color-hex")

    val state = stateManager.getCurrentState.unsafeRunSync()
    toolbarContentWidth(state) shouldBe 55

    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new MockRenderSurface(viewport.width, viewport.height)
    Renderer.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      CellMetrics.fromFont(font),
      None
    )

    val row        = surface.getRow(toolbarRowY(state, 0))
    val hexStart   = row.indexOf(toolbarInput(state, "color-hex").icon)
    val separatorX = row.indexOf('│')
    hexStart should be >= 0
    separatorX should be >= 0
    surface.getBg(hexStart, toolbarRowY(state, 0)) shouldBe state.persisted.theme.highlighted.background
    surface.getBg(separatorX, toolbarRowY(state, 0)) shouldBe state.persisted.theme.panel.background
  }
