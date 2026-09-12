package com.serenity

import java.awt.Font

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.{AppMode, ToolbarDisplayMode}
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Mouse-driven interaction with the toolbar: selecting and executing items on click (including fractional
  * code-metric pixel offsets), opening dropdown/input details with a click and applying the chosen option or
  * clicked-away value, and ignoring hover/click/drag on the group-separator gutters. Placement/positioning is
  * covered in [[ContextualToolbarPlacementSpec]], keyboard-driven focus and detail lifecycle in
  * [[ContextualToolbarDetailSpec]], and display-mode/rendering behaviour in [[ContextualToolbarDisplaySpec]].
  */
class ContextualToolbarMouseSpec extends AnyFlatSpec with Matchers with ContextualToolbarTestSupport:

  "Contextual toolbar" should "select and execute toolbar items on click without stealing editor focus" in {
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
                state.persisted.config.editorConfig.fontConfig.copy(
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
