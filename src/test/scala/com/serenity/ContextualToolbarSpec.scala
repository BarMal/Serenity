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

class ContextualToolbarSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "Contextual toolbar" should "toggle on below the cursor without stealing focus and stack above the command runner" in {
    val stateManager = createStateManager("ContextualToolbarSpec-stack")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val opened         = stateManager.getCurrentState.unsafeRunSync()
    val toolbarSurface = opened.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar"))
    opened.focus shouldBe Focus.EditorPane(PaneId(0))
    toolbarSurface.presentation shouldBe SurfacePresentation.Floating(
      opened.activeCursorPosition,
      SurfacePlacement.BelowCursor
    )
    toolbarStateFrom(opened).displayMode shouldBe ToolbarDisplayMode.IconOnly

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val withRunner = stateManager.getCurrentState.unsafeRunSync()
    val layout = LayoutEngine
      .calculateLayoutWithUI(withRunner, withRunner.viewportSize.getOrElse(fail("Expected viewport size")))
    layout.aboveCursorOverlayStack.map(_._1) shouldBe Nil
    layout.belowCursorOverlayStack.map(_._1) shouldBe List(
      toolbarSurface.id,
      withRunner.commandRunnerSurface.getOrElse(fail("Expected command runner")).id
    )

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val afterClose = stateManager.getCurrentState.unsafeRunSync()
    afterClose.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "leave editor typing active while the toolbar is open" in {
    val stateManager = createStateManager("ContextualToolbarSpec-editor-focus")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha"),
            cursors = List(CursorPosition(0, 5))
          )
        state.copy(buffers = state.buffers.updated(bufferId, buffer))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    state.buffers(bufferId).content.toString shouldBe "alpha!"
    state.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "follow the active cursor while it remains open" in {
    val stateManager = createStateManager("ContextualToolbarSpec-follow-caret")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope(
              "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu\nnu xi omicron pi rho sigma tau"
            )
          )
        state.copy(buffers = state.buffers.updated(bufferId, buffer))
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
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta gamma"),
            selection = None,
            cursors = List(CursorPosition(0, 10)),
            richTextDocument = Some(document)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
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
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    focusToolbar(stateManager)
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.buffers(bufferId)
    buffer.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .shouldBe(Some(Set(InlineMark.Bold)))
    state.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "dismiss on Escape and restore editor focus" in {
    val stateManager = createStateManager("ContextualToolbarSpec-escape")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    focusToolbar(stateManager)
    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.contextualToolbarSurface shouldBe None
    state.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "move below the cursor line when there is no room above the selection" in {
    val stateManager = createStateManager("ContextualToolbarSpec-top-row-placement")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 20))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
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
    val buffer   = state.buffers(bufferId)
    buffer.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.fontSize)
      .shouldBe(Some(20.0f))
    state.focus shouldBe Focus.EditorPane(PaneId(0))
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
    val buffer   = state.buffers(bufferId)
    buffer.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.fontFamily)
      .shouldBe(Some("Serif"))
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
    val buffer   = state.buffers(bufferId)
    buffer.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.color)
      .shouldBe(Some("#ff6600"))
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
    afterFirstEscape.focus shouldBe Focus.Surface(
      afterFirstEscape.contextualToolbarSurface.getOrElse(fail("Expected toolbar surface")).id
    )

    stateManager.applyEvent(Escape).unsafeRunSync()

    val afterSecondEscape = stateManager.getCurrentState.unsafeRunSync()
    afterSecondEscape.contextualToolbarSurface shouldBe None
    afterSecondEscape.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "select and execute toolbar items on click without stealing editor focus" in {
    val stateManager = createStateManager("ContextualToolbarSpec-mouse")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before = stateManager.getCurrentState.unsafeRunSync()
    val point  = toolbarItemPoint(before, itemId = "italic")

    stateManager.applyEvent(MouseMove(point.x, point.y)).unsafeRunSync()

    val afterHover = stateManager.getCurrentState.unsafeRunSync()
    toolbarStateFrom(afterHover).focusedIndex shouldBe toolbarStateFrom(before).focusedIndex
    afterHover.focus shouldBe before.focus

    stateManager.applyEvent(MouseClick(point.x, point.y)).unsafeRunSync()

    val after    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(after)
    after
      .buffers(bufferId)
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .shouldBe(Some(Set(InlineMark.Italic)))
    after.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "restore editor focus when a button is clicked while a toolbar detail is open" in {
    val stateManager = createStateManager("ContextualToolbarSpec-mouse-button-after-detail")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    val dropdownPoint = toolbarItemPoint(stateManager.getCurrentState.unsafeRunSync(), "paragraph-role")
    stateManager.applyEvent(MouseClick(dropdownPoint.x, dropdownPoint.y)).unsafeRunSync()

    val withOpenDetail = stateManager.getCurrentState.unsafeRunSync()
    withOpenDetail.focus shouldBe Focus.Surface(
      withOpenDetail.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar")).id
    )

    val buttonPoint = toolbarItemPoint(withOpenDetail, "bold")
    stateManager.applyEvent(MouseClick(buttonPoint.x, buttonPoint.y)).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "open a paragraph role dropdown and apply the clicked option" in {
    val stateManager = createStateManager("ContextualToolbarSpec-role-dropdown")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val triggerPoint = toolbarItemPoint(stateManager.getCurrentState.unsafeRunSync(), "paragraph-role")
    stateManager.applyEvent(MouseClick(triggerPoint.x, triggerPoint.y)).unsafeRunSync()

    val openedDropdown = stateManager.getCurrentState.unsafeRunSync()
    openedDropdown.focus shouldBe Focus.Surface(
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
    state
      .buffers(bufferId)
      .richTextDocument
      .flatMap(_.paragraphs.headOption)
      .map(_.role)
      .shouldBe(Some(ParagraphRole.Heading(1)))
    state.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "open a paragraph role dropdown and apply heading level 4" in {
    val stateManager = createStateManager("ContextualToolbarSpec-role-dropdown-h4")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
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
    state
      .buffers(bufferId)
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
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
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
    state
      .buffers(bufferId)
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
        state.copy(config = state.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly))
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
          CommandIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.config.contextualToolbarDisplayMode shouldBe ToolbarDisplayMode.IconOnly
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
        options = List(com.serenity.command.CommandOption("Serif", CommandIntent.SetRichTextFontFamily("Serif"))),
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
        parse = _.toFloatOption.map(CommandIntent.SetRichTextFontSize(_)),
        category = CommandCategory.Edit
      )
    )

    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.IconOnly) shouldBe "Serif ▾"
    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.TextOnly) shouldBe "Font Serif"
    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.IconAndText) shouldBe "\ue167 Font Serif"

    ContextualToolbar.displayText(input, ToolbarDisplayMode.IconOnly) shouldBe "18"
    ContextualToolbar.displayText(input, ToolbarDisplayMode.TextOnly) shouldBe "Size 18"
    ContextualToolbar.displayText(input, ToolbarDisplayMode.IconAndText) shouldBe "\ue245 Size 18"
  }

  it should "use Material Icons Round code points in icon-only mode" in {
    ContextualToolbar.markdownItems.map(_.icon) shouldBe List("\uf1c5", "\ue86f", "\uf06d", "\ue8b6")
    ContextualToolbar.codeItems.map(_.icon) shouldBe List("\ue869", "\ue86c", "\ue037", "\ue868")

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

  it should "render compact toolbar controls alongside icon-only action glyphs" in {
    val stateManager = createStateManager("ContextualToolbarSpec-rendered-glyphs")
    val viewport     = ViewportSize(120, 30)
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
    ContextualToolbar.itemsFor(state).collect { case button: ContextualToolbarItem.Button => button.icon }.foreach(
      renderedText should include(_)
    )
    val compactControlText = ContextualToolbar.itemsFor(state).collect {
      case dropdown: ContextualToolbarItem.Dropdown => ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.IconOnly)
      case input: ContextualToolbarItem.Input       => ContextualToolbar.displayText(input, ToolbarDisplayMode.IconOnly)
    }
    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      toolbarStateFrom(state),
      state,
      LayoutRect(0, 0, 120, 10),
      SurfaceRenderMode.Floating
    )
    val resolvedText = resolved.rows.flatMap(_.segments).map(_.text)
    compactControlText.foreach(resolvedText should contain(_))
    surface.setFontCalls.map(_.getFamily) should contain(FontLoader.ToolbarIconFontFamily)
  }

  it should "render icon-font glyphs alongside labels in IconAndText mode" in {
    val stateManager = createStateManager("ContextualToolbarSpec-rendered-icon-and-text")
    val viewport     = ViewportSize(120, 30)
    stateManager
      .updateState(state =>
        state.copy(config = state.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconAndText))
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

  it should "keep prose formatting controls in semantic clusters when rows wrap" in {
    val stateManager = createStateManager("ContextualToolbarSpec-clustered-rows")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val rowGroups = ContextualToolbar
      .rowGroups(
        ContextualToolbar.itemsFor(stateManager.getCurrentState.unsafeRunSync()),
        24,
        ToolbarDisplayMode.IconOnly
      )
      .map(_.map(_.id))

    rowGroups shouldBe List(
      List("bold", "italic", "underline"),
      List("font-family", "font-family-text", "font-size"),
      List("color", "color-hex"),
      List("paragraph-role", "align-left", "align-center", "align-right", "align-justify")
    )
  }

  it should "use one compact row when the editor has room for all formatting controls" in {
    val stateManager = createStateManager("ContextualToolbarSpec-compact-row")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val state        = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = ContextualToolbarState(displayMode = ToolbarDisplayMode.IconOnly)
    val width        = ContextualToolbar.compactContentWidth(toolbarState, state, maxWidth = 120)

    width shouldBe 70
    ContextualToolbar.rowCount(toolbarState, state, width) shouldBe 1
  }

  it should "move focus vertically between wrapped toolbar rows" in {
    val stateManager = createStateManager("ContextualToolbarSpec-vertical-top-level")

    stateManager.applyEvent(ResizeEvent(ViewportSize(26, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before       = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = toolbarStateFrom(before)
    val contentWidth = toolbarContentWidth(before)
    val rowGroups =
      ContextualToolbar.rowGroups(ContextualToolbar.itemsFor(before), contentWidth, toolbarState.displayMode)
    rowGroups.length should be > 1

    val (startItemId, expectedDownItemId) = verticalTopLevelPair(rowGroups)
    moveToolbarFocusTo(stateManager, startItemId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    focusedToolbarItemId(stateManager.getCurrentState.unsafeRunSync()) shouldBe expectedDownItemId

    stateManager.applyEvent(MoveUp).unsafeRunSync()
    focusedToolbarItemId(stateManager.getCurrentState.unsafeRunSync()) shouldBe startItemId
  }

  it should "move dropdown selection vertically between wrapped option rows" in {
    val items = List(
      ContextualToolbarItem.Dropdown(
        id = "paragraph-role",
        label = "Role",
        icon = "P",
        optionItem = CommandSurfaceItem.OptionItem(
          id = "paragraph-role",
          label = "Role",
          options = List(
            com.serenity.command.CommandOption("Body", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Body)),
            com.serenity.command.CommandOption("H1", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(1))),
            com.serenity.command.CommandOption("H2", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(2))),
            com.serenity.command.CommandOption("H3", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(3)))
          ),
          selectedIndex = 1,
          category = CommandCategory.Edit
        )
      )
    )
    val toolbarState =
      ContextualToolbarState(detailState = Some(ContextualToolbarDetailState.Dropdown("paragraph-role", 1)))

    val movedDown = toolbarState.moveDetailSelectionVertical(1, items, contentWidth = 12)
    movedDown.detailState shouldBe Some(ContextualToolbarDetailState.Dropdown("paragraph-role", 3))

    val movedUp = movedDown.moveDetailSelectionVertical(-1, items, contentWidth = 12)
    movedUp.detailState shouldBe Some(ContextualToolbarDetailState.Dropdown("paragraph-role", 1))
  }

  private case class Point(x: Int, y: Int)

  private def seedToolbarDocument(stateManager: com.serenity.state.manager.StateManager): Unit =
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val range     = RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 10))
        val document = RichTextDocument
          .fromPlainText("alpha beta")
          .applyMark(range, InlineMark.Bold)
          .setFontFamily(range, "A")
          .setFontSize(range, 18.0f)
          .setColor(range, "#336699")
          .setParagraphRole(range, ParagraphRole.Body)
          .setParagraphAlignment(range, ParagraphAlignment.Left)
          .normalized
        val nextBuffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus),
            richTextDocument = Some(document)
          )
        state.copy(buffers = state.buffers.updated(bufferId, nextBuffer))
      }
      .unsafeRunSync()

  private def toolbarItemPoint(state: AppState, itemId: String): Point =
    val itemIndex    = toolbarItemIndex(state, itemId)
    val rect         = toolbarRect(state)
    val toolbarState = toolbarStateFrom(state)
    val contentRect  = SurfaceFrameLayout.forContent(rect, SurfaceContent.ContextualToolbar(toolbarState)).contentRect
    val rowGroups =
      ContextualToolbar.rowGroups(ContextualToolbar.itemsFor(state), contentRect.width.max(1), toolbarState.displayMode)
    val (rowIndex, localIndex) = rowGroups.zipWithIndex
      .collectFirst {
        case (row, currentRowIndex) if itemIndex < row.length + rowGroups.take(currentRowIndex).map(_.length).sum =>
          val offset = rowGroups.take(currentRowIndex).map(_.length).sum
          (currentRowIndex, itemIndex - offset)
      }
      .getOrElse(fail(s"Expected toolbar item index $itemIndex"))
    val rowItems = rowGroups.lift(rowIndex).getOrElse(fail(s"Expected toolbar row $rowIndex"))
    Point(
      x = contentRect.x + hitColumnCenter(localIndex, rowItems.length, contentRect.width),
      y = toolbarRowY(state, rowIndex)
    )

  private def toolbarDetailPoint(state: AppState, itemId: String, optionLabel: String): Point =
    val rect         = toolbarRect(state)
    val toolbarState = toolbarStateFrom(state)
    val contentRect  = SurfaceFrameLayout.forContent(rect, SurfaceContent.ContextualToolbar(toolbarState)).contentRect
    val rowGroups =
      ContextualToolbar.rowGroups(ContextualToolbar.itemsFor(state), contentRect.width.max(1), toolbarState.displayMode)
    val detailRows =
      ContextualToolbar.detailRowGroups(toolbarState, ContextualToolbar.itemsFor(state), contentRect.width.max(1))
    val optionIndex = ContextualToolbar
      .dropdownItem(itemId, ContextualToolbar.itemsFor(state))
      .map(_.optionItem.options.indexWhere(_.label == optionLabel))
      .filter(_ >= 0)
      .getOrElse(fail(s"Expected toolbar detail option $optionLabel for $itemId"))
    val (rowIndex, localIndex) = detailRows.zipWithIndex
      .collectFirst {
        case (rowOptions, currentRowIndex)
            if optionIndex < rowOptions.length + detailRows.take(currentRowIndex).map(_.length).sum =>
          val offset = detailRows.take(currentRowIndex).map(_.length).sum
          (currentRowIndex, optionIndex - offset)
      }
      .getOrElse(fail(s"Expected toolbar detail option $optionIndex"))
    val rowOptions = detailRows.lift(rowIndex).getOrElse(fail(s"Expected toolbar detail row $rowIndex"))
    Point(
      x = contentRect.x + hitColumnCenter(localIndex, rowOptions.length, contentRect.width),
      y = toolbarRowY(state, rowGroups.length + rowIndex)
    )

  private def toolbarRowY(state: AppState, displayedRowIndex: Int): Int =
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))
    contract.floatingOverlayRowSlots
      .getOrElse(surface.id, Nil)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(`displayedRowIndex`), y) => y }
      .getOrElse(fail(s"Expected toolbar content row $displayedRowIndex"))

  private def toolbarRect(state: AppState) =
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))
    contract
      .overlayRect(surface.id)
      .getOrElse(fail("Expected toolbar overlay rect"))

  private def toolbarContentWidth(state: AppState): Int =
    val toolbarState = toolbarStateFrom(state)
    SurfaceFrameLayout
      .forContent(toolbarRect(state), SurfaceContent.ContextualToolbar(toolbarState))
      .contentRect
      .width
      .max(1)

  private def toolbarStateFrom(state: AppState): ContextualToolbarState =
    state.contextualToolbarSurface
      .flatMap {
        _.content match
          case SurfaceContent.ContextualToolbar(toolbarState) => Some(toolbarState)
          case _                                              => None
      }
      .getOrElse(fail("Expected contextual toolbar state"))

  private def toolbarButton(state: AppState, itemId: String): ContextualToolbarItem.Button =
    ContextualToolbar
      .itemsFor(state)
      .collectFirst {
        case item: ContextualToolbarItem.Button if item.id == itemId => item
      }
      .getOrElse(fail(s"Expected toolbar button $itemId"))

  private def toolbarInput(state: AppState, itemId: String): ContextualToolbarItem.Input =
    ContextualToolbar
      .itemsFor(state)
      .collectFirst {
        case item: ContextualToolbarItem.Input if item.id == itemId => item
      }
      .getOrElse(fail(s"Expected toolbar input $itemId"))

  private def focusedToolbarItemId(state: AppState): String =
    val items = ContextualToolbar.itemsFor(state)
    toolbarStateFrom(state)
      .normalized(items)
      .focusedItem(items)
      .map(_.id)
      .getOrElse(fail("Expected focused toolbar item"))

  private def verticalTopLevelPair(rowGroups: List[List[ContextualToolbarItem]]): (String, String) =
    rowGroups.zipWithIndex
      .collectFirst {
        case (rowItems, rowIndex)
            if rowIndex < rowGroups.length - 1 && rowItems.length > 1 && rowGroups(rowIndex + 1).length > 1 =>
          val localIndex = rowItems.length - 1
          (
            rowItems(localIndex).id,
            rowGroups(rowIndex + 1)(verticalTargetIndex(localIndex, rowItems.length, rowGroups(rowIndex + 1).length)).id
          )
      }
      .getOrElse(fail("Expected wrapped toolbar rows with multiple items"))

  private def verticalTargetIndex(currentIndex: Int, currentRowLength: Int, targetRowLength: Int): Int =
    (((currentIndex + 0.5d) * targetRowLength) / currentRowLength).toInt
      .max(0)
      .min(targetRowLength - 1)

  private def hitColumnCenter(localIndex: Int, itemCount: Int, contentWidth: Int): Int =
    val start = ((localIndex * contentWidth) + itemCount - 1) / itemCount
    val end   = ((((localIndex + 1) * contentWidth) + itemCount - 1) / itemCount) - 1
    start + math.max(0, (end - start) / 2)

  private def moveToolbarFocusTo(stateManager: com.serenity.state.manager.StateManager, itemId: String): Unit =
    focusToolbar(stateManager)
    val state     = stateManager.getCurrentState.unsafeRunSync()
    val target    = toolbarItemIndex(state, itemId)
    val toolbar   = toolbarStateFrom(state)
    val itemCount = ContextualToolbar.itemsFor(state).length
    val delta     = (target - toolbar.focusedIndex + itemCount) % itemCount
    (0 until delta).foreach(_ => stateManager.applyEvent(MoveRight).unsafeRunSync())

  private def focusToolbar(stateManager: com.serenity.state.manager.StateManager): Unit =
    stateManager
      .updateState { state =>
        val toolbarId = state.contextualToolbarSurface
          .map(_.id)
          .getOrElse(fail("Expected contextual toolbar surface"))
        state.pushFocus(Focus.Surface(toolbarId))
      }
      .unsafeRunSync()

  private def toolbarItemIndex(state: AppState, itemId: String): Int =
    ContextualToolbar.itemsFor(state).indexWhere(_.id == itemId) match
      case -1    => fail(s"Expected toolbar item $itemId")
      case index => index

  private def activeBufferId(state: AppState): BufferId =
    state.layout.activeEditorPaneId
      .flatMap(state.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .getOrElse(fail("Expected active buffer"))
