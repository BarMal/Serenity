package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.ToolbarDisplayMode
import com.serenity.keystroke.events.*
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, SurfaceFrameLayout, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextualToolbarSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "Contextual toolbar" should "toggle on, keep command runner stacked beneath it, and restore toolbar focus when the runner closes" in {
    val stateManager = createStateManager("ContextualToolbarSpec-stack")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val opened         = stateManager.getCurrentState.unsafeRunSync()
    val toolbarSurface = opened.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar"))
    opened.focus shouldBe Focus.Surface(toolbarSurface.id)

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val withRunner = stateManager.getCurrentState.unsafeRunSync()
    val stackIds = LayoutEngine
      .calculateLayoutWithUI(withRunner, withRunner.viewportSize.getOrElse(fail("Expected viewport size")))
      .belowCursorOverlayStack
      .map(_._1)
    stackIds.take(2) shouldBe List(
      toolbarSurface.id,
      withRunner.commandRunnerSurface.getOrElse(fail("Expected command runner")).id
    )

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val afterClose = stateManager.getCurrentState.unsafeRunSync()
    afterClose.focus shouldBe Focus.Surface(toolbarSurface.id)
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
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.buffers(bufferId)
    buffer.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .shouldBe(Some(Set(InlineMark.Bold)))
  }

  it should "dismiss on Escape and restore editor focus" in {
    val stateManager = createStateManager("ContextualToolbarSpec-escape")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.contextualToolbarSurface shouldBe None
    state.focus shouldBe Focus.EditorPane(PaneId(0))
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
  }

  it should "close an open toolbar control on Escape before dismissing the toolbar" in {
    val stateManager = createStateManager("ContextualToolbarSpec-escape-detail")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
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

  it should "highlight and execute toolbar items with the mouse" in {
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

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).focusedIndex shouldBe 1

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

    val optionPoint = toolbarDetailPoint(
      stateManager.getCurrentState.unsafeRunSync(),
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
    val rowItems  = rowGroups.lift(rowIndex).getOrElse(fail(s"Expected toolbar row $rowIndex"))
    val slotWidth = math.max(1, contentRect.width / rowItems.length)
    Point(
      x = contentRect.x + (slotWidth * localIndex) + math.max(0, slotWidth / 2),
      y = contentRect.y + rowIndex
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
    val slotWidth  = math.max(1, contentRect.width / rowOptions.length)
    Point(
      x = contentRect.x + (slotWidth * localIndex) + math.max(0, slotWidth / 2),
      y = contentRect.y + rowGroups.length + rowIndex
    )

  private def toolbarRect(state: AppState) =
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    LayoutEngine
      .calculateLayoutWithUI(state, viewport)
      .belowCursorOverlayStack
      .collectFirst { case (`surface`.id, rect) => rect }
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

  private def moveToolbarFocusTo(stateManager: com.serenity.state.manager.StateManager, itemId: String): Unit =
    val state     = stateManager.getCurrentState.unsafeRunSync()
    val target    = toolbarItemIndex(state, itemId)
    val toolbar   = toolbarStateFrom(state)
    val itemCount = ContextualToolbar.itemsFor(state).length
    val delta     = (target - toolbar.focusedIndex + itemCount) % itemCount
    (0 until delta).foreach(_ => stateManager.applyEvent(MoveRight).unsafeRunSync())

  private def toolbarItemIndex(state: AppState, itemId: String): Int =
    ContextualToolbar.itemsFor(state).indexWhere(_.id == itemId) match
      case -1    => fail(s"Expected toolbar item $itemId")
      case index => index

  private def activeBufferId(state: AppState): BufferId =
    state.layout.activeEditorPaneId
      .flatMap(state.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .getOrElse(fail("Expected active buffer"))
