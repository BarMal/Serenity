package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.TextAreaInsets
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MouseClickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  final case class NonCollectingRope(delegate: Rope) extends Rope:
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override def newlineCount: Int =
      delegate.newlineCount

    override def lastLineLength: Int =
      delegate.lastLineLength

    override def endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def lineCount: Int =
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      delegate.getLine(lineIndex)

    override def lineColumnToOffset(line: Int, column: Int): Int =
      delegate.lineColumnToOffset(line, column)

    override def offsetToLineColumn(offset: Int): (Int, Int) =
      delegate.offsetToLineColumn(offset)

    override def collect(): String =
      throw AssertionError("mouse word selection should not materialise the whole buffer")

  // Layout at 80x24, showLineNumbers=true, showGutter=true:
  //   gutterHeight=1  → contentHeight=23
  //   spacerWidth=(80*0.15).toInt=12
  //   lineNumberWidth for 4-line buffer = max(3, "4".length+1) = 3
  //   editorPanelRect = LayoutRect(x=15, y=0, width=53, height=23)
  //   PaneId(0) → LayoutRect(x=15, y=0, width=53, height=23)
  //   contentRow starts at y=1 (header at y=0)

  "MouseClick" should "move cursor to the clicked buffer position" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("aaaa\nbbbb\ncccc\ndddd").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Click at (18, 3): bufferLine = topLine(0) + (3-1) = 2, bufferCol = leftCol(0) + (18-15) = 3
    sm.applyEvent(MouseClick(6, 3)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption.map(_.line) shouldBe Some(2)
    buffer.editing.cursors.headOption.map(_.column) shouldBe Some(3)
  }

  it should "move cursor to the first row of the content area" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Click at (15, 1): first cell of content area → bufferLine=0, bufferCol=0
    sm.applyEvent(MouseClick(3, 1)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption.map(_.line) shouldBe Some(0)
    buffer.editing.cursors.headOption.map(_.column) shouldBe Some(0)
  }

  it should "not move the cursor for non-primary clicks" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(
              document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
              editing = state.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 1)))
            )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors shouldBe List(CursorPosition(0, 1))
  }

  it should "consume workspace clicks, presses, and drags while a close confirmation is active" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha\nbeta\ngamma").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val close = UiSurface(
      SurfaceId("close-confirmation"),
      SurfaceContent.ModalWorkflow(
        Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala"))
      ),
      SurfacePresentation.Modal
    )
    sm.updateState(state => state.copy(uiSurfaces = state.uiSurfaces :+ close, focus = Focus.Surface(close.id)))
      .unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val paneRect = LayoutEngine
      .calculatePaneLayouts(before, LayoutEngine.calculateLayout(before, ViewportSize(80, 24)))
      .getOrElse(PaneId(0), fail("Expected editor pane"))

    sm.applyEvent(MouseClick(paneRect.x + 4, paneRect.y + 2)).unsafeRunSync()
    sm.applyEvent(MousePress(paneRect.x + 5, paneRect.y + 2)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 8, paneRect.y + 3)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.buffers(bufferId).editing.cursors shouldBe before.buffers(bufferId).editing.cursors
    after.buffers(bufferId).primarySelection shouldBe before.buffers(bufferId).primarySelection
    after.focus shouldBe Focus.Surface(close.id)
    after.topBlockingModalSurface.map(_.id) shouldBe Some(close.id)
  }

  it should "route a click inside a close confirmation to its cancel action" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val close = UiSurface(
      SurfaceId("close-confirmation"),
      SurfaceContent.ModalWorkflow(
        Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala"))
      ),
      SurfacePresentation.Modal
    )
    sm.updateState(state => state.copy(uiSurfaces = state.uiSurfaces :+ close, focus = Focus.Surface(close.id)))
      .unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val modal = UiSceneSnapshot
      .from(before, ViewportSize(80, 24))
      .modal
      .lastOption
      .getOrElse(fail("Expected close confirmation modal"))
    val targetRows = SurfaceFrameLayout.minimumTargetRows(before.config.interfaceDensity)
    val cancel = ModalSurfaceComposition
      .forModal(
        Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala")),
        modal.frameRect,
        targetRows
      )
      .getOrElse(fail("Expected close confirmation composition"))
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId("close-cancel")))
      .getOrElse(fail("Expected cancel action"))
    val cancelX  = cancel.rect.x.toInt
    val choicesY = cancel.rect.y.toInt

    sm.applyEvent(MouseClick(cancelX, choicesY)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModalSurface shouldBe None
    after.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "route a reflowed close action inside a constrained modal frame" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    val viewport = ViewportSize(40, 4)
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    val workflow = CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala")
    val close = UiSurface(
      SurfaceId("close-constrained"),
      SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)),
      SurfacePresentation.Modal
    )
    sm.updateState(state => state.copy(uiSurfaces = state.uiSurfaces :+ close, focus = Focus.Surface(close.id)))
      .unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val modal = UiSceneSnapshot
      .from(before, viewport)
      .modal
      .lastOption
      .getOrElse(fail("Expected constrained close modal"))
    val cancel = ModalSurfaceComposition
      .forModal(
        Modal.CloseWorkflow(workflow),
        modal.frameRect,
        SurfaceFrameLayout.minimumTargetRows(before.config.interfaceDensity)
      )
      .getOrElse(fail("Expected close confirmation composition"))
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId("close-cancel")))
      .getOrElse(fail("Expected reflowed cancel action"))

    sm.applyEvent(MouseClick(cancel.rect.x.toInt, cancel.rect.y.toInt)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModalSurface shouldBe None
    after.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "route find and replace modal hit regions through their reducers" in {
    val findManager = makeStateManager()
    val findBuffer  = findManager.createBuffer("needle\nneedle").unsafeRunSync()
    findManager.setBufferForPane(PaneId(0), findBuffer).unsafeRunSync()
    findManager.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val findSurface = UiSurface(
      SurfaceId("find-click"),
      SurfaceContent.ModalWorkflow(Modal.Find("needle", List(FindResult(0, 0), FindResult(1, 0)), 0)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    findManager
      .updateState(state =>
        state.copy(uiSurfaces = state.uiSurfaces :+ findSurface, focus = Focus.Surface(findSurface.id))
      )
      .unsafeRunSync()
    val findState = findManager.getCurrentState.unsafeRunSync()
    val findNode = UiSceneSnapshot
      .from(findState, ViewportSize(80, 24))
      .floating
      .find(_.id == SceneNodeId.Surface(findSurface.id))
      .getOrElse(fail("Expected floating find modal"))
    val findHit = ModalSurfaceComposition
      .forModal(findSurface.content.asInstanceOf[SurfaceContent.ModalWorkflow].modal, findNode.frameRect, 2)
      .get
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId("find-result-0")))
      .getOrElse(fail("Expected find result hit region"))
    findManager.applyEvent(MouseClick(findHit.rect.x.toInt, findHit.rect.y.toInt)).unsafeRunSync()
    findManager.getCurrentState
      .unsafeRunSync()
      .modalSurface
      .flatMap(_.content match
        case SurfaceContent.ModalWorkflow(Modal.Find(_, _, currentIndex)) => Some(currentIndex)
        case _                                                            => None) shouldBe Some(0)

    val replaceManager = makeStateManager()
    val replaceBuffer  = replaceManager.createBuffer("needle").unsafeRunSync()
    replaceManager.setBufferForPane(PaneId(0), replaceBuffer).unsafeRunSync()
    replaceManager.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val replaceSurface = UiSurface(
      SurfaceId("replace-click"),
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(ReplaceWorkflowState())),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    replaceManager
      .updateState(state =>
        state.copy(uiSurfaces = state.uiSurfaces :+ replaceSurface, focus = Focus.Surface(replaceSurface.id))
      )
      .unsafeRunSync()
    val replaceState = replaceManager.getCurrentState.unsafeRunSync()
    val replaceNode = UiSceneSnapshot
      .from(replaceState, ViewportSize(80, 24))
      .floating
      .find(_.id == SceneNodeId.Surface(replaceSurface.id))
      .getOrElse(fail("Expected floating replace modal"))
    val replaceHit = ModalSurfaceComposition
      .forModal(
        replaceSurface.content.asInstanceOf[SurfaceContent.ModalWorkflow].modal,
        replaceNode.frameRect,
        SurfaceFrameLayout.minimumTargetRows(replaceState.config.interfaceDensity)
      )
      .get
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId("replace-selection")))
      .getOrElse(fail("Expected replace scope hit region"))
    replaceManager
      .applyEvent(
        MouseClick(
          (replaceHit.rect.x + replaceHit.rect.width / 2).toInt,
          (replaceHit.rect.y + replaceHit.rect.height / 2).toInt
        )
      )
      .unsafeRunSync()
    replaceManager.getCurrentState
      .unsafeRunSync()
      .modalSurface
      .flatMap(_.content match
        case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => Some(workflow.selectedScope)
        case _ => None) shouldBe Some(ReplaceWorkflowScope.Selection)

    val fileManager = makeStateManager()
    val fileBuffer  = fileManager.createBuffer("needle").unsafeRunSync()
    fileManager.setBufferForPane(PaneId(0), fileBuffer).unsafeRunSync()
    fileManager.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val fileSurface = UiSurface(
      SurfaceId("file-click"),
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          FileWorkflowState(
            mode = FileWorkflowMode.Open,
            suggestions = List(
              FileWorkflowSuggestion("notes.scala"),
              FileWorkflowSuggestion("README.md")
            ),
            selectedSuggestionIndex = 1
          )
        )
      ),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    fileManager
      .updateState(state =>
        state.copy(uiSurfaces = state.uiSurfaces :+ fileSurface, focus = Focus.Surface(fileSurface.id))
      )
      .unsafeRunSync()
    val fileState = fileManager.getCurrentState.unsafeRunSync()
    val fileNode = UiSceneSnapshot
      .from(fileState, ViewportSize(80, 24))
      .floating
      .find(_.id == SceneNodeId.Surface(fileSurface.id))
      .getOrElse(fail("Expected floating file modal"))
    val fileHit = ModalSurfaceComposition
      .forModal(
        fileSurface.content.asInstanceOf[SurfaceContent.ModalWorkflow].modal,
        fileNode.frameRect,
        SurfaceFrameLayout.minimumTargetRows(fileState.config.interfaceDensity)
      )
      .get
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId("file-suggestion-0")))
      .getOrElse(fail("Expected file suggestion hit region"))
    fileManager
      .applyEvent(
        MouseClick(
          (fileHit.rect.x + fileHit.rect.width / 2).toInt,
          (fileHit.rect.y + fileHit.rect.height / 2).toInt
        )
      )
      .unsafeRunSync()
    fileManager.getCurrentState
      .unsafeRunSync()
      .modalSurface
      .flatMap(_.content match
        case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some(workflow.selectedSuggestionIndex)
        case _                                                          => None) shouldBe Some(0)
  }

  it should "open an editor context menu on secondary click without moving the cursor" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(
              document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
              editing = state.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 1)))
            )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    val state  = sm.getCurrentState.unsafeRunSync()
    val buffer = state.buffers(bufferId)
    buffer.editing.cursors shouldBe List(CursorPosition(0, 1))
    val menu = state.contextMenuSurface
      .flatMap {
        _.content match
          case SurfaceContent.ContextMenu(menu) => Some(menu)
          case _                                => None
      }
      .getOrElse(fail("Expected editor context menu"))
    menu.targetFocus shouldBe Focus.EditorPane(PaneId(0))
    menu.items.map(_.id) should contain allOf (
      "copy",
      "cut",
      "paste",
      "select-all",
      "save",
      "find",
      "replace",
      "bold",
      "italic",
      "underline",
      "heading-1",
      "heading-2",
      "heading-3",
      "paragraph-body",
      "align-left",
      "align-center",
      "align-right",
      "align-justify",
      "goto-line",
      "toggle-bookmark",
      "add-document-comment",
      "delete-document-comment",
      "next-document-comment",
      "previous-document-comment",
      "navigate-back",
      "navigate-forward",
      "next-document-symbol",
      "previous-document-symbol"
    )
    state.focus shouldBe Focus.Surface(SurfaceId("context-menu"))
  }

  it should "execute the clicked context menu command against the target editor pane" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    val openedState = sm.getCurrentState.unsafeRunSync()
    val menu = openedState.contextMenuSurface
      .flatMap {
        _.content match
          case SurfaceContent.ContextMenu(menu) => Some(menu)
          case _                                => None
      }
      .getOrElse(fail("Expected editor context menu"))
    val copyIndex = menu.items.indexWhere(_.id == "copy")
    copyIndex should be >= 0
    val (x, y) = contextMenuItemPoint(openedState, copyIndex)

    sm.applyEvent(MouseClick(x, y)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.contextMenuSurface shouldBe None
    after.focus shouldBe Focus.EditorPane(PaneId(0))
    after.clipboard shouldBe Some("hello")
  }

  it should "not select a context menu item when clicking a configured item gap" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState(state => state.copy(config = state.config.withCommandRunnerItemGapRows(1))).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    val openedState = sm.getCurrentState.unsafeRunSync()
    val viewport    = openedState.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface     = openedState.contextMenuSurface.getOrElse(fail("Expected context menu surface"))
    val layout      = LayoutEngine.calculateLayoutWithUI(openedState, viewport)
    val contract    = EditorLayoutContract.from(openedState, viewport, layout)
    val contentRect = contract
      .overlayContentRect(surface.id)
      .getOrElse(fail("Expected context menu overlay content rect"))
    val firstItemRow = contract
      .overlayRowSlots(surface.id)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(0), y) => y }
      .getOrElse(fail("Expected first context menu item row"))

    sm.applyEvent(MouseClick(contentRect.x + 1, firstItemRow + 2)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.contextMenuSurface shouldBe Some(surface)
  }

  it should "dismiss the context menu on Escape" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    sm.applyEvent(Escape).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.contextMenuSurface shouldBe None
    after.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "clamp column to line length when clicking past end of line" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hi\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Click at (35, 1): bufferLine=0, bufferCol = 35-15 = 20, "hi" length=2 → clamp to 2
    sm.applyEvent(MouseClick(35, 1)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption.map(_.line) shouldBe Some(0)
    buffer.editing.cursors.headOption.map(_.column) shouldBe Some(2)
  }

  it should "track the editor position under the pointer on mouse move" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    sm.applyEvent(MouseMove(6, 2)).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().hoveredEditorTarget shouldBe Some(
      HoveredEditorTarget(PaneId(0), bufferId, CursorPosition(1, 3))
    )
  }

  it should "clear the editor hover target when the pointer leaves editor panes" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello\nworld").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    sm.applyEvent(MouseMove(6, 2)).unsafeRunSync()
    sm.applyEvent(MouseMove(0, 23)).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().hoveredEditorTarget shouldBe None
  }

  it should "ignore clicks in the pane header row" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val initialCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).editing.cursors.headOption

    // Click at row=0 (header row of pane at y=0) — should be ignored
    sm.applyEvent(MouseClick(20, 0)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).editing.cursors.headOption
    afterCursor shouldBe initialCursor
  }

  it should "ignore clicks in the spacer area outside any pane" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState(state => state.copy(config = state.config.withTextAreaInsets(TextAreaInsets(0.15, 0.15))))
      .unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val initialCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).editing.cursors.headOption

    // Click at col=5 (left spacer, pane starts at col=15) — should be ignored
    sm.applyEvent(MouseClick(5, 5)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).editing.cursors.headOption
    afterCursor shouldBe initialCursor
  }

  it should "ignore clicks when terminal size has not been set" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    // No ResizeEvent applied — ViewportSize is None

    val initialCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).editing.cursors.headOption

    sm.applyEvent(MouseClick(20, 5)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().buffers(bufferId).editing.cursors.headOption
    afterCursor shouldBe initialCursor
  }

  it should "use pixel-aware hit testing for proportional text when pixel coordinates are available" in {
    given LoggerFactory[IO]                 = Slf4jFactory.create[IO]
    given org.typelevel.log4cats.Logger[IO] = org.typelevel.log4cats.slf4j.Slf4jLogger.getLogger[IO]

    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("iW").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Markdown)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state       = sm.getCurrentState.unsafeRunSync()
    val layout      = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))
    val contentRect = CursorLayout.contentRectForPane(paneRect)
    val font        = FontLoader.previewFontForRole(state.config.fontConfig, TypographyRole.Prose)
    // Mouse pixels arrive in the screen grid's coordinates, and that grid is the code font's even for prose.
    val gridMetrics  = CellMetrics.fromFont(FontLoader.previewFontForRole(state.config.fontConfig, TypographyRole.Code))
    val panelWidthPx = contentRect.width * gridMetrics.charWidth
    val snapshot     = TextLayoutSnapshot.fromBuffer(state.buffers(bufferId), panelWidthPx, font)
    val line         = snapshot.visualLines.head
    val pixelX       = contentRect.x * gridMetrics.charWidth + math.round(line.xForColumn(1).getOrElse(0.0f) + 1.0f)
    val pixelY       = contentRect.y * gridMetrics.lineHeight

    sm.applyEvent(
      MouseClick(
        col = contentRect.x,
        row = contentRect.y,
        pixelX = Some(pixelX),
        pixelY = Some(pixelY)
      )
    ).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(com.serenity.state.models.CursorPosition(0, 1))
  }

  it should "create a selection while dragging inside an editor pane" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha\nbravo\ncharlie").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MousePress(paneRect.x + 1, paneRect.y + 1)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 3, paneRect.y + 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(1, 3))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(1, 3)))
    buffer.editing.selections shouldBe Nil
  }

  it should "start a new drag selection from the latest press instead of reusing an old anchor" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha\nbravo\ncharlie").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MousePress(paneRect.x + 1, paneRect.y + 1)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 3, paneRect.y + 2)).unsafeRunSync()
    sm.applyEvent(MousePress(paneRect.x + 2, paneRect.y + 2)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 5, paneRect.y + 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(1, 5))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(1, 2), CursorPosition(1, 5)))
    buffer.editing.selections shouldBe Nil
  }

  it should "select the clicked word on double click" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha beta gamma").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 7, paneRect.y + 1, clickCount = 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(0, 10))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    buffer.editing.selections shouldBe Nil
  }

  it should "select the clicked word on double click without materialising the whole buffer" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha beta gamma").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(
              document = state
                .buffers(bufferId)
                .document
                .copy(content = NonCollectingRope(Rope("alpha beta gamma")), language = Some(LanguageId.Scala))
            )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 7, paneRect.y + 1, clickCount = 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(0, 10))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    buffer.editing.selections shouldBe Nil
  }

  it should "select the clicked line on triple click" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha\nbeta gamma\ncharlie").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 2, paneRect.y + 2, clickCount = 3)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(1, 10))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(1, 0), CursorPosition(1, 10)))
    buffer.editing.selections shouldBe Nil
  }

  it should "extend the current selection from the existing anchor on shift-click" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha\nbeta gamma\ncharlie").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(
              document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
              editing = state.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 2)))
            )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 4, paneRect.y + 2, shiftDown = true)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(1, 4))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(1, 4)))
    buffer.editing.selections shouldBe Nil
  }

  it should "preserve the original anchor while extending with shift-drag" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha\nbeta gamma\ncharlie").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(
              document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
              editing = state.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 2)))
            )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MousePress(paneRect.x + 4, paneRect.y + 2, shiftDown = true)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 5, paneRect.y + 3, shiftDown = true)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(CursorPosition(2, 5))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(2, 5)))
    buffer.editing.selections shouldBe Nil
  }

  it should "collapse multi-cursor state to the clicked cursor" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha\nbeta\ngamma").unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(
              document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
              editing = state
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(CursorPosition(0, 1), CursorPosition(2, 3)),
                  multiCursorVerticalStates = List(
                    VerticalCursorState(CursorPosition(0, 1), 1, 1.0f),
                    VerticalCursorState(CursorPosition(2, 3), 3, 3.0f)
                  )
                )
            )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MouseClick(paneRect.x + 2, paneRect.y + 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors shouldBe List(CursorPosition(1, 2))
    buffer.editing.selection shouldBe None
    buffer.editing.selections shouldBe Nil
    buffer.editing.multiCursorVerticalStates shouldBe Nil
  }

  it should "collapse multi-selection state to a single drag selection" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("alpha\nbeta\ngamma").unsafeRunSync()
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 2))
    val second   = Selection(CursorPosition(2, 0), CursorPosition(2, 2))
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        buffers = state.buffers.updated(
          bufferId,
          state
            .buffers(bufferId)
            .copy(
              document = state.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
              editing = state
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(first.focus, second.focus),
                  selection = Some(first),
                  selections = List(first, second)
                )
            )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))

    sm.applyEvent(MousePress(paneRect.x + 1, paneRect.y + 1)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 3, paneRect.y + 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().buffers(bufferId)
    buffer.editing.cursors shouldBe List(CursorPosition(1, 3))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(1, 3)))
    buffer.editing.selections shouldBe Nil
  }

  private def contextMenuItemPoint(state: AppState, itemIndex: Int): (Int, Int) =
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextMenuSurface.getOrElse(fail("Expected context menu surface"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contract = EditorLayoutContract.from(state, viewport, layout)
    val contentRect = contract
      .overlayContentRect(surface.id)
      .getOrElse(fail("Expected context menu overlay content rect"))
    val rowY = contract
      .overlayRowSlots(surface.id)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(`itemIndex`), y) => y }
      .getOrElse(fail(s"Expected context menu item row $itemIndex"))
    (contentRect.x + 1, rowY)
