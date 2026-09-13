package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MouseClickModalSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  "MouseClick" should "consume workspace clicks, presses, and drags while a close confirmation is active" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha\nbeta\ngamma", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val close = ModalDialog(
      SurfaceId("close-confirmation"),
      Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala")),
      ModalPlacement.Centered
    )
    sm.updateState(state =>
      state.copy(
        persisted = state.persisted.copy(focus = Focus.Modal),
        runtime = state.runtime.copy(modalStack = state.runtime.modalStack :+ close)
      )
    ).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val paneRect = LayoutEngine
      .calculatePaneLayouts(before, LayoutEngine.calculateLayout(before, ViewportSize(80, 24)))
      .getOrElse(PaneId(0), fail("Expected editor pane"))

    sm.applyEvent(MouseClick(paneRect.x + 4, paneRect.y + 2)).unsafeRunSync()
    sm.applyEvent(MousePress(paneRect.x + 5, paneRect.y + 2)).unsafeRunSync()
    sm.applyEvent(MouseDrag(paneRect.x + 8, paneRect.y + 3)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.persisted.buffers(bufferId).editing.cursors shouldBe before.persisted.buffers(bufferId).editing.cursors
    after.persisted.buffers(bufferId).primarySelection shouldBe before.persisted.buffers(bufferId).primarySelection
    after.persisted.focus shouldBe Focus.Modal
    after.topModal.map(_.id) shouldBe Some(close.id)
  }

  it should "route a click inside a close confirmation to its cancel action" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val close = ModalDialog(
      SurfaceId("close-confirmation"),
      Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala")),
      ModalPlacement.Centered
    )
    sm.updateState(state =>
      state.copy(
        persisted = state.persisted.copy(focus = Focus.Modal),
        runtime = state.runtime.copy(modalStack = state.runtime.modalStack :+ close)
      )
    ).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val modal = UiSceneSnapshot
      .from(before, ViewportSize(80, 24))
      .modal
      .lastOption
      .getOrElse(fail("Expected close confirmation modal"))
    val targetRows = SurfaceFrameLayout.minimumTargetRows(before.persisted.config.interfaceDensity)
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
    after.topModal shouldBe None
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "route a reflowed close action inside a constrained modal frame" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("alpha", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    val viewport = ViewportSize(40, 4)
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    val workflow = CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala")
    val close    = ModalDialog(SurfaceId("close-constrained"), Modal.CloseWorkflow(workflow), ModalPlacement.Centered)
    sm.updateState(state =>
      state.copy(
        persisted = state.persisted.copy(focus = Focus.Modal),
        runtime = state.runtime.copy(modalStack = state.runtime.modalStack :+ close)
      )
    ).unsafeRunSync()

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
        SurfaceFrameLayout.minimumTargetRows(before.persisted.config.interfaceDensity)
      )
      .getOrElse(fail("Expected close confirmation composition"))
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId("close-cancel")))
      .getOrElse(fail("Expected reflowed cancel action"))

    sm.applyEvent(MouseClick(cancel.rect.x.toInt, cancel.rect.y.toInt)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "route find and replace modal hit regions through their reducers" in {
    val findManager = makeStateManager()
    val findBuffer  = findManager.bufferManager.createBuffer("needle\nneedle", None).unsafeRunSync()
    findManager.setBufferForPane(PaneId(0), findBuffer).unsafeRunSync()
    findManager.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val findSurface = UiSurface(
      SurfaceId("find-click"),
      SurfaceContent.ModalWorkflow(Modal.Find("needle", List(FindResult(0, 0), FindResult(1, 0)), 0)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    findManager
      .updateState(state =>
        state.copy(
          persisted = state.persisted.copy(focus = Focus.Surface(findSurface.id)),
          runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces :+ findSurface)
        )
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
    val replaceBuffer  = replaceManager.bufferManager.createBuffer("needle", None).unsafeRunSync()
    replaceManager.setBufferForPane(PaneId(0), replaceBuffer).unsafeRunSync()
    replaceManager.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    val replaceSurface = UiSurface(
      SurfaceId("replace-click"),
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(ReplaceWorkflowState())),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    replaceManager
      .updateState(state =>
        state.copy(
          persisted = state.persisted.copy(focus = Focus.Surface(replaceSurface.id)),
          runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces :+ replaceSurface)
        )
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
        SurfaceFrameLayout.minimumTargetRows(replaceState.persisted.config.interfaceDensity)
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
    val fileBuffer  = fileManager.bufferManager.createBuffer("needle", None).unsafeRunSync()
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
        state.copy(
          persisted = state.persisted.copy(focus = Focus.Surface(fileSurface.id)),
          runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces :+ fileSurface)
        )
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
        SurfaceFrameLayout.minimumTargetRows(fileState.persisted.config.interfaceDensity)
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
    val bufferId = sm.bufferManager.createBuffer("hello\nworld", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(
                document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
                editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 1)))
              )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    val state  = sm.getCurrentState.unsafeRunSync()
    val buffer = state.persisted.buffers(bufferId)
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
    state.persisted.focus shouldBe Focus.Surface(SurfaceId("context-menu"))
  }

  it should "execute the clicked context menu command against the target editor pane" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello\nworld", None).unsafeRunSync()
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
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    after.runtime.clipboard shouldBe Some("hello")
  }

  it should "not select a context menu item when clicking a configured item gap" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello\nworld", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState(state =>
      state
        .copy(persisted = state.persisted.copy(config = state.persisted.config.withCommandRunnerItemGapRows(Some(1))))
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    val openedState = sm.getCurrentState.unsafeRunSync()
    val viewport    = openedState.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
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
    val bufferId = sm.bufferManager.createBuffer("hello\nworld", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    sm.applyEvent(Escape).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.contextMenuSurface shouldBe None
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  private def contextMenuItemPoint(state: AppState, itemIndex: Int): (Int, Int) =
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
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
