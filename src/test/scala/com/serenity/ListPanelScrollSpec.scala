package com.serenity

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Direction, MouseClick, MouseWheel, PanelInputEvent, ResizeEvent}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, PinnedPanelComponent}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{PinnedPanelViewModel, TextPanelRow}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** List panels -- explorer, outline, comments, diagnostics -- keep a scroll position of their own (#1953, #1955): the
  * keyboard scrolls only as far as it must to keep the highlight in view, the wheel over a panel scrolls that panel and
  * leaves the highlight where it is, and the position survives the panel's content being refreshed.
  */
class ListPanelScrollSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport  = ViewportSize(100, 30)
  private val paneId    = PaneId(0)
  private val bufferId  = BufferId(1)
  private val surfaceId = SurfaceId("left-panel")
  private val component = PinnedPanelComponent(PanelPosition.Left)

  private val issues =
    (0 until 60).toList.map(line => Diagnostic(s"issue $line", DiagnosticSeverity.Warning, Location(line, 0)))

  private val headings =
    (0 until 60).toList.map(line => Symbol(s"Heading $line", SymbolKind.Heading, Location(line * 5, 0)))

  private def editorState: AppState =
    val buffer = Buffer.fromString(bufferId, (0 until 400).map(line => s"line $line").mkString("\n"))
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime =
        AppState.initial.runtime.copy(viewportSize = Some(viewport), nextBufferId = BufferId(bufferId.value + 1))
    )

  private def docked(content: SurfaceContent, state: AppState = editorState): AppState =
    val dockedState = DockedPanelFixtures.dock(state, surfaceId, content, PanelPosition.Left, 30)
    dockedState.copy(persisted = dockedState.persisted.copy(focus = Focus.Surface(surfaceId)))

  private def after(event: PanelInputEvent, state: AppState): AppState =
    component.processEvent(event, state) match
      case ComponentResult.StateChange(update) => update(state)
      case ComponentResult.NoChange            => state
      case other                               => fail(s"Expected a state change, got $other")

  private def pressed(event: PanelInputEvent, times: Int, state: AppState): AppState =
    (1 to times).foldLeft(state)((current, _) => after(event, current))

  private def painted(state: AppState): List[TextPanelRow] =
    PinnedPanelViewModel
      .fromState(state, LayoutEngine.calculateLayoutWithUI(state, viewport))
      .find(_.surfaceId.contains(surfaceId))
      .map(_.rows)
      .getOrElse(fail("expected the panel to be painted"))

  private def paintedText(state: AppState): List[String] = painted(state).map(_.plainText.trim.stripPrefix("> "))

  private def issueRows(state: AppState): List[String] = paintedText(state).filter(_.startsWith("issue"))

  private def highlightedText(state: AppState): List[String] =
    painted(state).filter(_.selected).map(_.plainText.trim.stripPrefix("> "))

  private def highlight(state: AppState): Option[Location] =
    state.surfaceById(surfaceId).map(_.content).flatMap {
      case outline: SurfaceContent.Outline         => outline.activeLocation
      case diagnostics: SurfaceContent.Diagnostics => diagnostics.activeLocation
      case _                                       => None
    }

  "A diagnostics panel" should "keep its rows still when the highlight moves back up after scrolling down" in {
    val top      = docked(SurfaceContent.Diagnostics(issues, Some(Location(0, 0))))
    val shown    = painted(top).size
    val pastFold = pressed(PanelInputEvent.Navigate(Direction.Down), shown, top)

    val backUp = after(PanelInputEvent.Navigate(Direction.Up), pastFold)

    paintedText(backUp) shouldBe paintedText(pastFold)
    highlightedText(backUp) shouldBe List(s"issue ${shown - 1}")
  }

  "An outline" should "keep its rows still when the highlight moves back up after scrolling down" in {
    val top      = docked(SurfaceContent.Outline(headings, Some(Location(0, 0))))
    val shown    = painted(top).size
    val pastFold = pressed(PanelInputEvent.Navigate(Direction.Down), shown, top)

    val backUp = pressed(PanelInputEvent.Navigate(Direction.Up), 2, pastFold)

    paintedText(backUp) shouldBe paintedText(pastFold)
    highlight(backUp) shouldBe Some(Location((shown - 2) * 5, 0))
  }

  "A list panel" should "move a page with PageDown and show the last row after End" in {
    val top   = docked(SurfaceContent.Diagnostics(issues, Some(Location(0, 0))))
    val shown = painted(top).size

    val paged = after(PanelInputEvent.Page(1), top)
    highlightedText(paged) shouldBe List(s"issue ${shown - 2}")

    val atEnd = after(PanelInputEvent.Last, paged)
    issueRows(atEnd).lastOption shouldBe Some("issue 59")
    highlightedText(atEnd) shouldBe List("issue 59")

    highlightedText(after(PanelInputEvent.First, atEnd)) shouldBe List("issue 0")
  }

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("ListPanelScrollSpec"))
    StateManager(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()

  private def managerWith(content: SurfaceContent): StateManager =
    val sm = makeStateManager()
    sm.updateState(_ => editorState).unsafeRunSync()
    sm.updateState { state =>
      val withPanel = DockedPanelFixtures.dock(state, surfaceId, content, PanelPosition.Left, 30)
      withPanel.copy(persisted = withPanel.persisted.copy(focus = Focus.EditorPane(paneId)))
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    sm

  private def contract(state: AppState): EditorLayoutContract =
    EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))

  private def panelPoint(state: AppState): (Int, Int) =
    val frame = contract(state).panelRect(surfaceId).getOrElse(fail("expected the panel to be laid out"))
    (frame.x + frame.width / 2, frame.y + frame.height / 2)

  private def editorPoint(state: AppState): (Int, Int) =
    val pane = contract(state).workspace.paneLayouts.getOrElse(paneId, fail("expected the editor pane"))
    (pane.contentRect.x + pane.contentRect.width / 2, pane.contentRect.y + pane.contentRect.height / 2)

  private def wheel(sm: StateManager, at: AppState => (Int, Int), lines: Int): AppState =
    val (col, row) = at(sm.getCurrentState.unsafeRunSync())
    sm.applyEvent(MouseWheel(col, row, lines)).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync()

  private def editorTopLine(state: AppState): Int =
    state.persisted.buffers.get(bufferId).map(_.viewport.topLine).getOrElse(fail("expected the buffer"))

  "The wheel over a docked list panel" should "scroll the panel, leaving its highlight and the editor alone" in {
    val sm     = managerWith(SurfaceContent.Diagnostics(issues, Some(Location(0, 0))))
    val before = sm.getCurrentState.unsafeRunSync()

    val scrolled = wheel(sm, panelPoint, 3)

    issueRows(scrolled).headOption shouldBe Some("issue 3")
    highlight(scrolled) shouldBe Some(Location(0, 0))
    highlightedText(scrolled) shouldBe Nil
    editorTopLine(scrolled) shouldBe editorTopLine(before)
  }

  it should "stop at the top and at the last row" in {
    val sm = managerWith(SurfaceContent.Diagnostics(issues, Some(Location(0, 0))))

    issueRows(wheel(sm, panelPoint, -3)).headOption shouldBe Some("issue 0")
    issueRows(wheel(sm, panelPoint, 500)).lastOption shouldBe Some("issue 59")
  }

  it should "bring the highlight back into view at the next keyboard move" in {
    val sm       = managerWith(SurfaceContent.Diagnostics(issues, Some(Location(0, 0))))
    val scrolled = wheel(sm, panelPoint, 40)
    highlightedText(scrolled) shouldBe Nil

    val moved = after(PanelInputEvent.Navigate(Direction.Down), scrolled)

    highlightedText(moved) shouldBe List("issue 1")
  }

  "The wheel over the editor" should "still scroll the editor when a panel is docked beside it" in {
    val sm     = managerWith(SurfaceContent.Diagnostics(issues, Some(Location(0, 0))))
    val before = sm.getCurrentState.unsafeRunSync()

    val scrolled = wheel(sm, editorPoint, 3)

    editorTopLine(scrolled) shouldBe editorTopLine(before) + 3
    issueRows(scrolled).headOption shouldBe Some("issue 0")
  }

  private val explorerRoot  = Paths.get("/repo")
  private val explorerFiles = (0 until 80).toList.map(index => explorerRoot.resolve(f"file-$index%02d"))

  private val tallTree = DirectoryTreeData(
    explorerRoot,
    entries = Map(
      explorerRoot -> explorerFiles.map(path => DirEntry(path, path.getFileName.toString, isDirectory = false))
    )
  )

  "The wheel over the explorer" should "reach the rows below the first screen, which a click then selects" in {
    val sm       = managerWith(SurfaceContent.DirectoryTree(tallTree, Some(explorerRoot)))
    val scrolled = wheel(sm, panelPoint, 500)

    paintedText(scrolled).lastOption shouldBe Some("file-79")
    highlightedText(scrolled) shouldBe Nil

    val lastRow = contract(scrolled)
      .panelRowSlots(surfaceId)
      .collect { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), y) => y }
      .lastOption
      .getOrElse(fail("expected painted rows"))
    val content = contract(scrolled).panelContentRect(surfaceId).getOrElse(fail("expected a content rect"))
    sm.applyEvent(MouseClick(content.x + 1, lastRow)).unsafeRunSync()

    val clicked = sm.getCurrentState.unsafeRunSync()
    clicked.surfaceById(surfaceId).map(_.content).collect {
      case tree: SurfaceContent.DirectoryTree =>
        tree.selectedPath
    } shouldBe Some(Some(explorerFiles.last))
    paintedText(clicked) shouldBe paintedText(scrolled)
  }
