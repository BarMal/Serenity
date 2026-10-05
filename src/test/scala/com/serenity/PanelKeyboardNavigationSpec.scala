package com.serenity

import java.nio.file.Paths

import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.keystroke.translators.PinnedPanelTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, PinnedPanelComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{PinnedPanelViewModel, TextPanelRow}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every list panel answers the same keys: arrows move its highlight, Home/End and PageUp/PageDown jump, and Enter
  * opens what is highlighted. Project output scrolls instead, showing the newest output until scrolled back.
  */
class PanelKeyboardNavigationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId    = PaneId(0)
  private val surfaceId = SurfaceId("left-panel")
  private val component = PinnedPanelComponent(PanelPosition.Left)

  private def baseState: AppState =
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, (0 until 40).map(line => s"line $line").mkString("\n"))
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(100, 30)))
    )

  private def docked(content: SurfaceContent): AppState =
    val state = DockedPanelFixtures.dock(baseState, surfaceId, content, PanelPosition.Left, 30)
    state.copy(persisted = state.persisted.copy(focus = Focus.Surface(surfaceId)))

  private def after(event: PanelInputEvent, state: AppState): AppState =
    component.processEvent(event, state) match
      case ComponentResult.StateChange(update) => update(state)
      case ComponentResult.NoChange            => state
      case other                               => fail(s"Expected a state change, got $other")

  private def content(state: AppState): Option[SurfaceContent] = state.surfaceById(surfaceId).map(_.content)

  private def highlighted(state: AppState): Option[Location] =
    content(state).flatMap {
      case SurfaceContent.Outline(_, location, _)     => location
      case SurfaceContent.Comments(_, location, _)    => location
      case SurfaceContent.Diagnostics(_, location, _) => location
      case _                                          => None
    }

  private val issues =
    (0 until 30).toList.map(line => Diagnostic(s"issue $line", DiagnosticSeverity.Warning, Location(line, 0)))

  private val headings =
    List(0, 10, 20, 30).map(line => Symbol(s"Heading $line", SymbolKind.Heading, Location(line, 0)))

  "A list panel" should "highlight its first row on Down when nothing is highlighted, then step through the rest" in {
    val first = after(PanelInputEvent.Navigate(Direction.Down), docked(SurfaceContent.Diagnostics(issues)))
    highlighted(first) shouldBe Some(Location(0, 0))

    highlighted(after(PanelInputEvent.Navigate(Direction.Down), first)) shouldBe Some(Location(1, 0))
  }

  it should "stop at either end rather than wrapping" in {
    val atEnd = docked(SurfaceContent.Diagnostics(issues, Some(Location(29, 0))))

    highlighted(after(PanelInputEvent.Navigate(Direction.Down), atEnd)) shouldBe Some(Location(29, 0))
  }

  it should "jump to the first and last rows with Home and End" in {
    val middle = docked(SurfaceContent.Diagnostics(issues, Some(Location(12, 0))))

    highlighted(after(PanelInputEvent.First, middle)) shouldBe Some(Location(0, 0))
    highlighted(after(PanelInputEvent.Last, middle)) shouldBe Some(Location(29, 0))
  }

  it should "move a page at a time with PageUp and PageDown, stopping at the ends" in {
    val top  = docked(SurfaceContent.Diagnostics(issues, Some(Location(0, 0))))
    val down = highlighted(after(PanelInputEvent.Page(1), top)).getOrElse(fail("expected a highlight"))

    down.line should be > 1
    highlighted(after(PanelInputEvent.Page(-1), after(PanelInputEvent.Page(1), top))) shouldBe Some(Location(0, 0))
    highlighted(
      after(PanelInputEvent.Page(1), docked(SurfaceContent.Diagnostics(issues, Some(Location(28, 0)))))
    ) shouldBe
      Some(Location(29, 0))
  }

  it should "start an outline from the section the cursor is in" in {
    val inSecondSection = docked(SurfaceContent.Outline(headings))
    val withCursor = inSecondSection.copy(persisted =
      inSecondSection.persisted.copy(buffers = inSecondSection.persisted.buffers.view.mapValues { buffer =>
        buffer.copy(editing = EditingState(List(CursorPosition(12, 0))))
      }.toMap)
    )

    highlighted(after(PanelInputEvent.Navigate(Direction.Down), withCursor)) shouldBe Some(Location(20, 0))
  }

  it should "open the highlighted row in the editor on Enter, moving focus there" in {
    val state = docked(SurfaceContent.Diagnostics(issues, Some(Location(17, 0))))

    val opened = after(PanelInputEvent.Activate, state)

    opened.persisted.focus shouldBe Focus.EditorPane(paneId)
    opened.activeCursorPosition shouldBe Some(CursorPosition(17, 0))
  }

  it should "move through the comments panel the same way" in {
    val comments = headings.map(_.copy(kind = SymbolKind.Comment))
    val state    = docked(SurfaceContent.Comments(comments, Some(Location(10, 0))))

    highlighted(after(PanelInputEvent.Navigate(Direction.Up), state)) shouldBe Some(Location(0, 0))
    after(PanelInputEvent.Activate, state).activeCursorPosition shouldBe Some(CursorPosition(10, 0))
  }

  "The explorer" should "jump to its root and last rows with Home and End" in {
    val root = Paths.get("/repo")
    val tree = DirectoryTreeData(
      root,
      entries = Map(
        root -> List(
          DirEntry(root.resolve("a"), "a", isDirectory = false),
          DirEntry(root.resolve("b"), "b", isDirectory = false),
          DirEntry(root.resolve("c"), "c", isDirectory = false)
        )
      )
    )
    val state = docked(SurfaceContent.DirectoryTree(tree, Some(root.resolve("b"))))

    content(after(PanelInputEvent.Last, state)) shouldBe Some(
      SurfaceContent.DirectoryTree(tree, Some(root.resolve("c")))
    )
    content(after(PanelInputEvent.First, state)) shouldBe Some(SurfaceContent.DirectoryTree(tree, Some(root)))
  }

  private val explorerRoot  = Paths.get("/repo")
  private val explorerFiles = (0 until 60).toList.map(index => explorerRoot.resolve(f"file-$index%02d"))

  private val tallTree = DirectoryTreeData(
    explorerRoot,
    entries = Map(
      explorerRoot -> explorerFiles.map(path => DirEntry(path, path.getFileName.toString, isDirectory = false))
    )
  )

  private def explorerRows(state: AppState): List[TextPanelRow] =
    val viewport = state.runtime.viewportSize.getOrElse(fail("expected a viewport"))
    PinnedPanelViewModel
      .fromState(state, LayoutEngine.calculateLayoutWithUI(state, viewport))
      .find(_.surfaceId.contains(surfaceId))
      .map(_.rows)
      .getOrElse(fail("expected the explorer to be painted"))

  private def selectedRowText(state: AppState): List[String] =
    explorerRows(state).filter(_.selected).map(_.plainText.trim)

  it should "scroll to keep the selection in view when it moves past the last visible row" in {
    val shown = explorerRows(docked(SurfaceContent.DirectoryTree(tallTree, Some(explorerRoot)))).size
    shown should be < explorerFiles.size
    // Row 0 is the root, so the last row that fits shows explorerFiles(shown - 2).
    val atFold = docked(SurfaceContent.DirectoryTree(tallTree, Some(explorerFiles(shown - 2))))

    val scrolled = after(PanelInputEvent.Navigate(Direction.Down), atFold)

    selectedRowText(scrolled) shouldBe List(f"file-${shown - 1}%02d")
    explorerRows(scrolled).size shouldBe shown
  }

  it should "keep the scroll position while the selection moves within the rows already shown" in {
    val shown    = explorerRows(docked(SurfaceContent.DirectoryTree(tallTree, Some(explorerRoot)))).size
    val atFold   = docked(SurfaceContent.DirectoryTree(tallTree, Some(explorerFiles(shown - 2))))
    val scrolled = after(PanelInputEvent.Navigate(Direction.Down), atFold)

    val backUp = after(PanelInputEvent.Navigate(Direction.Up), scrolled)

    explorerRows(backUp).map(_.plainText) shouldBe explorerRows(scrolled).map(_.plainText)
    selectedRowText(backUp) shouldBe List(f"file-${shown - 2}%02d")
  }

  it should "show the last row once End selects it" in {
    val atEnd = after(PanelInputEvent.Last, docked(SurfaceContent.DirectoryTree(tallTree, Some(explorerRoot))))

    selectedRowText(atEnd) shouldBe List("file-59")
    explorerRows(atEnd).lastOption.map(_.plainText.trim) shouldBe Some("file-59")
  }

  "Project output" should "scroll back a line at a time, and return to following new output with End" in {
    val text      = (0 until 50).map(line => s"out $line").mkString("\n")
    val following = docked(SurfaceContent.Terminal(text, text.length))

    val scrolled = after(PanelInputEvent.Navigate(Direction.Up), following)
    content(scrolled) should not be Some(SurfaceContent.Terminal(text, text.length))
    content(after(PanelInputEvent.Last, scrolled)) shouldBe Some(SurfaceContent.Terminal(text, text.length))
  }

  "The panel keymap" should "send Home, End, PageUp and PageDown to the focused panel" in {
    val translator           = new PinnedPanelTranslator()
    def key(input: InputKey) = translator.translate(KeyStrokeInfo(input, None, Set.empty))

    key(InputKey.Home) shouldBe PanelInputEvent.First
    key(InputKey.End) shouldBe PanelInputEvent.Last
    key(InputKey.PageUp) shouldBe PanelInputEvent.Page(-1)
    key(InputKey.PageDown) shouldBe PanelInputEvent.Page(1)
  }
