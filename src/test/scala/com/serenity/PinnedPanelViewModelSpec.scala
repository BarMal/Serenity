package com.serenity

import java.nio.file.Paths

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{PinnedPanelViewModel, TextPanelRow, TextPanelView}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelViewModelSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val root = Paths.get("/repo")

  private val tree = DirectoryTreeData(
    rootPath = root,
    expandedPaths = Set(root, root.resolve("src")),
    entries = Map(
      root -> List(
        DirEntry(root.resolve("src"), "src", isDirectory = true),
        DirEntry(root.resolve("test"), "test", isDirectory = true),
        DirEntry(root.resolve("build.sbt"), "build.sbt", isDirectory = false),
        DirEntry(root.resolve("project"), "project", isDirectory = true)
      ),
      root.resolve("src") -> List(
        DirEntry(root.resolve("src").resolve("main"), "main", isDirectory = true),
        DirEntry(root.resolve("src").resolve("Serenity.scala"), "Serenity.scala", isDirectory = false)
      )
    )
  )

  private val panel = UiSurface(
    id = SurfaceId("directory-tree"),
    content = SurfaceContent.DirectoryTree(
      tree,
      Some(root.resolve("src"))
    ),
    presentation = SurfacePresentation.Pinned(PanelPosition.Left, 24)
  )

  private val terminalPanel = UiSurface(
    id = SurfaceId("terminal"),
    content = SurfaceContent.Terminal("sbt test\ncompile\nrun", cursor = 7),
    presentation = SurfacePresentation.Pinned(PanelPosition.Bottom, 10)
  )

  private val outlineSymbols = List(
    Symbol("Serenity", SymbolKind.Class, Location(1, 1)),
    Symbol("render", SymbolKind.Method, Location(10, 3)),
    Symbol("state", SymbolKind.Variable, Location(20, 5)),
    Symbol("Chapter 1", SymbolKind.Heading, Location(30, 0))
  )

  private val outlinePanel = UiSurface(
    id = SurfaceId("outline"),
    content = SurfaceContent.Outline(outlineSymbols),
    presentation = SurfacePresentation.Pinned(PanelPosition.Right, 20)
  )

  private val diagnosticsPanel = UiSurface(
    id = SurfaceId("diagnostics"),
    content = SurfaceContent.Diagnostics(
      List(
        Diagnostic("Unused import", DiagnosticSeverity.Warning, Location(2, 1)),
        Diagnostic("Type mismatch", DiagnosticSeverity.Error, Location(8, 4)),
        Diagnostic("Can inline", DiagnosticSeverity.Info, Location(12, 2))
      )
    ),
    presentation = SurfacePresentation.Pinned(PanelPosition.Bottom, 12)
  )

  "PinnedPanelViewModel.resolve" should "shape directory trees for wide panel geometry" in {
    val view = PinnedPanelViewModel.resolve(panel, LayoutRect(0, 0, 60, 10))

    view.title shouldBe "repo"
    view.rows.map(_.plainText) shouldBe List(
      "▾ repo",
      "  ▾ src",
      "    ▹ main",
      "    Serenity.scala",
      "  ▹ test",
      "  build.sbt",
      "  ▹ project"
    )
    view.rows.count(_.selected) shouldBe 1
    view.rows.find(_.selected).map(_.plainText) shouldBe Some("  ▾ src")
  }

  it should "shape directory trees for tall panel geometry" in {
    val view = PinnedPanelViewModel.resolve(panel, LayoutRect(0, 0, 18, 40))

    view.title shouldBe "repo"
    view.rows.map(_.plainText) shouldBe List(
      "▾ repo",
      "  ▾ src",
      "    ▹ main",
      "    Serenity.scala",
      "  ▹ test",
      "  build.sbt",
      "  ▹ project"
    )
  }

  it should "shape directory trees for square panel geometry" in {
    val view = PinnedPanelViewModel.resolve(panel, LayoutRect(0, 0, 24, 20))

    view.title shouldBe "repo"
    view.rows.map(_.plainText) shouldBe List(
      "▾ repo",
      "  ▾ src",
      "    ▹ main",
      "    Serenity.scala",
      "  ▹ test",
      "  build.sbt",
      "  ▹ project"
    )
  }

  it should "shape directory trees for compact panel geometry" in {
    val view = PinnedPanelViewModel.resolve(panel, LayoutRect(0, 0, 14, 4))

    view.title shouldBe "repo"
    view.rows.map(_.plainText) shouldBe List("▾ repo", "  ▾ src")
  }

  it should "shape terminal content differently for wide and compact geometry" in {
    val wide    = PinnedPanelViewModel.resolve(terminalPanel, LayoutRect(0, 0, 60, 8))
    val compact = PinnedPanelViewModel.resolve(terminalPanel, LayoutRect(0, 0, 14, 4))

    wide.title shouldBe "terminal"
    wide.rows.map(_.plainText) shouldBe List("sbt test", "compile", "run")

    compact.title shouldBe "terminal"
    compact.rows.map(_.plainText) shouldBe List("3 lines", "cursor 7")
  }

  it should "preserve resolved header and footer rows separately from item rows" in {
    val modalPanel = UiSurface(
      id = SurfaceId("find-panel"),
      content = SurfaceContent.ModalWorkflow(
        Modal.Find("needle", List(FindResult(2, 4)), 0)
      ),
      presentation = SurfacePresentation.Pinned(PanelPosition.Right, 24)
    )

    val view = PinnedPanelViewModel.resolve(modalPanel, LayoutRect(0, 0, 40, 8))

    view.title shouldBe "find"
    view.header.map(_.plainText) shouldBe Some("find")
    view.rows.map(_.plainText) shouldBe List("Find needle", "1. 3:5")
    view.footer.map(_.plainText) shouldBe Some("1 match, 1/1 at 3:5")
  }

  it should "capture the resolved content rectangle from the pinned surface content" in {
    val rect = LayoutRect(0, 0, 24, 8)

    val view = PinnedPanelViewModel.resolve(panel, rect)

    view.contentRect.shouldBe(Some(SurfaceFrameLayout.forContent(rect, panel.content).contentRect))
  }

  it should "derive row slots from an explicit pinned panel content rect" in {
    val view = TextPanelView(
      rect = LayoutRect(0, 0, 12, 8),
      contentRect = Some(LayoutRect(2, 3, 6, 4)),
      title = "panel",
      rows = List(TextPanelRow("A"), TextPanelRow("B"), TextPanelRow("C")),
      header = Some(TextPanelRow("head")),
      footer = Some(TextPanelRow("foot"))
    )

    view.contentRowSlots
      .map(slot => slot.kind -> slot.y)
      .shouldBe(
        List(
          SurfaceContentRowKind.Header  -> 3,
          SurfaceContentRowKind.Item(0) -> 4,
          SurfaceContentRowKind.Item(1) -> 5,
          SurfaceContentRowKind.Footer  -> 6
        )
      )
  }

  it should "shape outline content differently for tall and wide geometry" in {
    val tall = PinnedPanelViewModel.resolve(outlinePanel, LayoutRect(0, 0, 18, 40))
    val wide = PinnedPanelViewModel.resolve(outlinePanel, LayoutRect(0, 0, 60, 10))

    tall.title shouldBe "outline"
    tall.rows.map(_.plainText) shouldBe List(
      "Class Serenity",
      "Method render",
      "Variable state",
      "Heading Chapter 1"
    )

    wide.title shouldBe "outline"
    wide.rows.map(_.plainText) shouldBe List("Serenity | render | state | Chapter 1")
  }

  it should "mark the active outline symbol in tall and wide panel geometry" in {
    val activePanel = outlinePanel.copy(content = SurfaceContent.Outline(outlineSymbols, Some(Location(10, 3))))
    val tall        = PinnedPanelViewModel.resolve(activePanel, LayoutRect(0, 0, 18, 40))
    val wide        = PinnedPanelViewModel.resolve(activePanel, LayoutRect(0, 0, 60, 10))

    tall.rows.map(_.plainText) shouldBe List(
      "Class Serenity",
      "> Method render",
      "Variable state",
      "Heading Chapter 1"
    )
    tall.rows.map(_.selected) shouldBe List(false, true, false, false)
    wide.rows.map(_.plainText) shouldBe List("Serenity | [render] | state | Chapter 1")
    wide.rows.map(_.selected) shouldBe List(true)
  }

  it should "derive the active outline symbol from the editor cursor when state is available" in {
    val initialState = AppState.initial
    val state = initialState.copy(
      buffers = initialState.buffers.updated(
        BufferId(0),
        initialState.buffers(BufferId(0)).copy(cursors = List(CursorPosition(12, 1)))
      )
    )

    val view = PinnedPanelViewModel.resolve(outlinePanel, LayoutRect(0, 0, 18, 40), state)

    view.rows.map(_.plainText) shouldBe List(
      "Class Serenity",
      "> Method render",
      "Variable state",
      "Heading Chapter 1"
    )
  }

  it should "prefer an explicit outline active location when state is available" in {
    val initialState = AppState.initial
    val state = initialState.copy(
      buffers = initialState.buffers.updated(
        BufferId(0),
        initialState.buffers(BufferId(0)).copy(cursors = List(CursorPosition(12, 1)))
      )
    )
    val hoveredPanel = outlinePanel.copy(content = SurfaceContent.Outline(outlineSymbols, Some(Location(30, 0))))

    val view = PinnedPanelViewModel.resolve(hoveredPanel, LayoutRect(0, 0, 18, 40), state)

    view.rows.map(_.plainText) shouldBe List(
      "Class Serenity",
      "Method render",
      "Variable state",
      "> Heading Chapter 1"
    )
  }

  it should "shape diagnostics content differently for wide and compact geometry" in {
    val wide    = PinnedPanelViewModel.resolve(diagnosticsPanel, LayoutRect(0, 0, 60, 10))
    val compact = PinnedPanelViewModel.resolve(diagnosticsPanel, LayoutRect(0, 0, 14, 4))

    wide.title shouldBe "diagnostics"
    wide.rows.map(_.plainText) shouldBe List("1 error | 1 warning | 1 info")

    compact.title shouldBe "diagnostics"
    compact.rows.map(_.plainText) shouldBe List("3 issues", "1 error")
  }
end PinnedPanelViewModelSpec
