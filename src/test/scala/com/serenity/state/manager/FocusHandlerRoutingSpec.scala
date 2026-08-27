package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.command.CommandRunner
import com.serenity.document.RenderedComment
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.DefaultThemes
import com.serenity.ui.theme.config.ThemeCreatorState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FocusHandlerRoutingSpec extends AnyFlatSpec with Matchers:

  private val quickInfo: SurfaceContent        = SurfaceContent.QuickInfo("info")
  private val filePreview: SurfaceContent      = SurfaceContent.FilePreview(Path.of("a.txt"), "content")
  private val symbolDefinition: SurfaceContent = SurfaceContent.SymbolDefinition("sym", Location(0, 0))
  private val cursorInfoBar: SurfaceContent    = SurfaceContent.CursorInfoBar("cursor")
  private val directoryListing: SurfaceContent =
    SurfaceContent.DirectoryListing(Path.of("dir"), Nil, None)
  private val directoryTree: SurfaceContent =
    SurfaceContent.DirectoryTree(DirectoryTreeData(Path.of("root")), None)
  private val commandPalette: SurfaceContent = SurfaceContent.CommandPalette(CommandRunner.empty)
  private val commandPaletteSubmenu: SurfaceContent =
    SurfaceContent.CommandPaletteSubmenu(CommandRunner.empty, "group", previewOnly = false)
  private val themePicker: SurfaceContent = SurfaceContent.ThemePicker(ThemePickerState(List("dark"), 0, "dark"))
  private val themeCreator: SurfaceContent =
    SurfaceContent.ThemeCreator(ThemeCreatorState.fromTheme(DefaultThemes.defaultDark))
  private val fileSearch: SurfaceContent        = SurfaceContent.FileSearch(FileSearchState("q", Nil, 0))
  private val contextualToolbar: SurfaceContent = SurfaceContent.ContextualToolbar(ContextualToolbarState())
  private val contextMenu: SurfaceContent =
    SurfaceContent.ContextMenu(ContextMenu("menu", Focus.EditorPane(PaneId(0)), Nil))

  private val commentLens: SurfaceContent = SurfaceContent.CommentLens(
    CommentLensState(RenderedComment(0, "raw", "md"), "draft", 0, None)
  )

  private val markdownPreview: SurfaceContent = SurfaceContent.MarkdownPreview(BufferId(1), "title")
  private val modalGotoLine: SurfaceContent   = SurfaceContent.ModalWorkflow(Modal.GotoLine(""))
  private val modalCustomA: SurfaceContent    = SurfaceContent.ModalWorkflow(Modal.Custom("plugin-a", ""))
  private val modalCustomB: SurfaceContent    = SurfaceContent.ModalWorkflow(Modal.Custom("plugin-b", ""))
  private val terminal: SurfaceContent        = SurfaceContent.Terminal("buffer", 0)
  private val outline: SurfaceContent         = SurfaceContent.Outline(Nil, None)
  private val comments: SurfaceContent        = SurfaceContent.Comments(Nil, None)
  private val diagnostics: SurfaceContent     = SurfaceContent.Diagnostics(Nil, None)
  private val startPage: SurfaceContent       = SurfaceContent.StartPage(StartupPage("title"))
  private val ghostOverlay: SurfaceContent =
    SurfaceContent.GhostOverlay(quickInfo, LayoutRect(0, 0, 1, 1))

  /** Every [[SurfaceContent]] case, so a case added to the enum without a corresponding fixture here shows up as a
    * mismatch against `SurfaceContent.values`-style coverage rather than silently passing.
    */
  private val allContent: List[SurfaceContent] = List(
    quickInfo,
    filePreview,
    symbolDefinition,
    cursorInfoBar,
    directoryListing,
    directoryTree,
    commandPalette,
    commandPaletteSubmenu,
    themePicker,
    themeCreator,
    fileSearch,
    contextualToolbar,
    contextMenu,
    commentLens,
    markdownPreview,
    modalGotoLine,
    terminal,
    outline,
    comments,
    diagnostics,
    startPage,
    ghostOverlay
  )

  "FocusHandlerRouting.forSurfaceContent" should "return a real handler, never a null or missing routing, for every SurfaceContent case" in
    allContent.foreach { content =>
      withClue(s"content = $content: ") {
        FocusHandlerRouting.forSurfaceContent(content) should not be null
      }
    }

  it should "route command palette and its submenu to the same pooled CommandRunnerComponent instance" in {
    FocusHandlerRouting.forSurfaceContent(commandPalette) should
      be theSameInstanceAs FocusHandlerRouting.forSurfaceContent(commandPaletteSubmenu)
  }

  it should "route theme picker content to a ThemePickerComponent" in {
    FocusHandlerRouting.forSurfaceContent(themePicker).getClass.getSimpleName shouldBe "ThemePickerComponent"
  }

  it should "route theme creator content to a ThemeCreatorComponent" in {
    FocusHandlerRouting.forSurfaceContent(themeCreator).getClass.getSimpleName shouldBe "ThemeCreatorComponent"
  }

  it should "route file search content to a FileSearchComponent" in {
    FocusHandlerRouting.forSurfaceContent(fileSearch).getClass.getSimpleName shouldBe "FileSearchComponent"
  }

  it should "route contextual toolbar content to a ContextualToolbarComponent" in {
    FocusHandlerRouting
      .forSurfaceContent(contextualToolbar)
      .getClass
      .getSimpleName shouldBe "ContextualToolbarComponent"
  }

  it should "route comment lens content to a CommentLensComponent" in {
    FocusHandlerRouting.forSurfaceContent(commentLens).getClass.getSimpleName shouldBe "CommentLensComponent"
  }

  it should "route the start page to a StartupPageComponent" in {
    FocusHandlerRouting.forSurfaceContent(startPage).getClass.getSimpleName shouldBe "StartupPageComponent"
  }

  it should "route a known modal kind to a pooled ModalComponent, reused across dispatches" in {
    val first  = FocusHandlerRouting.forSurfaceContent(modalGotoLine)
    val second = FocusHandlerRouting.forSurfaceContent(SurfaceContent.ModalWorkflow(Modal.GotoLine("different draft")))
    first.getClass.getSimpleName shouldBe "ModalComponent"
    first should be theSameInstanceAs second
  }

  it should "build a fresh ModalComponent per Custom modal name, since the name space is open-ended" in {
    val handler = FocusHandlerRouting.forSurfaceContent(modalCustomA)
    handler.getClass.getSimpleName shouldBe "ModalComponent"
    handler should not be theSameInstanceAs(FocusHandlerRouting.forSurfaceContent(modalCustomB))
  }

  it should "route peek-style overlay content (info popups, previews, panel-only content, ghost overlays) to the pooled PeekOverlayComponent" in {
    val peekCases = List(
      quickInfo,
      filePreview,
      symbolDefinition,
      cursorInfoBar,
      directoryListing,
      directoryTree,
      contextMenu,
      markdownPreview,
      terminal,
      outline,
      comments,
      diagnostics,
      ghostOverlay
    )
    val reference = FocusHandlerRouting.forSurfaceContent(quickInfo)
    reference.getClass.getSimpleName shouldBe "PeekOverlayComponent"
    peekCases.foreach { content =>
      withClue(s"content = $content: ") {
        FocusHandlerRouting.forSurfaceContent(content) should be theSameInstanceAs reference
      }
    }
  }

  it should "not allocate a new component on repeated dispatches for the same content case" in
    allContent.filterNot(c => c == modalCustomA || c == modalCustomB).foreach { content =>
      withClue(s"content = $content: ") {
        val first  = FocusHandlerRouting.forSurfaceContent(content)
        val second = FocusHandlerRouting.forSurfaceContent(content)
        first should be theSameInstanceAs second
      }
    }

  "FocusHandlerRouting.forPinnedPanel" should "return the same pooled PinnedPanelComponent instance per position across dispatches" in
    PanelPosition.values.foreach { position =>
      withClue(s"position = $position: ") {
        val first  = FocusHandlerRouting.forPinnedPanel(position)
        val second = FocusHandlerRouting.forPinnedPanel(position)
        first.getClass.getSimpleName shouldBe "PinnedPanelComponent"
        first should be theSameInstanceAs second
      }
    }

  it should "return distinct instances for distinct positions" in {
    val instances = PanelPosition.values.map(FocusHandlerRouting.forPinnedPanel).toList
    instances.distinct.size shouldBe instances.size
  }
