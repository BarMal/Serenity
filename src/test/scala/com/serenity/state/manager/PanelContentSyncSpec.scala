package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.DockedPanelFixtures
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic as LspDiagnostic, DiagnosticSeverity as LspSeverity, LspPosition, LspRange}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.{
  Diagnostic,
  DiagnosticSeverity,
  DirEntry,
  DirectoryTreeData,
  Location,
  PanelPosition,
  Symbol,
  SymbolKind
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Docked panels whose content derives from the active document stay in step with it on every commit. */
class PanelContentSyncSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val panelId = SurfaceId("panel")

  private def shownInActivePane(state: AppState, text: String, name: String): (AppState, BufferId) =
    val creation = EditorTransitions.bufferCreated(state, text, Some(Paths.get(s"/docs/$name")))
    val created  = creation.created
    val bufferId = creation.bufferId
    val markdown = created.persisted.buffers(bufferId)
    val paneId   = created.persisted.layout.activeEditorPaneId.getOrElse(fail("no active pane"))
    val pane     = created.persisted.layout.editorPanes(paneId)
    val shown = created.copy(persisted =
      created.persisted.copy(
        buffers = created.persisted.buffers +
          (bufferId -> markdown.copy(document = markdown.document.copy(language = Some(LanguageId.Markdown)))),
        layout = created.persisted.layout.copy(
          editorPanes = created.persisted.layout.editorPanes + (paneId -> pane.copy(bufferId = Some(bufferId)))
        )
      )
    )
    (shown, bufferId)

  private def docked(state: AppState, content: SurfaceContent): AppState =
    DockedPanelFixtures.dock(state, panelId, content, PanelPosition.Left, 30)

  private def edited(state: AppState, bufferId: BufferId, text: String): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers + (bufferId -> buffer.copy(document = buffer.document.withContent(Rope(text))))
      )
    )

  private def panelContent(state: AppState): SurfaceContent =
    state.surfaceById(panelId).map(_.content).getOrElse(fail("panel gone"))

  private def heading(name: String, line: Int): Symbol = Symbol(name, SymbolKind.Heading, Location(line, 0))

  "A docked outline" should "ask for a debounced re-parse after an edit, rather than re-parsing on the keystroke" in {
    val (withDoc, bufferId) = shownInActivePane(AppState.initial, "# One\n", "a.md")
    val before              = docked(withDoc, SurfaceContent.Outline(List(heading("One", 0))))
    val after               = edited(before, bufferId, "# One\n\n# Two\n")

    PanelContentSync.synced(after, before) shouldBe after
    PanelContentSync.outlineRefreshDue(after, before) shouldBe Some(bufferId)
  }

  it should "take a re-parse of the active document's current version" in {
    val (withDoc, bufferId) = shownInActivePane(AppState.initial, "# One\n", "a.md")
    val before              = docked(withDoc, SurfaceContent.Outline(List(heading("One", 0))))
    val after               = edited(before, bufferId, "# One\n\n# Two\n")
    val version             = after.persisted.buffers(bufferId).document.contentVersion
    val symbols             = PanelSymbolLookup.outlineSymbolsForBuffer(after.persisted.buffers(bufferId))

    panelContent(PanelContentSync.withRefreshedOutline(after, bufferId, version, symbols)) shouldBe
      SurfaceContent.Outline(List(heading("One", 0), heading("Two", 2)), None)
  }

  it should "drop a re-parse overtaken by a later edit" in {
    val (withDoc, bufferId) = shownInActivePane(AppState.initial, "# One\n", "a.md")
    val before              = docked(withDoc, SurfaceContent.Outline(List(heading("One", 0))))
    val parsed              = edited(before, bufferId, "# One\n\n# Two\n")
    val version             = parsed.persisted.buffers(bufferId).document.contentVersion
    val later               = edited(parsed, bufferId, "# One\n")

    PanelContentSync.withRefreshedOutline(later, bufferId, version, List(heading("stale", 0))) shouldBe later
  }

  it should "need no re-parse when no outline is docked" in {
    val (before, bufferId) = shownInActivePane(AppState.initial, "# One\n", "a.md")

    PanelContentSync.outlineRefreshDue(edited(before, bufferId, "# Two\n"), before) shouldBe None
  }

  it should "follow a switch to another document, dropping a selection made in the old one" in {
    val (first, _)    = shownInActivePane(AppState.initial, "# First\n", "a.md")
    val before        = docked(first, SurfaceContent.Outline(List(heading("First", 0)), Some(Location(0, 0))))
    val (switched, _) = shownInActivePane(before, "# Second\n", "b.md")

    panelContent(PanelContentSync.synced(switched, before)) shouldBe
      SurfaceContent.Outline(List(heading("Second", 0)), None)
  }

  it should "be left alone when nothing it derives from changed" in {
    val (withDoc, _) = shownInActivePane(AppState.initial, "# One\n", "a.md")
    val before       = docked(withDoc, SurfaceContent.Outline(List(heading("One", 0)), Some(Location(0, 0))))
    val focusMoved   = before.copy(persisted = before.persisted.copy(focus = Focus.Surface(panelId)))

    PanelContentSync.synced(focusMoved, before) shouldBe focusMoved
  }

  "A docked comments panel" should "follow comments added to the active document" in {
    val (withDoc, bufferId) = shownInActivePane(AppState.initial, "some prose\n", "a.md")
    val before              = docked(withDoc, SurfaceContent.Comments(Nil))
    val buffer              = before.persisted.buffers(bufferId)
    val comment             = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 4), "tighten")
    val commented = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers +
          (bufferId -> buffer.copy(annotations = buffer.annotations.copy(documentComments = List(comment))))
      )
    )

    panelContent(PanelContentSync.synced(commented, before)) match
      case SurfaceContent.Comments(symbols, None) => symbols.map(_.location) shouldBe List(Location(0, 0))
      case other                                  => fail(s"unexpected content $other")
  }

  "A docked diagnostics panel" should "show the active document's diagnostics as they arrive" in {
    val (withDoc, bufferId) = shownInActivePane(AppState.initial, "val x = 1\n", "a.md")
    val before              = docked(withDoc, SurfaceContent.Diagnostics(Nil))
    val uri                 = SpellChecker.diagnosticsUri(before.persisted.buffers(bufferId))
    val lspDiagnostic =
      LspDiagnostic(LspRange(LspPosition(0, 4), LspPosition(0, 5)), Some(LspSeverity.Warning), "unused value")
    val languageService = before.runtime.languageService
    val arrived = before.copy(runtime =
      before.runtime.copy(languageService =
        languageService.copy(diagnosticsState =
          languageService.diagnosticsState.copy(diagnostics = Map(uri -> List(lspDiagnostic)))
        )
      )
    )

    panelContent(PanelContentSync.synced(arrived, before)) shouldBe
      SurfaceContent.Diagnostics(List(Diagnostic("unused value", DiagnosticSeverity.Warning, Location(0, 4))), None)
  }

  "A docked markdown preview" should "follow a switch to another markdown document" in {
    val (first, firstId)     = shownInActivePane(AppState.initial, "# First\n", "a.md")
    val before               = docked(first, SurfaceContent.MarkdownPreview(firstId, "a.md"))
    val (switched, secondId) = shownInActivePane(before, "# Second\n", "b.md")

    panelContent(PanelContentSync.synced(switched, before)) shouldBe SurfaceContent.MarkdownPreview(secondId, "b.md")
  }
  "A docked explorer" should "wait on every directory it shows but has no listing for" in {
    val root   = Paths.get("/repo")
    val src    = root.resolve("src")
    val before = AppState.initial
    val after  = docked(before, SurfaceContent.DirectoryTree(DirectoryTreeData(root, expandedPaths = Set(src))))

    val synced = PanelContentSync.synced(after, before)

    panelContent(synced) shouldBe
      SurfaceContent.DirectoryTree(DirectoryTreeData(root, expandedPaths = Set(src), loading = Set(root, src)))
    PanelContentSync.explorerListingsDue(synced, before) shouldBe List(panelId -> root, panelId -> src)
  }

  it should "not ask again for a listing already on its way, or one that failed" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val waiting = docked(
      AppState.initial,
      SurfaceContent.DirectoryTree(
        DirectoryTreeData(root, expandedPaths = Set(src), loading = Set(root), failed = Map(src -> "denied"))
      )
    )

    PanelContentSync.synced(waiting, waiting) shouldBe waiting
    PanelContentSync.explorerListingsDue(waiting, waiting) shouldBe Nil
  }

  it should "list a stale directory again, keeping its entries on show meanwhile" in {
    val root    = Paths.get("/repo")
    val listing = List(DirEntry(root.resolve("a.md"), "a.md", isDirectory = false))
    val stale   = DirectoryTreeData(root, entries = Map(root -> listing), stale = Set(root))
    val state   = docked(AppState.initial, SurfaceContent.DirectoryTree(stale))

    panelContent(PanelContentSync.synced(state, state)) shouldBe
      SurfaceContent.DirectoryTree(stale.copy(loading = Set(root)))
  }
  it should "mark only directories it has listed as stale" in {
    val root    = Paths.get("/repo")
    val listing = List(DirEntry(root.resolve("a.md"), "a.md", isDirectory = false))
    val listed  = DirectoryTreeData(root, entries = Map(root -> listing))
    val state   = docked(AppState.initial, SurfaceContent.DirectoryTree(listed))

    panelContent(PanelContentSync.withStaleDirectories(state, Set(root, Paths.get("/elsewhere")))) shouldBe
      SurfaceContent.DirectoryTree(listed.copy(stale = Set(root)))
    PanelContentSync.explorerWatchDirectories(state) shouldBe Set(root)
  }
end PanelContentSyncSpec
