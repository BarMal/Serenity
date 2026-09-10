package com.serenity.ui.layout

import java.nio.file.Paths

import com.serenity.config.AppMode
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `PanelContentResolver` (issue #1421) -- `SurfaceContentResolverSpec` exercises some of
  * this behavior indirectly through `SurfaceContentResolver.resolve`, but never names this object, and never reaches
  * every `SurfaceLayoutKind` branch each method here switches on. Lives in this package because the resolver is
  * `private[layout]`.
  */
class PanelContentResolverSpec extends AnyFlatSpec with Matchers:

  private val root = Paths.get("/repo")

  // ── resolveDirectoryListing ────────────────────────────────────────────────

  "resolveDirectoryListing" should "join entries onto one row for a floating horizontal layout" in {
    val resolved = PanelContentResolver.resolveDirectoryListing(
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Floating,
      "repo",
      List("src", "test", "build.sbt", "README.md", "LICENSE"),
      selectedName = None
    )

    resolved.rows.map(_.plainText) shouldBe List("repo  src | test | build.sbt | README.md")
  }

  it should "cap floating compact listings to a single count summary row" in {
    val resolved = PanelContentResolver.resolveDirectoryListing(
      LayoutRect(0, 0, 10, 10),
      SurfaceRenderMode.Floating,
      "repo",
      List("src", "test"),
      selectedName = None
    )

    resolved.rows.map(_.plainText) shouldBe List("repo (2)")
  }

  it should "drop an empty entry row for a pinned horizontal listing with no entries" in {
    val resolved = PanelContentResolver.resolveDirectoryListing(
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Pinned,
      "repo",
      Nil,
      selectedName = None
    )

    resolved.rows shouldBe Nil
  }

  it should "summarize pinned compact listings by entry count alone" in {
    val resolved = PanelContentResolver.resolveDirectoryListing(
      LayoutRect(0, 0, 10, 10),
      SurfaceRenderMode.Pinned,
      "repo",
      List("a", "b", "c"),
      selectedName = None
    )

    resolved.rows.map(_.plainText) shouldBe List("3 entries")
  }

  // ── resolveTerminal ─────────────────────────────────────────────────────────

  "resolveTerminal" should "number lines for a vertical layout" in {
    val resolved = PanelContentResolver.resolveTerminal(
      LayoutRect(0, 0, 20, 50),
      SurfaceRenderMode.Floating,
      buffer = "alpha\nbeta\ngamma",
      cursor = 3
    )

    resolved.rows.map(_.plainText) shouldBe List("1: alpha", "2: beta", "3: gamma")
  }

  it should "prefix a square layout with the cursor position" in {
    val resolved = PanelContentResolver.resolveTerminal(
      LayoutRect(0, 0, 20, 20),
      SurfaceRenderMode.Floating,
      buffer = "alpha\nbeta",
      cursor = 7
    )

    resolved.rows.map(_.plainText) shouldBe List("cursor: 7", "alpha", "beta")
  }

  it should "summarize a compact layout as line count and cursor" in {
    val resolved = PanelContentResolver.resolveTerminal(
      LayoutRect(0, 0, 10, 3),
      SurfaceRenderMode.Floating,
      buffer = "alpha\nbeta\ngamma",
      cursor = 2
    )

    resolved.rows.map(_.plainText) shouldBe List("3 lines", "cursor 2")
  }

  it should "clip horizontal terminal output to the available rows" in {
    val resolved = PanelContentResolver.resolveTerminal(
      LayoutRect(0, 0, 40, 5),
      SurfaceRenderMode.Floating,
      buffer = (1 to 10).map(n => s"line$n").mkString("\n"),
      cursor = 0
    )

    resolved.rows.map(_.plainText) shouldBe List("line1", "line2", "line3")
  }

  // ── resolveOutline ──────────────────────────────────────────────────────────

  private def symbolAt(name: String, line: Int): Symbol =
    Symbol(name, SymbolKind.Function, Location(line, 0))

  "resolveOutline" should "bracket the active symbol among a horizontal preview" in {
    val active = symbolAt("bar", 1)
    val resolved = PanelContentResolver.resolveOutline(
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Floating,
      List(symbolAt("foo", 0), active, symbolAt("baz", 2)),
      activeLocation = Some(active.location)
    )

    resolved.rows.map(_.plainText) shouldBe List("foo | [bar] | baz")
    resolved.rows.head.selected shouldBe true
  }

  it should "prefix the active symbol with a marker in a vertical layout" in {
    val active = symbolAt("bar", 1)
    val resolved = PanelContentResolver.resolveOutline(
      LayoutRect(0, 0, 20, 50),
      SurfaceRenderMode.Floating,
      List(symbolAt("foo", 0), active),
      activeLocation = Some(active.location)
    )

    resolved.rows.map(_.plainText) shouldBe List("Function foo", "> Function bar")
    resolved.rows.map(_.selected) shouldBe List(false, true)
  }

  it should "show the active symbol name as a second row in compact layout" in {
    val active = symbolAt("bar", 1)
    val resolved = PanelContentResolver.resolveOutline(
      LayoutRect(0, 0, 10, 3),
      SurfaceRenderMode.Floating,
      List(symbolAt("foo", 0), active),
      activeLocation = Some(active.location)
    )

    resolved.rows.map(_.plainText) shouldBe List("2 symbols", "bar")
    resolved.rows.head.selected shouldBe true
  }

  it should "report only the count in compact layout when nothing is active" in {
    val resolved = PanelContentResolver.resolveOutline(
      LayoutRect(0, 0, 10, 3),
      SurfaceRenderMode.Floating,
      List(symbolAt("foo", 0)),
      activeLocation = None
    )

    resolved.rows.map(_.plainText) shouldBe List("1 symbols")
    resolved.rows.head.selected shouldBe false
  }

  // ── resolveComments ─────────────────────────────────────────────────────────

  "resolveComments" should "treat square layout the same as vertical" in {
    val active = symbolAt("note", 3)
    val resolved = PanelContentResolver.resolveComments(
      LayoutRect(0, 0, 20, 20),
      SurfaceRenderMode.Floating,
      List(active),
      activeLocation = Some(active.location)
    )

    resolved.rows.map(_.plainText) shouldBe List("> note")
    resolved.rows.head.selected shouldBe true
  }

  it should "summarize compact layout by comment count" in {
    val resolved = PanelContentResolver.resolveComments(
      LayoutRect(0, 0, 10, 3),
      SurfaceRenderMode.Floating,
      List(symbolAt("note", 0)),
      activeLocation = None
    )

    resolved.rows.map(_.plainText) shouldBe List("1 comments")
  }

  // ── resolveDiagnostics ──────────────────────────────────────────────────────

  private def diagnostic(message: String, severity: DiagnosticSeverity, line: Int): Diagnostic =
    Diagnostic(message, severity, Location(line, 0))

  "resolveDiagnostics" should "summarize severity counts on one horizontal row" in {
    val issues = List(
      diagnostic("boom", DiagnosticSeverity.Error, 0),
      diagnostic("careful", DiagnosticSeverity.Warning, 1),
      diagnostic("note", DiagnosticSeverity.Info, 2),
      diagnostic("hint", DiagnosticSeverity.Hint, 3)
    )

    val resolved = PanelContentResolver.resolveDiagnostics(
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Floating,
      issues,
      activeLocation = None
    )

    resolved.rows.map(_.plainText) shouldBe List("1 error | 1 warning | 2 info")
  }

  it should "list vertical rows with severity and message, marking the active location" in {
    val active = diagnostic("boom", DiagnosticSeverity.Error, 0)
    val resolved = PanelContentResolver.resolveDiagnostics(
      LayoutRect(0, 0, 20, 50),
      SurfaceRenderMode.Floating,
      List(active, diagnostic("careful", DiagnosticSeverity.Warning, 1)),
      activeLocation = Some(active.location)
    )

    resolved.rows.map(_.plainText) shouldBe List("Error: boom", "Warning: careful")
    resolved.rows.map(_.selected) shouldBe List(true, false)
  }

  it should "prefix square-layout diagnostics with an error/warning summary" in {
    val resolved = PanelContentResolver.resolveDiagnostics(
      LayoutRect(0, 0, 20, 20),
      SurfaceRenderMode.Floating,
      List(diagnostic("boom", DiagnosticSeverity.Error, 0)),
      activeLocation = None
    )

    resolved.rows.map(_.plainText) shouldBe List("1 error, 0 warning", "boom")
  }

  it should "summarize compact-layout diagnostics as total and error count" in {
    val resolved = PanelContentResolver.resolveDiagnostics(
      LayoutRect(0, 0, 10, 3),
      SurfaceRenderMode.Floating,
      List(diagnostic("boom", DiagnosticSeverity.Error, 0), diagnostic("careful", DiagnosticSeverity.Warning, 1)),
      activeLocation = None
    )

    resolved.rows.map(_.plainText) shouldBe List("2 issues", "1 error")
  }

  // ── resolveShortcutsHelp ────────────────────────────────────────────────────

  "resolveShortcutsHelp" should "render every layout kind identically -- group headings followed by entries" in {
    val groups = List(ShortcutHelpGroup("Global", List(ShortcutHelpEntry("Save", "ctrl+s"))))

    val floating =
      PanelContentResolver.resolveShortcutsHelp(LayoutRect(0, 0, 10, 20), SurfaceRenderMode.Floating, groups)
    // Width alone would classify as SurfaceLayoutKind.Compact, but resolveShortcutsHelp never branches on layout
    // kind (unlike its siblings above) -- only `rect.height` bounds the row count, so a narrow-but-tall rect still
    // renders identically to a wide one.
    val narrow = PanelContentResolver.resolveShortcutsHelp(LayoutRect(0, 0, 10, 6), SurfaceRenderMode.Floating, groups)

    floating.rows.map(_.plainText) shouldBe List("Global", "Save: ctrl+s")
    narrow.rows.map(_.plainText) shouldBe List("Global", "Save: ctrl+s")
  }

  // ── resolveTabList ──────────────────────────────────────────────────────────

  "resolveTabList" should "mark the active buffer selected and append a dirty marker" in {
    val activeId = BufferId(2)
    val resolved = PanelContentResolver.resolveTabList(
      LayoutRect(0, 0, 20, 20),
      SurfaceRenderMode.Floating,
      List(
        TabListEntry(BufferId(1), "clean.scala", isDirty = false),
        TabListEntry(activeId, "dirty.scala", isDirty = true)
      ),
      activeBufferId = Some(activeId)
    )

    resolved.rows.map(_.plainText) shouldBe List("clean.scala", "dirty.scala ●")
    resolved.rows.map(_.selected) shouldBe List(false, true)
  }

  it should "clip the tab list to the rows the rect allows" in {
    val entries = (1 to 10).map(n => TabListEntry(BufferId(n), s"buf$n.scala", isDirty = false)).toList
    val resolved = PanelContentResolver.resolveTabList(
      LayoutRect(0, 0, 20, 4),
      SurfaceRenderMode.Floating,
      entries,
      activeBufferId = None
    )

    resolved.rows should have size 2
  }

  // ── resolveRecentFilesInMode ────────────────────────────────────────────────

  "resolveRecentFilesInMode" should "show a placeholder row when there are no recent files" in {
    val resolved = PanelContentResolver.resolveRecentFilesInMode(
      LayoutRect(0, 0, 20, 20),
      SurfaceRenderMode.Pinned,
      AppMode.Code,
      Nil
    )

    resolved.title shouldBe Some("Recent in Code Mode")
    resolved.rows.map(_.plainText) shouldBe List("No recent files in this mode yet")
  }

  it should "list recent file paths in full" in {
    val paths = List(root.resolve("a.scala"), root.resolve("b.scala"))
    val resolved = PanelContentResolver.resolveRecentFilesInMode(
      LayoutRect(0, 0, 40, 20),
      SurfaceRenderMode.Floating,
      AppMode.Code,
      paths
    )

    resolved.rows.map(_.plainText) shouldBe paths.map(_.toString)
  }
