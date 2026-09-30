package com.serenity

import com.serenity.command.*
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.{BufferKind, EditingContext, Focus, PaneId, Shell}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the palette shows before anything is typed: Settings first, then the commands that can act on the current
  * editing context, recently used ones first. Everything else stays reachable by search.
  */
class CommandRunnerPaletteContextSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.default

  private def context(mode: AppMode, buffer: Option[BufferKind]): EditingContext =
    EditingContext(mode, buffer, Shell.Gui, Focus.EditorPane(PaneId(0)))

  private def openingCommands(editing: Option[EditingContext]): List[String] =
    CommandRunner.empty
      .activate(registry, AppConfig.default, context = CommandRunnerContext(editingContext = editing))
      .visibleItems
      .collect { case CommandSurfaceItem.CommandItem(command) => command.name }

  private def searchedCommands(editing: EditingContext, term: String): List[String] =
    CommandRunner.empty
      .activate(registry, AppConfig.default, context = CommandRunnerContext(editingContext = Some(editing)))
      .updateSearchTerm(term)(using registry)
      .visibleItems
      .collect { case CommandSurfaceItem.CommandItem(command) => command.name }

  "the opening palette" should "keep Settings as its first row even when other commands were used more recently" in {
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .recordCommandUsage("save")
      .recordCommandUsage("undo")

    runner.visibleItems.headOption.map(_.id) shouldBe Some("open-settings")
    runner.visibleItems.lift(1).map(_.id) shouldBe Some("undo")
  }

  it should "offer every command when no editing context is known" in {
    openingCommands(None) should contain allOf ("lsp-hover", "project-run", "bold", "markdown-preview", "save")
  }

  it should "leave code tooling out of a prose workspace, search included" in {
    val prose = openingCommands(Some(context(AppMode.Prose, Some(BufferKind.PlainText))))

    prose should not contain "lsp-hover"
    prose should not contain "project-run"
    prose should not contain "format"
    prose should contain("save")

    searchedCommands(context(AppMode.Prose, None), "project run") should not contain "project-run"
  }

  it should "leave prose tooling out of a code workspace, search included" in {
    val code = openingCommands(Some(context(AppMode.Code, Some(BufferKind.RichText))))

    code should not contain "bold"
    code should not contain "cut-to-darlings"
    searchedCommands(context(AppMode.Code, None), "bold") should not contain "bold"
    searchedCommands(context(AppMode.Code, None), "darlings") should not contain "cut-to-darlings"
  }

  it should "keep rich-text formatting searchable on a plain buffer in a prose workspace" in {
    searchedCommands(context(AppMode.Prose, Some(BufferKind.PlainText)), "bold") should contain("bold")
  }

  it should "offer code tooling in a code workspace" in {
    val code = openingCommands(Some(context(AppMode.Code, Some(BufferKind.Code(LanguageId.Scala)))))

    code should contain allOf ("lsp-hover", "project-run", "format")
  }

  it should "offer rich-text formatting only on a rich-text buffer" in {
    openingCommands(Some(context(AppMode.Prose, Some(BufferKind.PlainText)))) should not contain "bold"
    openingCommands(Some(context(AppMode.Prose, Some(BufferKind.RichText)))) should contain("bold")
  }

  it should "offer the Markdown preview only on a Markdown buffer" in {
    openingCommands(Some(context(AppMode.Prose, Some(BufferKind.PlainText)))) should not contain "markdown-preview"
    openingCommands(Some(context(AppMode.Prose, Some(BufferKind.Markdown)))) should contain("markdown-preview")
  }
