package com.serenity.command

import com.serenity.config.{AppConfig, AppMode}
import com.serenity.state.models.{BufferKind, EditingContext, Focus, PaneId, Shell}
import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2577 P1a: the palette's results come from one pure function, so the command picker that replaces the palette
  * surface lists exactly what the palette does today. Settings hits are passed in, since they come from the settings
  * index the runner carries.
  */
class CommandPaletteResultsSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.default

  private val fontFamilies =
    FontLoader.FontFamilyCatalog(monospace = List("Mono A"), text = List("Serif A"), ui = List("Sans A"))

  private def context(mode: AppMode, buffer: BufferKind, hasSelection: Boolean = false): CommandRunnerContext =
    CommandRunnerContext(editingContext =
      Some(EditingContext(mode, Some(buffer), Shell.Gui, Focus.EditorPane(PaneId(0)), hasSelection))
    )

  private val usage = Map(CommandId("undo") -> 1, CommandId("toggle-line-wrap") -> 2)

  private def runnerFor(ctx: CommandRunnerContext, query: String): CommandRunner =
    CommandRunner.empty
      .copy(fontFamilies = fontFamilies)
      .activate(registry, AppConfig.default, context = ctx)
      .copy(commandUsage = usage)
      .updateSearchTerm(query)(using registry)

  private val queries =
    List(
      "",
      "save",
      "line",
      "goto line",
      "run tests",
      "theme",
      "font size",
      "tab width",
      "wrap",
      "tgogle theme",
      "bold"
    )

  for
    (name, ctx) <- List(
      "code"           -> context(AppMode.Code, BufferKind.PlainText),
      "prose"          -> context(AppMode.Prose, BufferKind.RichText),
      "code selection" -> context(AppMode.Code, BufferKind.PlainText, hasSelection = true)
    )
    query <- queries
  do
    s"The results for '$query' in a $name workspace" should "be what the palette lists" in {
      val runner = runnerFor(ctx, query)

      val results = CommandPaletteResults.items(
        query,
        CommandPaletteResults.matching(registry, query, usage, ctx),
        usage,
        ctx,
        runner.searchResults.settingsResults
      )

      results shouldBe runner.visibleItems
    }

  "Matching" should "keep only offered commands and cap the list at fifty" in {
    val ctx     = context(AppMode.Prose, BufferKind.RichText)
    val matched = CommandPaletteResults.matching(registry, "e", Map.empty, ctx)

    matched.size shouldBe CommandPaletteResults.MaxMatches
    matched.forall(CommandAvailability.of(_, ctx).isOffered) shouldBe true
  }

  it should "list every command, unranked, for an empty query" in {
    CommandPaletteResults.matching(
      registry,
      "",
      usage,
      context(AppMode.Code, BufferKind.PlainText)
    ) shouldBe
      registry.getAllCommands
  }
