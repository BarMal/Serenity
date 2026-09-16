package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSettingsSearchSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def groupByIdRecursive(
    groups: List[CommandSurfaceItem.GroupItem],
    id: String
  ): CommandSurfaceItem.GroupItem =
    (groups ++ groups.flatMap(group =>
      descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child }
    ))
      .find(_.id == id)
      .getOrElse(fail(s"missing group $id"))

  "CommandRunner state" should "initialize with empty search and no selection" in {
    val runner = CommandRunner.empty

    runner.searchTerm shouldBe ""
    runner.selectedIndex shouldBe 0
    runner.isActive shouldBe false
    runner.filteredCommands shouldBe empty
  }

  it should "update search term and filter commands" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner            = CommandRunner.empty.activate(registry, AppConfig.default)

    val updated = runner.updateSearchTerm("save")
    updated.searchTerm shouldBe "save"
    updated.filteredCommands should not be empty
    updated.filteredCommands.exists(_.name.contains("save")) shouldBe true
  }

  // issue #931: category tabs -- and the `activeCategory` field they drove -- are retired outright, so an empty
  // query is simply every registered command now.
  it should "show every command when search is empty, and switch to global search once typing begins" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)

    runner.visibleItems should not be empty
    runner.visibleItems.collect {
      case CommandSurfaceItem.CommandItem(command) => command.category
    }.distinct should contain(CommandCategory.Settings)

    val searched = runner.updateSearchTerm("theme")
    searched.searchTerm shouldBe "theme"
    searched.visibleItems.exists {
      case CommandSurfaceItem.CommandItem(command) => command.name == "toggle-theme"
      case _                                       => false
    } shouldBe true
  }

  it should "surface motion settings as an expandable group in settings browsing" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)

    val animationGroup = groupByIdRecursive(runner.settingsGroups, "settings-animation")

    animationGroup.label shouldBe "Motion & Animation"
    animationGroup.children.map(_.id) shouldBe List(
      "motion-accessibility",
      "motion-preset",
      "editor-text-transition",
      "panel-open-transition",
      "panel-close-transition",
      "command-runner-transition",
      "command-runner-fade",
      "ui-animation",
      "element-transition-speed-scale",
      "editor-text-speed-scale",
      "command-runner-speed-scale",
      "ui-speed-scale",
      "cursor-speed-scale",
      "window-sitter-enabled",
      "window-sitter-action",
      "window-sitter-frames",
      "window-sitter-active-ticks",
      "window-sitter-fast-active-ticks",
      "window-sitter-fast-threshold-ms"
    )
  }

  it should "return all matching global motion targets for an animation search" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withMotionPreset(MotionPreset.Custom))
      .updateSearchTerm("animation")

    // Window-sitter settings now live inside Motion & Animation (consolidated from Interface Layout),
    // so "settings-animation" is the sole direct global match and the search short-circuits to it.
    runner.visibleItems.collect { case group: CommandSurfaceItem.GroupItem => group.id } shouldBe List(
      "settings-animation"
    )
  }

  // issue #1549: the command runner/palette's visible-item count is governed entirely by Interface Density (issue
  // #1046 folded the old standalone "visible rows" knob into it), but nothing about that item's label, id, or
  // breadcrumb ever said so -- searching the very words a user looking for that setting would type ("command
  // runner", "palette", "visible items") either found nothing or, worse, landed on the unrelated Command Runner Key
  // Hints toggle (whose label happens to also contain "Command Runner"). The setting's hint text now names it.
  it should "surface Interface Density as a settings search result for \"command runner\", \"palette\", and \"visible items\"" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry

    List("command runner", "palette", "visible items").foreach { query =>
      val runner = CommandRunner.empty.activate(registry, AppConfig.default).updateSearchTerm(query)

      val matchedTargetIds = runner.visibleItems.collect {
        case item: CommandSurfaceItem.SettingSearchItem => item.targetItemId
        case item: CommandSurfaceItem.GroupItem         => item.id
      }
      withClue(s"query '$query' -> $matchedTargetIds: ") {
        matchedTargetIds should (contain("interface-density") or contain("settings-interface-layout"))
      }
    }
  }

  it should "return a unique leaf result with its breadcrumb for an exact settings search" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withMotionPreset(MotionPreset.Custom))
      .updateSearchTerm("\"ANIMATION-duration\"")

    runner.visibleItems.collect {
      case item: CommandSurfaceItem.SettingSearchItem =>
        (item.targetGroupId, item.targetItemId, item.label, item.breadcrumb)
    } shouldBe List(
      (
        "settings-animation",
        "animation-duration",
        "Animation Duration",
        "Settings > Appearance & Motion > Motion & Animation"
      )
    )
  }

  it should "describe duplicate settings as global until preset drafts have independent values and actions" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withMotionPreset(MotionPreset.Custom))
      .copy(editingPresetName = Some("Review"))
      .updateSearchTerm("animation duration")

    runner.visibleItems.collect {
      case item: CommandSurfaceItem.SettingSearchItem if item.targetItemId == "animation-duration" =>
        (item.targetGroupId, item.effectiveValue, item.sourceScope)
    } shouldBe List(("settings-animation", Some("0"), "Global"))
  }

  it should "rank a normalized exact setting ahead of a prefix command" in {
    val prefixCommand = Command.typed(
      name = "quoted-animation-duration",
      description = "A command whose label begins with the raw query.",
      intent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers)),
      label = "\"ANIMATION-duration\" options"
    )
    val registry          = CommandRegistry(List(prefixCommand))
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withMotionPreset(MotionPreset.Custom))
      .updateSearchTerm("\"ANIMATION-duration\"")

    runner.visibleItems.headOption.map(_.id) shouldBe Some("settings-search:animation-duration")
  }

  it should "keep an exact settings group query as navigation rather than a leaf edit" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("ui font")

    runner.visibleItems.headOption.map(_.id) shouldBe Some("settings-ui-font")
  }

  // issue #1057: "lang-markdown" used to be a settings-tree search target (`settings-language`); it is an ordinary
  // CommandRegistry command now, so an exact search for its own name finds the command directly, not a
  // SettingSearchItem pointing into a settings group.
  it should "find a buffer-language command by its exact name via search" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("lang-markdown")

    runner.visibleItems.headOption.collect { case CommandSurfaceItem.CommandItem(command) => command.name } shouldBe
      Some("lang-markdown")
  }

  it should "surface font settings groups ahead of command matches when searching font-related terms" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .updateSearchTerm("font")

    runner.visibleItems.headOption.map(_.id) shouldBe Some("settings-prose-font")
    runner.visibleItems.exists {
      case group: CommandSurfaceItem.GroupItem => group.id == "settings-code-font"
      case _                                   => false
    } shouldBe true
  }

  // issue #1060: a font-family picker embedded inside a settings row (e.g. rich text's "Selection Font Family",
  // or code/prose/UI font's own family picker) is an inline value editor, not an independently navigable settings
  // section -- it must not outrank the real "Prose Font"/"Code Font" sections in a bare "font" search just because
  // its parent (Document & Writing, say) happens to precede Typography in the settings tree.
  it should "not let an inline font-family picker outrank real font settings sections during search" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .updateSearchTerm("font")

    runner.visibleItems.exists {
      case group: CommandSurfaceItem.GroupItem => group.id == "rich-text-font-family"
      case _                                   => false
    } shouldBe false
  }

  it should "keep strong command matches ahead of settings groups during search" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("new")

    runner.visibleItems.headOption.map(_.id) shouldBe Some("new")
  }

  it should "handle selection navigation" in {
    val commands = List(
      Command.typed("cmd1", "Command 1", CommandIntent.Theme(ThemeIntent.ToggleTheme)),
      Command.typed(
        "cmd2",
        "Command 2",
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers))
      ),
      Command.typed(
        "cmd3",
        "Command 3",
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleGutter))
      )
    )
    val runner = CommandRunner.withCommands(commands).activate(CommandRegistry(commands), AppConfig.default)

    val movedDown = runner.moveSelection(1)
    movedDown.selectedIndex shouldBe 1

    val movedUp = movedDown.moveSelection(-1)
    movedUp.selectedIndex shouldBe 0

    // Should wrap around
    val wrapDown = runner.moveSelection(commands.length)
    wrapDown.selectedIndex shouldBe 0
  }
