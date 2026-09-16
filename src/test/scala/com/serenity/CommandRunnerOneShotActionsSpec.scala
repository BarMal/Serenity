package com.serenity

import com.serenity.command.*
import com.serenity.lsp.config.LanguageId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every one-shot action issue #1057 found embedded in the settings tree (a "settings item" that executes an action
  * rather than holding a persisted value) must be reachable as an ordinary `CommandRegistry` command. Three of the four
  * locations (Navigation, Theme Selection, Panel Actions) turn out to already be registered as commands alongside their
  * settings-tree duplicates -- those assertions are green today. The fourth (buffer-language switchers) is not, and is
  * deliberately left red this turn: registering `lang-<id>` commands while `CommandRunnerSettingsItems.languageItems`'s
  * identical ids are still live in the settings tree makes an exact-match command outrank the exact-match settings
  * target it collides with, which broke two previously-passing specs (`CommandRunnerFloatingRenderingSpec`,
  * `CommandRunnerReducerSpec`) when tried. The next turn registers them *and* removes the settings-tree group in the
  * same commit, so the collision never exists in a committed state.
  */
class CommandRunnerOneShotActionsSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.default

  private def commandNames: Set[String] = registry.getAllCommands.map(_.name).toSet

  // CommandRunnerSettingsGroups.scala: the "Navigation" group's one-shot commands.
  "the registry" should "already register every Navigation group one-shot action (issue #1057)" in {
    val navigationCommandNames = Set(
      "comment-lens",
      "add-document-comment",
      "delete-document-comment",
      "toggle-bookmark",
      "next-bookmark",
      "previous-bookmark",
      "next-document-comment",
      "previous-document-comment",
      "next-document-symbol",
      "previous-document-symbol",
      "navigate-back",
      "navigate-forward"
    )
    navigationCommandNames.diff(commandNames) shouldBe empty
  }

  // CommandRunnerSettingsItems.scala: the "Theme Selection" group's one-shot commands.
  it should "already register every Theme Selection one-shot action (issue #1057)" in {
    val themeCommandNames = Set("theme-chooser", "theme-creator", "toggle-theme", "reload-theme")
    themeCommandNames.diff(commandNames) shouldBe empty
  }

  // CommandRunnerSettingsItems.scala: the "Panel Actions" group's one-shot commands (per pinned edge).
  it should "already register every Panel Actions one-shot action (issue #1057)" in {
    val panelActionCommandNames = for
      position <- Set("left", "right", "bottom")
      verb     <- Set("focus", "expand", "unpin")
    yield s"$verb-$position-panel"
    panelActionCommandNames.diff(commandNames) shouldBe empty
  }

  // issue #1047: the buffer-language switchers are one picker under Language Tools rather than 23 palette commands
  // whose labels ("Markdown", "Java"...) collided with every other search for those words.
  it should "not register a palette command per buffer language" in {
    commandNames.filter(_.startsWith("lang-")) shouldBe empty
  }

  private def languagePicker(current: Option[LanguageId]): CommandSurfaceItem.GroupItem =
    def groups(items: List[CommandSurfaceItem]): List[CommandSurfaceItem.GroupItem] =
      items.collect { case group: CommandSurfaceItem.GroupItem => group }.flatMap(g => g :: groups(g.children))
    val runner = CommandRunner.empty.activate(
      registry,
      com.serenity.config.AppConfig.default,
      context = CommandRunnerContext(bufferLanguage = current)
    )
    groups(runner.settingsGroups).find(_.id == "buffer-language").getOrElse(fail("missing buffer-language picker"))

  "the buffer language picker" should "offer every language with a SetBufferLanguage intent for it" in {
    val picker  = languagePicker(Some(LanguageId.Scala))
    val entries = picker.children.collect { case CommandSurfaceItem.CommandItem(command) => command }

    picker.hint shouldBe Some("Scala")
    entries.map(_.name) should contain("lang-plain-text")
    entries.find(_.name == "lang-plain-text").map(_.intent) shouldBe Some(
      CommandIntent.File(FileIntent.SetBufferLanguage(None))
    )
    LanguageId.values.foreach { lang =>
      entries.find(_.name == s"lang-${lang.id}").map(_.intent) shouldBe Some(
        CommandIntent.File(FileIntent.SetBufferLanguage(Some(lang)))
      )
    }
  }

  it should "name a plain-text buffer as such" in {
    languagePicker(None).hint shouldBe Some("Plain Text")
  }
