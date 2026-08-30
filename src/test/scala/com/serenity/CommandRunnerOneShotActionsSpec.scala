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

  // CommandRunnerSettingsItems.scala: "Current Buffer Language" -- these did not previously exist as registry
  // commands at all (only as settings-tree entries), so this is the one genuinely new registration this turn adds.
  it should "register every buffer-language switcher as a command (issue #1057)" in {
    commandNames should contain("lang-plain-text")
    LanguageId.values.foreach(lang => commandNames should contain(s"lang-${lang.id}"))
  }

  it should "give each language command a SetBufferLanguage intent for its own language" in {
    LanguageId.values.foreach { lang =>
      val command = registry.findCommand(s"lang-${lang.id}").getOrElse(fail(s"missing lang-${lang.id}"))
      command.intent shouldBe CommandIntent.File(FileIntent.SetBufferLanguage(Some(lang)))
    }
    val plainText = registry.findCommand("lang-plain-text").getOrElse(fail("missing lang-plain-text"))
    plainText.intent shouldBe CommandIntent.File(FileIntent.SetBufferLanguage(None))
  }
