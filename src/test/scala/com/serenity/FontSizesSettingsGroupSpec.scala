package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.TypographyRole
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.fonts.{FontLoader, FontSizes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2405: the "Font Sizes" group at the top of Settings > Typography shows the prose, code and UI sizes together in
  * every app mode, marks the size the active document is drawn with, sets all three behind a confirmation and undoes
  * the last change. A terminal has no font sizes to set, so it has no such group.
  */
class FontSizesSettingsGroupSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val GroupId = "settings-font-sizes"
  private val catalog = FontLoader.FontFamilyCatalog(monospace = List("Mono"), text = List("Serif"), ui = Nil)
  private val prose   = "app-mode"          -> 1
  private val showAll = "settings-show-all" -> 0

  private def configWith(fonts: FontConfig): AppConfig = AppConfig.default.withFontConfig(fonts)

  private def treeOn(
    capabilities: FrontendCapabilities,
    config: AppConfig = AppConfig.default,
    context: CommandRunnerContext = CommandRunnerContext.empty,
    selections: (String, Int)*
  ): List[CommandSurfaceItem.GroupItem] =
    CommandRunnerSettingsGroups.build(
      selections.toMap,
      CommandRunnerSettingsInputItems.build(config, capabilities),
      Nil,
      None,
      capabilities,
      catalog,
      context = context
    )

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def allRows(groups: List[CommandSurfaceItem.GroupItem]): List[CommandSurfaceItem] =
    groups.flatMap(group => group :: descendants(group))

  private def typography(groups: List[CommandSurfaceItem.GroupItem]): CommandSurfaceItem.GroupItem =
    groups.find(_.id == "settings-typography").getOrElse(fail("missing the Typography group"))

  private def sizes(groups: List[CommandSurfaceItem.GroupItem]): CommandSurfaceItem.GroupItem =
    typography(groups).children
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == GroupId => group }
      .getOrElse(fail("missing the Font Sizes group"))

  private def input(group: CommandSurfaceItem.GroupItem, id: String): CommandSurfaceItem.InputItem =
    group.children
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == id => item }
      .getOrElse(fail(s"missing input $id"))

  "The Font Sizes group" should "lead Settings > Typography in a code workspace" in {
    typography(treeOn(FrontendCapabilities.gui)).children.map(_.id).headOption shouldBe Some(GroupId)
  }

  it should "lead it in a prose workspace too, ahead of the per-role groups, which keep their mode filter" in {
    val children = typography(treeOn(FrontendCapabilities.gui, selections = prose)).children.map(_.id)

    children.take(2) shouldBe List(GroupId, "settings-prose-font")
    children should not contain "settings-code-font"
  }

  it should "list the prose, code and UI sizes, then Set All Sizes and Undo Last Size Change" in {
    sizes(treeOn(FrontendCapabilities.gui)).children.map(_.id) shouldBe
      List("font-sizes-prose", "font-sizes-code", "font-sizes-ui", "set-all-font-sizes", "undo-font-size-change")
  }

  it should "show each size's current value" in {
    val config = configWith(FontConfig(fontSize = 13.0f, textFontSize = 36.0f, uiFontSize = 11.0f))

    val group = sizes(treeOn(FrontendCapabilities.gui, config))

    List("font-sizes-prose", "font-sizes-code", "font-sizes-ui").map(input(group, _).currentValue) shouldBe
      List("36.0", "13.0", "11.0")
  }

  it should "be absent on a terminal, which has no font sizes to set" in {
    val tui = FrontendCapabilities.tui()

    allRows(treeOn(tui, selections = prose)).map(_.id) should not contain GroupId
    allRows(treeOn(tui)).map(_.id) should not contain GroupId
  }

  it should "come back on a terminal only with Show All Settings, led by the inert note" in {
    val group = sizes(treeOn(FrontendCapabilities.tui(), selections = Seq(prose, showAll)*))

    group.hint.getOrElse("") should startWith("Inert in TUI mode -- ")
  }

  "Set All Sizes" should "take one size in the range every size accepts and ask before applying it" in {
    val set = input(sizes(treeOn(FrontendCapabilities.gui)), "set-all-font-sizes")

    set.parse("16") shouldBe Some(CommandIntent.Settings(SettingsIntent.Font(FontIntent.ConfirmSetAllFontSizes(16.0f))))
    set.parse("8.0").isDefined shouldBe true
    set.parse("48.0").isDefined shouldBe true
    set.parse("7.9") shouldBe None
    set.parse("48.1") shouldBe None
    set.parse("big") shouldBe None
  }

  "Undo Last Size Change" should "be unavailable, and say why, while there is nothing to undo" in {
    val group = sizes(treeOn(FrontendCapabilities.gui))

    group.children.collectFirst { case item: CommandSurfaceItem.CommandItem => item.disabledReason } shouldBe
      Some(Some("No size change to undo"))
  }

  it should "run the undo once a size change has been made" in {
    val context = CommandRunnerContext(previousFontSizes = Some(FontSizes(36.0f, 12.0f, 12.0f)))

    val undo = sizes(treeOn(FrontendCapabilities.gui, context = context)).children
      .collectFirst { case item: CommandSurfaceItem.CommandItem => item }
      .getOrElse(fail("missing the undo row"))

    undo.disabledReason shouldBe None
    undo.command.keepMenuOpenOnSubmit shouldBe true
    undo.command.intent shouldBe CommandIntent.Settings(SettingsIntent.Font(FontIntent.UndoFontSizeChange))
    undo.command.label shouldBe "Undo Last Size Change"
  }

  "The size the active document is drawn with" should "be marked, by the document's typography role" in {
    def marked(role: Option[TypographyRole]): List[String] =
      val group = sizes(treeOn(FrontendCapabilities.gui, context = CommandRunnerContext(activeTypographyRole = role)))
      List("font-sizes-prose", "font-sizes-code", "font-sizes-ui").filter(
        input(group, _).label.endsWith("(in use)")
      )

    marked(Some(TypographyRole.Prose)) shouldBe List("font-sizes-prose")
    marked(Some(TypographyRole.MarkdownSource)) shouldBe List("font-sizes-prose")
    marked(Some(TypographyRole.MarkdownPreview)) shouldBe List("font-sizes-prose")
    marked(Some(TypographyRole.Mixed)) shouldBe List("font-sizes-prose")
    marked(Some(TypographyRole.Code)) shouldBe List("font-sizes-code")
    marked(None) shouldBe Nil
  }

  it should "be marked on the per-role rows too, so the palette's settings entries carry it" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val context           = CommandRunnerContext(activeTypographyRole = Some(TypographyRole.Prose))

    val labels = CommandRunner.empty
      .activate(registry, AppConfig.default, context = context)
      .updateSearchTerm("Prose Font Size")
      .visibleItems
      .collect {
        case item: CommandSurfaceItem.SettingSearchItem if item.label.startsWith("Prose Font Size") => item.label
      }

    labels should contain("Prose Font Size (in use)")
  }

  "An activated command runner" should "offer the undo once the config holds sizes to go back to, and not before" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val undone            = AppConfig.default
    val changed           = configWith(FontConfig(previousSizes = Some(FontSizes(36.0f, 12.0f, 12.0f))))

    def undoReason(runner: CommandRunner): Option[String] =
      runner.openSettings.settingsGroups
        .find(_.id == "settings-typography")
        .flatMap(_.children.collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == GroupId => group })
        .flatMap(_.children.collectFirst { case item: CommandSurfaceItem.CommandItem => item.disabledReason })
        .getOrElse(fail("missing the undo row"))

    val activated = CommandRunner.empty.activate(registry, undone)

    undoReason(activated) shouldBe Some("No size change to undo")
    undoReason(activated.updateInputItems(changed)) shouldBe None
    undoReason(CommandRunner.empty.activate(registry, changed)) shouldBe None
  }
