package com.serenity.command

import com.serenity.config.AppMode
import com.serenity.state.models.{EditingContext, Focus, PaneId, Shell}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandScopeSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.withToggleUI

  private def context(mode: AppMode, shell: Shell = Shell.Gui): EditingContext =
    EditingContext(mode, buffer = None, shell = shell, focus = Focus.EditorPane(PaneId(0)))

  private def namesIn(family: CommandFamily): Set[String] =
    registry.getAllCommands.filter(_.scope.family == family).map(_.name).toSet

  // Pinned on purpose: a command changing family is a user-visible decision (it appears or disappears per mode), so it
  // should show up here as a deliberate edit rather than slip through with an intent refactor.
  private val codeCommands = Set(
    "format",
    "lsp-completion",
    "lsp-definition",
    "lsp-hover",
    "lsp-references",
    "lsp-rename",
    "pin-diagnostics",
    "project-build",
    "project-cancel",
    "project-debug",
    "project-dependencies",
    "project-run",
    "project-test"
  )

  private val proseCommands = Set(
    "align-center",
    "align-justify",
    "align-left",
    "align-right",
    "bold",
    "cut-to-darlings",
    "decrease-text-size",
    "delete-placeholder",
    "heading-1",
    "heading-2",
    "heading-3",
    "increase-text-size",
    "italic",
    "next-placeholder",
    "paragraph-body",
    "paragraph-drop-cap",
    "previous-placeholder",
    "restore-darling",
    "underline"
  )

  "CommandFamily" should "admit Core in every mode and Code/Prose only in their own mode" in {
    CommandFamily.Core.modes shouldBe AppMode.values.toSet
    CommandFamily.Code.modes shouldBe Set(AppMode.Code)
    CommandFamily.Prose.modes shouldBe Set(AppMode.Prose)
  }

  "FrontendSupport" should "admit Both in every shell and GuiOnly only in the GUI" in {
    FrontendSupport.Both.shells shouldBe Shell.values.toSet
    FrontendSupport.GuiOnly.shells shouldBe Set(Shell.Gui)
  }

  "CommandScope.of" should "classify exactly the pinned code commands as Code" in {
    namesIn(CommandFamily.Code) shouldBe codeCommands
  }

  it should "classify exactly the pinned prose commands as Prose" in {
    namesIn(CommandFamily.Prose) shouldBe proseCommands
  }

  it should "classify every other registered command as Core" in {
    namesIn(CommandFamily.Core) shouldBe registry.getAllCommands.map(_.name).toSet -- codeCommands -- proseCommands
  }

  it should "leave settings changes available everywhere, including switching the app mode itself" in {
    CommandScope.of(CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose))) shouldBe CommandScope.core
    CommandScope.of(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetBlurRadius(4f)))
    ) shouldBe CommandScope.core
  }

  "CommandScope.admits" should "combine the mode and frontend dimensions" in {
    val proseGui = CommandScope(CommandFamily.Prose, FrontendSupport.GuiOnly)

    proseGui.admits(context(AppMode.Prose, Shell.Gui)) shouldBe true
    proseGui.admits(context(AppMode.Prose, Shell.Tui)) shouldBe false
    proseGui.admits(context(AppMode.Code, Shell.Gui)) shouldBe false
  }

  "CommandScope.unavailableReason" should "name the mode a command needs" in {
    CommandScope(CommandFamily.Code, FrontendSupport.Both).unavailableReason(context(AppMode.Prose)) shouldBe
      Some("Only available in code mode.")
    CommandScope(CommandFamily.Prose, FrontendSupport.Both).unavailableReason(context(AppMode.Code)) shouldBe
      Some("Only available in prose mode.")
  }

  it should "name the graphical app for a GUI-only command in the terminal" in {
    CommandScope(CommandFamily.Core, FrontendSupport.GuiOnly).unavailableReason(
      context(AppMode.Code, Shell.Tui)
    ) shouldBe
      Some("Only available in the graphical app.")
  }

  it should "be empty when the scope admits the context" in {
    CommandScope.core.unavailableReason(context(AppMode.Prose, Shell.Tui)) shouldBe None
  }

  "BufferRequirement" should "mark rich-text commands and the Markdown preview" in {
    registry.findCommand("bold").map(_.bufferRequirement) shouldBe Some(BufferRequirement.RichText)
    registry.findCommand("markdown-preview").map(_.bufferRequirement) shouldBe Some(BufferRequirement.Markdown)
    registry.findCommand("save").map(_.bufferRequirement) shouldBe Some(BufferRequirement.AnyBuffer)
  }

  "CommandRegistry.availableCommands" should "offer code commands but no prose commands in code mode" in {
    val names = registry.availableCommands(AppMode.Code, Shell.Gui).map(_.name).toSet

    names should contain allElementsOf codeCommands
    names.intersect(proseCommands) shouldBe empty
  }

  it should "offer prose commands but no code commands in prose mode" in {
    val names = registry.availableCommands(AppMode.Prose, Shell.Tui).map(_.name).toSet

    names should contain allElementsOf proseCommands
    names.intersect(codeCommands) shouldBe empty
  }

  it should "keep registry order, which the palette's recency ranking relies on" in {
    val available = registry.availableCommands(AppMode.Code, Shell.Gui)

    available shouldBe registry.getAllCommands.filter(command => available.contains(command))
  }

  it should "cover every registered command across all modes and frontends" in {
    val everywhere =
      for
        mode  <- AppMode.values.toList
        shell <- Shell.values.toList
        c     <- registry.availableCommands(mode, shell)
      yield c.name

    everywhere.toSet shouldBe registry.getAllCommands.map(_.name).toSet
  }
