package com.serenity.command

import com.serenity.config.AppMode
import com.serenity.state.models.{EditingContext, Focus, PaneId, PanelId, PanelRegistry, Shell}
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Panel commands come from the registry -- one show/hide and one focus command per panel, rather than one per edge --
  * and each takes its availability from that panel's registration.
  */
class PanelCommandsSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.withToggleUI

  private def command(name: String): Command =
    registry.findCommand(name).getOrElse(fail(s"missing command $name"))

  private def context(mode: AppMode, shell: Shell): EditingContext =
    EditingContext(mode, buffer = None, shell = shell, focus = Focus.EditorPane(PaneId(0)))

  "The command registry" should "offer a show/hide command for every panel, labelled from its registration" in
    PanelId.values.foreach { id =>
      val toggle = command(s"toggle-${id.key}-panel")
      toggle.label shouldBe s"Show/Hide ${PanelRegistry.registrationFor(id).label}"
      toggle.intent shouldBe CommandIntent.View(ViewIntent.TogglePanelShown(id))
    }

  it should "offer a focus command for every panel that can take focus, and none for the companion" in {
    PanelId.values.filter(PanelRegistry.registrationFor(_).focusable).foreach { id =>
      command(s"focus-${id.key}-panel").intent shouldBe CommandIntent.View(ViewIntent.FocusPanel(id))
    }
    registry.findCommand("focus-companion-panel") shouldBe None
  }

  it should "offer one command to maximise or restore the focused panel" in {
    command("toggle-maximise-panel").intent shouldBe CommandIntent.View(ViewIntent.ToggleMaximisePanel)
  }

  it should "no longer offer the per-edge panel commands" in {
    val retired = List("pin-explorer", "pin-outline", "pin-comments", "pin-diagnostics", "collapse-expanded-panel") ++
      List("focus", "unpin", "expand").flatMap(verb => List("left", "right", "bottom").map(e => s"$verb-$e-panel"))

    retired.flatMap(registry.findCommand) shouldBe Nil
  }

  "A panel command's scope" should "come from the panel's registration" in {
    command("toggle-project-output-panel").scope.admits(context(AppMode.Prose, Shell.Gui)) shouldBe false
    command("toggle-project-output-panel").scope.admits(context(AppMode.Code, Shell.Gui)) shouldBe true
    command("focus-diagnostics-panel").scope.family shouldBe CommandFamily.Code
    command("toggle-outline-panel").scope shouldBe CommandScope.core
  }

  it should "keep the docked Markdown preview to the graphical app" in {
    command("toggle-markdown-preview-panel").scope.admits(context(AppMode.Prose, Shell.Tui)) shouldBe false
    command("toggle-markdown-preview-panel").scope.admits(context(AppMode.Prose, Shell.Gui)) shouldBe true
  }

  it should "apply to pinning a panel at an edge, but never stand in the way of hiding one" in {
    CommandScope.of(
      CommandIntent.View(ViewIntent.SetPanelPin(PanelId.ProjectOutput, Some(PanelPosition.Left)))
    ) shouldBe
      CommandScope(CommandFamily.Code, FrontendSupport.Both)
    CommandScope.of(CommandIntent.View(ViewIntent.SetPanelPin(PanelId.ProjectOutput, None))) shouldBe CommandScope.core
  }
