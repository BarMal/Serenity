package com.serenity

import java.nio.file.Path

import com.serenity.command.CommandFamily
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The panel/placement framework's registration primitive (issue #1310): a panel registers once, and every display mode
  * and settings surface reads its identity, label and default placement from that one registration.
  */
class PanelRegistrySpec extends AnyFlatSpec with Matchers:

  "PanelId" should "give every panel a fixed surface id" in {
    PanelId.Explorer.surfaceId shouldBe SurfaceId("panel-explorer")
    PanelId.MarkdownPreview.surfaceId shouldBe SurfaceId("panel-markdown-preview")
    PanelId.values.map(_.surfaceId).distinct should have size PanelId.values.length.toLong
  }

  it should "identify a panel from the content it shows" in {
    PanelId.forContent(SurfaceContent.DirectoryTree(DirectoryTreeData(Path.of(".")))) shouldBe Some(PanelId.Explorer)
    PanelId.forContent(SurfaceContent.Outline(Nil)) shouldBe Some(PanelId.Outline)
    PanelId.forContent(SurfaceContent.Comments(Nil)) shouldBe Some(PanelId.Comments)
    PanelId.forContent(SurfaceContent.Diagnostics(Nil)) shouldBe Some(PanelId.Diagnostics)
    PanelId.forContent(SurfaceContent.MarkdownPreview(BufferId(0), "notes.md")) shouldBe Some(PanelId.MarkdownPreview)
    PanelId.forContent(SurfaceContent.Terminal("done", 0)) shouldBe Some(PanelId.ProjectOutput)
    PanelId.forContent(SurfaceContent.QuickInfo("hover")) shouldBe None
  }

  it should "name the panel for every kind of dockable content" in {
    PanelId.of(PanelContent.Terminal("done", 0)) shouldBe PanelId.ProjectOutput
    PanelId.of(PanelContent.Outline(Nil)) shouldBe PanelId.Outline
  }

  "PanelRegistry.default" should "register every panel" in {
    PanelRegistry.default.all.map(_.id) should contain theSameElementsAs PanelId.values.toList
  }

  it should "dock every panel and offer none through the palette" in {
    PanelRegistry.default.supporting(PanelDisplayMode.Dock).map(_.id) should contain theSameElementsAs
      PanelId.values.toList
    PanelRegistry.default.supporting(PanelDisplayMode.Palette) shouldBe Nil
  }

  "PanelRegistry.registrationFor" should "pin each panel to its default edge" in {
    PanelId.values.map(id => id -> PanelRegistry.registrationFor(id).defaultPosition).toMap shouldBe Map(
      PanelId.Explorer        -> PanelPosition.Left,
      PanelId.Outline         -> PanelPosition.Right,
      PanelId.Comments        -> PanelPosition.Right,
      PanelId.Diagnostics     -> PanelPosition.Bottom,
      PanelId.MarkdownPreview -> PanelPosition.Right,
      PanelId.ProjectOutput   -> PanelPosition.Bottom
    )
  }

  it should "size project output as it was sized before it was registered" in {
    PanelRegistry.registrationFor(PanelId.ProjectOutput).defaultSize(PanelPosition.Bottom) shouldBe 14
  }

  it should "put the language-tooling panels in the code family, and the rest everywhere" in {
    PanelId.values.filter(id => PanelRegistry.registrationFor(id).family == CommandFamily.Code).toSet shouldBe
      Set(PanelId.Diagnostics, PanelId.ProjectOutput)
    PanelId.values.map(id => PanelRegistry.registrationFor(id).family).toSet shouldBe
      Set(CommandFamily.Code, CommandFamily.Core)
  }

  it should "size side-docked panels wider than top/bottom ones, and the Markdown preview wider still" in {
    val outline = PanelRegistry.registrationFor(PanelId.Outline)
    outline.defaultSize(PanelPosition.Left) shouldBe 30
    outline.defaultSize(PanelPosition.Bottom) shouldBe 10

    val preview = PanelRegistry.registrationFor(PanelId.MarkdownPreview)
    PanelPosition.values.map(preview.defaultSize).distinct shouldBe Array(40)
  }

  it should "label each panel for the settings and command surfaces" in {
    PanelRegistry.registrationFor(PanelId.MarkdownPreview).label shouldBe "Markdown Preview"
    PanelId.values.map(id => PanelRegistry.registrationFor(id).label).forall(_.nonEmpty) shouldBe true
  }

  "PanelRegistry" should "let a later registration with the same id replace an earlier one" in {
    val first    = PanelRegistry.registrationFor(PanelId.Outline)
    val second   = first.copy(label = "Symbols")
    val registry = PanelRegistry(List(first, second))

    registry.get(PanelId.Outline) shouldBe Some(second)
    registry.all shouldBe List(second)
  }

  it should "have no registrations when empty" in {
    PanelRegistry.empty.all shouldBe Nil
    PanelRegistry.empty.get(PanelId.Outline) shouldBe None
  }
