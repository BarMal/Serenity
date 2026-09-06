package com.serenity

import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The panel/placement framework's registration primitive (issue #1310): a panel registers once and declares which
  * display modes it supports, instead of every mode hand-adding a case for it.
  */
class PanelRegistrySpec extends AnyFlatSpec with Matchers:

  private def registrationFor(id: String, modes: Set[PanelDisplayMode]): PanelRegistration =
    PanelRegistration(
      id = PanelId(id),
      label = id,
      description = s"$id panel",
      buildContent = _ => SurfaceContent.QuickInfo(id),
      supportedModes = modes
    )

  "PanelRegistry.empty" should "have no registrations" in {
    PanelRegistry.empty.all shouldBe Nil
    PanelRegistry.empty.get(PanelId("missing")) shouldBe None
  }

  "PanelRegistry" should "look up a registration by its id" in {
    val registration = registrationFor("outline", Set(PanelDisplayMode.Palette))
    val registry     = PanelRegistry(List(registration))

    registry.get(PanelId("outline")) shouldBe Some(registration)
    registry.get(PanelId("missing")) shouldBe None
  }

  it should "list every registration" in {
    val a        = registrationFor("a", Set(PanelDisplayMode.Palette))
    val b        = registrationFor("b", Set(PanelDisplayMode.Corner))
    val registry = PanelRegistry(List(a, b))

    registry.all should contain theSameElementsAs List(a, b)
  }

  it should "filter registrations by the display mode they support" in {
    val paletteOnly = registrationFor("palette-only", Set(PanelDisplayMode.Palette))
    val cornerOnly  = registrationFor("corner-only", Set(PanelDisplayMode.Corner))
    val both        = registrationFor("both", Set(PanelDisplayMode.Palette, PanelDisplayMode.Corner))
    val registry    = PanelRegistry(List(paletteOnly, cornerOnly, both))

    registry.supporting(PanelDisplayMode.Palette) should contain theSameElementsAs List(paletteOnly, both)
    registry.supporting(PanelDisplayMode.Corner) should contain theSameElementsAs List(cornerOnly, both)
    registry.supporting(PanelDisplayMode.Dock) shouldBe Nil
  }

  it should "let a later registration with the same id replace an earlier one" in {
    val first    = registrationFor("dup", Set(PanelDisplayMode.Palette))
    val second   = registrationFor("dup", Set(PanelDisplayMode.Corner))
    val registry = PanelRegistry(List(first, second))

    registry.all shouldBe List(second)
  }
