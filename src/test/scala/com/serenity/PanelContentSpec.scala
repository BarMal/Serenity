package com.serenity

import java.nio.file.Paths

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.presets.UiPreset.PanelContentSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `PanelContent` is a refinement of `SurfaceContent`: every case wraps the exact `SurfaceContent` value it represents,
  * so there is one authoritative payload shape rather than two hand-kept-in-sync enums (issue #1009).
  */
class PanelContentSpec extends AnyFlatSpec with Matchers:

  "PanelContent" should "expose the exact SurfaceContent it wraps, for every pinnable case" in {
    val tree     = DirectoryTreeData(Paths.get("/repo"))
    val location = Location(3, 7)

    PanelContent.DirectoryTree(tree, Some(Paths.get("/repo/a"))).asSurfaceContent shouldBe
      SurfaceContent.DirectoryTree(tree, Some(Paths.get("/repo/a")))
    PanelContent.Terminal("buffer", 4).asSurfaceContent shouldBe SurfaceContent.Terminal("buffer", 4)
    PanelContent.Outline(Nil, Some(location)).asSurfaceContent shouldBe SurfaceContent.Outline(Nil, Some(location))
    PanelContent.Comments(Nil, Some(location)).asSurfaceContent shouldBe SurfaceContent.Comments(Nil, Some(location))
    PanelContent.MarkdownPreview(BufferId(1), "title").asSurfaceContent shouldBe
      SurfaceContent.MarkdownPreview(BufferId(1), "title")
  }

  it should "carry an active diagnostic location through to the underlying surface content" in {
    val location = Location(10, 2)

    PanelContent.Diagnostics(Nil, Some(location)).asSurfaceContent shouldBe
      SurfaceContent.Diagnostics(Nil, Some(location))
    PanelContent.Diagnostics(Nil).asSurfaceContent shouldBe SurfaceContent.Diagnostics(Nil, None)
  }

  it should "recognize every SurfaceContent case that is pinnable" in {
    val tree = DirectoryTreeData(Paths.get("/repo"))

    PanelContent.fromSurfaceContent(SurfaceContent.DirectoryTree(tree)) shouldBe
      Some(PanelContent.DirectoryTree(tree, None))
    PanelContent.fromSurfaceContent(SurfaceContent.Terminal("x", 0)) shouldBe
      Some(PanelContent.Terminal("x", 0))
    PanelContent.fromSurfaceContent(SurfaceContent.Outline(Nil)) shouldBe
      Some(PanelContent.Outline(Nil, None))
    PanelContent.fromSurfaceContent(SurfaceContent.Comments(Nil)) shouldBe
      Some(PanelContent.Comments(Nil, None))
    PanelContent.fromSurfaceContent(SurfaceContent.Diagnostics(Nil)) shouldBe
      Some(PanelContent.Diagnostics(Nil, None))
    PanelContent.fromSurfaceContent(SurfaceContent.MarkdownPreview(BufferId(0), "t")) shouldBe
      Some(PanelContent.MarkdownPreview(BufferId(0), "t"))
  }

  it should "preserve an active diagnostic location when recognizing a pinnable SurfaceContent" in {
    val location = Location(4, 4)

    PanelContent.fromSurfaceContent(SurfaceContent.Diagnostics(Nil, Some(location))) shouldBe
      Some(PanelContent.Diagnostics(Nil, Some(location)))
  }

  it should "reject SurfaceContent that cannot be pinned" in {
    PanelContent.fromSurfaceContent(SurfaceContent.QuickInfo("info")) shouldBe None
  }

  "UiPreset.PanelContentSnapshot" should
    "deliberately drop the transient active-location highlight for Outline, Comments, and Diagnostics when persisting" in {
      val location = Location(1, 1)

      UiPreset.PinnedPanel
        .fromPanelContent(PanelContent.Outline(Nil, Some(location)), PanelPosition.Left, 20)
        .map(_.content) shouldBe Some(PanelContentSnapshot.Outline(Nil))

      UiPreset.PinnedPanel
        .fromPanelContent(PanelContent.Comments(Nil, Some(location)), PanelPosition.Left, 20)
        .map(_.content) shouldBe Some(PanelContentSnapshot.Comments(Nil))

      UiPreset.PinnedPanel
        .fromPanelContent(PanelContent.Diagnostics(Nil, Some(location)), PanelPosition.Bottom, 10)
        .map(_.content) shouldBe Some(PanelContentSnapshot.Diagnostics(Nil))
    }

  it should "restore a diagnostics panel without resurrecting a stale active location" in {
    val location = Location(1, 1)
    val panel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Diagnostics(Nil, Some(location)), PanelPosition.Bottom, 10)
      .getOrElse(fail("diagnostics should be capturable"))

    panel.content.toSurfaceContent shouldBe SurfaceContent.Diagnostics(Nil, None)
  }
