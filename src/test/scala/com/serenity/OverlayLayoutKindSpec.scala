package com.serenity

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirEntry, Layout, LayoutRect}
import com.serenity.ui.renderer.OverlayViewModel

class OverlayLayoutKindSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def listingState: AppState =
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val entries = List(
      DirEntry(Paths.get("/repo/src"), "src", true),
      DirEntry(Paths.get("/repo/test"), "test", true),
      DirEntry(Paths.get("/repo/build.sbt"), "build.sbt", false)
    )

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("listing")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("listing"),
          SurfaceContent.DirectoryListing(Paths.get("/repo"), entries, None),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.AboveCursor),
          dismissOnMove = true
        )
      )
    )

  "OverlayViewModel" should "adapt directory listings for horizontal surfaces" in {
    val layout = com.serenity.ui.layout.CalculatedLayout(
      editorPanelRect = LayoutRect(0, 0, 80, 20),
      leftSpacerRect = LayoutRect(0, 0, 0, 20),
      rightSpacerRect = LayoutRect(80, 0, 0, 20),
      aboveCursorOverlayRect = Some(LayoutRect(10, 2, 48, 6))
    )

    val overlay = OverlayViewModel.fromState(listingState, layout).aboveCursor.get

    overlay.rows.map(_.plainText) shouldBe List("repo  src | test | build.sbt")
  }

  it should "adapt directory listings for vertical surfaces" in {
    val layout = com.serenity.ui.layout.CalculatedLayout(
      editorPanelRect = LayoutRect(0, 0, 80, 20),
      leftSpacerRect = LayoutRect(0, 0, 0, 20),
      rightSpacerRect = LayoutRect(80, 0, 0, 20),
      aboveCursorOverlayRect = Some(LayoutRect(10, 2, 18, 40))
    )

    val overlay = OverlayViewModel.fromState(listingState, layout).aboveCursor.get

    overlay.rows.map(_.plainText) shouldBe List("repo", "src", "test", "build.sbt")
  }
