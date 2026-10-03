package com.serenity.session

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, SurfaceId}
import com.serenity.ui.layout.WorkspaceNodeId
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The companion sprite panel is gone. A session saved while it was docked must still restore: the sprite is simply
  * dropped, and every other docked panel keeps the place the user gave it.
  */
class RemovedCompanionSpriteSessionSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  /** The `layout` object a session saved with the outline docked left and the companion sprite docked right. */
  private val layoutWithDockedSprite =
    """{"editorPanes":[{"id":0,"bufferId":0}],"activeEditorPaneId":0,
      |"workspaceTree":{"Split":{"id":"dock-split-panel-companion-3","axis":"Horizontal","ratio":0.8166666666666667,
      |"first":{"Split":{"id":"dock-split-panel-outline-1","axis":"Horizontal","ratio":0.24489795918367346,
      |"first":{"DockedSurface":{"id":"dock-panel-outline","surfaceId":"panel-outline","position":"Left"}},
      |"second":{"EditorLeaf":{"id":"editor-0","paneId":0}}}},
      |"second":{"DockedSurface":{"id":"dock-panel-companion","surfaceId":"panel-companion","position":"Right"}}}},
      |"maximizedWorkspaceNodeId":null,
      |"dockedPanels":[{"surfaceId":"panel-outline","panel":{"position":"Left","size":24,"content":{"Outline":{"symbols":[]}}}},
      |{"surfaceId":"panel-companion","panel":{"position":"Right","size":22,"content":{"CompanionSprite":{}}}}]}""".stripMargin

  private def restoredFrom(layout: String): AppState =
    val saved     = SessionState.fromAppState(AppState.initial).asJson
    val layoutObj = parse(layout).getOrElse(fail("fixture layout is not JSON"))
    val session   = saved.mapObject(_.add("layout", layoutObj))
    val decoded   = session.as[SessionState].getOrElse(fail("a session with a docked companion sprite must decode"))
    SessionState.toAppState(decoded, Theme.dark)

  "A session saved with the companion sprite docked" should "restore without the sprite" in {
    val restored = restoredFrom(layoutWithDockedSprite)

    restored.pinnedSurfaces.map(_.id) shouldBe List(SurfaceId("panel-outline"))
    restored.persisted.layout.workspaceTree.value.dockedSurfaceIds should not contain SurfaceId("panel-companion")
    restored.isValid shouldBe true
  }

  it should "keep the saved arrangement of the panels that remain" in {
    val restored = restoredFrom(layoutWithDockedSprite)
    val tree     = restored.persisted.layout.workspaceTree.value

    tree.surfaceIdForNode(WorkspaceNodeId("dock-panel-outline")) shouldBe Some(SurfaceId("panel-outline"))
    tree.root.nodeIds should contain(WorkspaceNodeId("dock-split-panel-outline-1"))
  }

  "A UI preset saved with the companion sprite docked" should "still load, without the sprite" in {
    val dockedPanels = parse(
      """[{"surfaceId":"panel-outline","panel":{"position":"Left","size":24,"content":{"Outline":{"symbols":[]}}}},
        |{"surfaceId":"panel-companion","panel":{"position":"Right","size":22,"content":{"CompanionSprite":{}}}}]""".stripMargin
    ).getOrElse(fail("fixture panels are not JSON"))
    val preset = UiPreset(name = "Sprite", config = AppConfig.default, themeName = Theme.dark.name)
    val saved  = preset.asJson.mapObject(_.add("dockedPanels", dockedPanels))

    val loaded = saved.as[UiPreset].getOrElse(fail("a preset with a docked companion sprite must decode"))

    loaded.dockedPanels.map(_.surfaceId) shouldBe List("panel-outline")
  }
