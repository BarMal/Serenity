package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Drop caps are laid out only while `document.drop_caps_enabled` holds, so the layout key and the scene cached under
  * it must follow that setting.
  */
class DropCapsKeySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val viewport = ViewportSize(60, 24)

  private val text = "Hello there this wraps across several lines of body text " * 4

  private def dropCapState(dropCapsEnabled: Boolean): AppState =
    val document = RichTextDocument(List(RichTextParagraph.plain(text, role = ParagraphRole.dropCap(lines = 2))))
    val base     = Buffer.fromString(BufferId(1), document.plainText)
    val buffer = base.copy(
      richText = RichTextState().withSyncedDocument(Some(document), base.document.contentVersion),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 10)
    )
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.default.withDropCapsEnabled(dropCapsEnabled)
      )
    )

  private def firstRowInset(scene: com.serenity.ui.layout.UiSceneSnapshot): Float =
    scene.textSnapshots(paneId).visualLines.head.xOffsetPx

  "Toggling drop caps" should "lay the drop cap paragraph out differently when built from scratch (control)" in {
    val on  = AuthoritativeUiScene().forState(dropCapState(true), viewport)
    val off = AuthoritativeUiScene().forState(dropCapState(false), viewport)

    firstRowInset(on) should be > 0.0f
    firstRowInset(off) shouldBe 0.0f
  }

  it should "change the layout key and rebuild the cached scene when only drop_caps_enabled changes" in {
    val enabled = dropCapState(true)
    val disabled =
      enabled.copy(persisted = enabled.persisted.copy(config = enabled.persisted.config.withDropCapsEnabled(false)))
    val scene = AuthoritativeUiScene()

    val before = MouseTargetCache.fromState(enabled, viewport, scene)
    val after  = MouseTargetCache.fromState(disabled, viewport, scene)

    after.layoutKey should not equal before.layoutKey
    after.scene should not be theSameInstanceAs(before.scene)
    firstRowInset(after.scene) shouldBe 0.0f
  }
