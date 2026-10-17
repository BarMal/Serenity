package com.serenity.state.manager

import com.serenity.config.{AppConfig, ConfigRegistry, LineNumberLayout, LineNumberSide}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.ConfigGenerators
import com.serenity.ui.layout.{Layout, UiSceneSnapshot, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A layout-affecting setting must reach the layout key: a key it leaves equal serves the scene cached before the
  * change. Every changed state here is the original with only its config replaced, so the buffer's rope identity cannot
  * make the key differ by itself.
  */
class ConfigLayoutKeySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val viewport = ViewportSize(60, 24)

  private val body = "\tHello there\tthis wraps across several lines of body text " * 4

  private val viewportOf = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 10)

  private def stateOf(buffer: Buffer, config: AppConfig): AppState =
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
        config = config
      )
    )

  private def codeState(config: AppConfig): AppState =
    stateOf(Buffer.fromString(BufferId(1), body).copy(viewport = viewportOf), config)

  private def proseState(config: AppConfig): AppState =
    val document = RichTextDocument(
      List(
        RichTextParagraph.plain("Title", role = ParagraphRole.Heading(1)),
        RichTextParagraph.plain(body, role = ParagraphRole.dropCap(lines = 2))
      )
    )
    val base = Buffer.fromString(BufferId(1), document.plainText)
    val buffer = base.copy(
      richText = RichTextState().withSyncedDocument(Some(document), base.document.contentVersion),
      viewport = viewportOf
    )
    stateOf(buffer, config)

  private def onTerminal(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(capabilities = FrontendCapabilities.tui()))

  private def withConfig(state: AppState, config: AppConfig): AppState =
    state.copy(persisted = state.persisted.copy(config = config))

  private val lineNumbered = AppConfig.default.withLineNumbers(true).withWordWrap(true)

  private def geometry(scene: UiSceneSnapshot) =
    (
      scene.calculatedLayout,
      scene.paneLayouts,
      scene.textSnapshots,
      scene.columnSnapshots,
      scene.paneScrolls.view.mapValues(_.extent).toMap
    )

  private def assertWarmMatchesFresh(original: AppState, changed: AppState): Unit =
    val warm      = AuthoritativeUiScene()
    val before    = MouseTargetCache.fromState(original, viewport, warm)
    val after     = MouseTargetCache.fromState(changed, viewport, warm)
    val fresh     = AuthoritativeUiScene().forState(changed, viewport)
    val unchanged = geometry(before.scene) == geometry(fresh)

    withClue("the change must alter the layout for this test to mean anything: ")(unchanged shouldBe false)
    after.layoutKey should not equal before.layoutKey
    geometry(after.scene) shouldBe geometry(fresh)

  "A tab size change" should "change the layout key and rebuild the cached scene" in {
    val original =
      onTerminal(codeState(lineNumbered.withIndentation(IndentationSettings(tabSize = 2, insertSpaces = false))))
    val changed = withConfig(
      original,
      original.persisted.config.withIndentation(IndentationSettings(tabSize = 8, insertSpaces = false))
    )

    assertWarmMatchesFresh(original, changed)
  }

  "A line number layout change" should "change the layout key and rebuild the cached scene" in {
    val original = codeState(lineNumbered)
    val changed = withConfig(
      original,
      original.persisted.config.withLineNumberLayout(LineNumberLayout(LineNumberSide.Both, marginRight = 3))
    )

    assertWarmMatchesFresh(original, changed)
  }

  private val otherConfigs: List[AppConfig] =
    (1L to 16L).toList.flatMap(n => ConfigGenerators.genAppConfig.apply(Gen.Parameters.default, Seed(n)))

  private val columnMode = lineNumbered.withColumnMode(true)

  private val representatives: List[(String, AppState)] =
    List(
      "code"              -> codeState(lineNumbered),
      "prose"             -> proseState(lineNumbered),
      "code in columns"   -> codeState(columnMode),
      "prose in columns"  -> proseState(columnMode),
      "code on terminal"  -> onTerminal(codeState(lineNumbered)),
      "prose on terminal" -> onTerminal(proseState(lineNumbered))
    )

  "Every registered setting" should "leave a warm scene agreeing with a fresh one when it changes" in {
    otherConfigs should not be empty
    val stale = for
      (name, original) <- representatives
      field            <- ConfigRegistry.fields
      other            <- otherConfigs
      changed = withConfig(original, field.set(original.persisted.config, field.get(other)))
      if changed.persisted.config != original.persisted.config
      warm = AuthoritativeUiScene()
      _    = warm.forState(original, viewport)
      if geometry(warm.forState(changed, viewport)) != geometry(AuthoritativeUiScene().forState(changed, viewport))
    yield s"${field.key} on $name"

    stale.distinct shouldBe empty
  }
