package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.openings.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Chapter-opening settings reach layout through the config and through the buffer's own overrides; neither is a field
  * of the layout key by itself, so a change has to show in the key or a cached scene outlives it.
  */
class OpeningsLayoutInvalidationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("Title", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("Body text under the title.")
    )
  )

  private val baseBuffer: Buffer =
    Buffer
      .fromString(bufferId, document.plainText)
      .copy(richText = RichTextState(richTextDocument = Some(document)))

  private def stateOf(config: AppConfig, overrides: OpeningOverride = OpeningOverride.Empty): AppState =
    val buffer = baseBuffer.copy(document = baseBuffer.document.copy(openings = overrides))
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

  private def keyFor(config: AppConfig, overrides: OpeningOverride = OpeningOverride.Empty): MouseTargetLayoutKey =
    AuthoritativeUiScene().layoutKeyFor(stateOf(config, overrides), viewport)

  private def configured(update: OpeningSettings => OpeningSettings): AppConfig =
    AppConfig.default.copy(openingsConfig = update(OpeningSettings.default.copy(enabled = true)))

  "The mouse-target layout key" should "stay put while openings are off, whatever else is set" in {
    val base = keyFor(AppConfig.default)

    keyFor(
      AppConfig.default.copy(openingsConfig =
        OpeningSettings.default.copy(letterMode = LetterMode.Raised, smallCapsFirstLine = true)
      )
    ) shouldBe base
    keyFor(AppConfig.default, OpeningOverride(letterMode = Some(LetterMode.Hybrid))) shouldBe base
  }

  it should "change when openings are turned on, globally or for the document" in {
    val base = keyFor(AppConfig.default)

    keyFor(configured(identity)) should not be base
    keyFor(AppConfig.default, OpeningOverride(enabled = Some(true))) should not be base
    keyFor(AppConfig.default, OpeningOverride(enabled = Some(true))) shouldBe keyFor(configured(identity))
  }

  it should "change with each global setting once openings are on" in {
    val base = keyFor(configured(identity))

    List(
      "letter"    -> configured(_.copy(letterMode = LetterMode.Raised)),
      "raisedRow" -> configured(_.copy(raisedRow = RaisedRowRule.Clamp)),
      "sink"      -> configured(_.copy(hybridSinkLines = 4)),
      "smallCaps" -> configured(_.copy(smallCapsFirstLine = true)),
      "marker"    -> configured(_.copy(epigraphMarker = "motto")),
      "epigraph"  -> configured(settings => settings.copy(rules = settings.rules.copy(epigraph = EpigraphRule.Never))),
      "scene" -> configured(settings =>
        settings.copy(rules = settings.rules.copy(sceneBreak = SceneBreakRule.FullOpening))
      ),
      "hand" -> configured(settings =>
        settings.copy(rules = settings.rules.copy(handDropCap = HandDropCapRule.OpeningWins))
      )
    ).foreach { case (what, config) => withClue(what)(keyFor(config) should not be base) }
  }

  it should "change with each of the document's overrides once openings are on" in {
    val on   = configured(identity)
    val base = keyFor(on)

    List(
      "letter"    -> OpeningOverride(letterMode = Some(LetterMode.Hybrid)),
      "raisedRow" -> OpeningOverride(raisedRow = Some(RaisedRowRule.Clamp)),
      "sink"      -> OpeningOverride(hybridSinkLines = Some(5)),
      "smallCaps" -> OpeningOverride(smallCapsFirstLine = Some(true)),
      "marker"    -> OpeningOverride(epigraphMarker = Some("motto")),
      "epigraph"  -> OpeningOverride(epigraph = Some(EpigraphRule.Never)),
      "scene"     -> OpeningOverride(sceneBreak = Some(SceneBreakRule.SmallCapsOnly)),
      "hand"      -> OpeningOverride(handDropCap = Some(HandDropCapRule.OpeningWins)),
      "off"       -> OpeningOverride(enabled = Some(false))
    ).foreach { case (what, overrides) => withClue(what)(keyFor(on, overrides) should not be base) }
  }

  it should "not change when a document override restates the global setting" in {
    val on = configured(_.copy(letterMode = LetterMode.Raised))

    keyFor(on, OpeningOverride(letterMode = Some(LetterMode.Raised))) shouldBe keyFor(on)
  }

  "The prepared scene" should "be rebuilt when a document override changes, and reused when nothing did" in {
    val scene     = AuthoritativeUiScene()
    val on        = configured(identity)
    val before    = MouseTargetCache.fromState(stateOf(on), viewport, scene).scene
    val unchanged = MouseTargetCache.fromState(stateOf(on), viewport, scene).scene
    val after =
      MouseTargetCache
        .fromState(stateOf(on, OpeningOverride(letterMode = Some(LetterMode.Raised))), viewport, scene)
        .scene

    unchanged should be theSameInstanceAs before
    after should not be theSameInstanceAs(before)
  }
