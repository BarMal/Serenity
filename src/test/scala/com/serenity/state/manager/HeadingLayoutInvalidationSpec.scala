package com.serenity.state.manager

import com.serenity.command.{
  CommandIntent,
  CommandRegistry,
  CommandRunner,
  CommandSurfaceItem,
  HeadingsConfigEdit,
  SettingsIntent
}
import com.serenity.config.{AppConfig, HeadingLevelConfig, HeadingsConfig}
import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.{FontConfig, TextScaleMode}
import com.serenity.ui.layout.{Layout, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeadingLayoutInvalidationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)

  private def document(headingAlignment: ParagraphAlignment = ParagraphAlignment.Left) =
    RichTextDocument(
      List(
        RichTextParagraph.plain("Title", alignment = headingAlignment, role = ParagraphRole.Heading(1)),
        RichTextParagraph.plain("Body text under the title.")
      )
    )

  private def bufferOf(richText: RichTextDocument): Buffer =
    Buffer.fromString(bufferId, richText.plainText).copy(richText = RichTextState(richTextDocument = Some(richText)))

  // One buffer per document, so two states differ only in their config and share the buffer's rope identity.
  private val defaultBuffer = bufferOf(document())
  private val rightBuffer   = bufferOf(document(ParagraphAlignment.Right))

  private def stateWith(config: AppConfig, aligned: ParagraphAlignment = ParagraphAlignment.Left): AppState =
    val buffer = if aligned == ParagraphAlignment.Right then rightBuffer else defaultBuffer
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

  private def withH1(update: HeadingLevelConfig => HeadingLevelConfig): AppConfig =
    AppConfig.default.withHeadingsConfig(HeadingsConfig().withLevel(1)(update))

  private def keyFor(config: AppConfig): MouseTargetLayoutKey =
    AuthoritativeUiScene().layoutKeyFor(stateWith(config), viewport)

  private def titleRow(config: AppConfig, aligned: ParagraphAlignment = ParagraphAlignment.Left) =
    val scene = AuthoritativeUiScene()
    val state = stateWith(config, aligned)
    MouseTargetCache
      .fromState(state, viewport, scene)
      .scene
      .textSnapshot(paneId)
      .flatMap(_.visualLines.headOption)
      .getOrElse(fail("expected a prepared title row"))

  "The mouse-target layout key" should "change when a heading's size, weight, slant, family or alignment changes" in {
    val base = keyFor(AppConfig.default)

    List(
      "size"   -> withH1(_.copy(size = Some(3.0f))),
      "bold"   -> withH1(_.copy(bold = Some(false))),
      "italic" -> withH1(_.copy(italic = Some(true))),
      "family" -> withH1(_.copy(family = Some("Serif"))),
      "align"  -> withH1(_.copy(align = Some(ParagraphAlignment.Center)))
    ).foreach { case (what, config) => withClue(what)(keyFor(config) should not be base) }
  }

  it should "stay put when a damping setting changes nothing at the current body size" in {
    val base = keyFor(AppConfig.default)

    keyFor(AppConfig.default.withHeadingsConfig(HeadingsConfig(damping = false))) shouldBe base
    keyFor(AppConfig.default.withHeadingsConfig(HeadingsConfig(dampingThresholdPt = 40.0f))) shouldBe base
  }

  it should "change with the damping settings once the logical body passes the threshold" in {
    val big = AppConfig.default.withFontConfig(
      FontConfig(textFontSize = 12.0f, textScaleMode = TextScaleMode.Manual, textScaleMultiplier = 3.0)
    )
    val damped = keyFor(big)

    keyFor(big.withHeadingsConfig(HeadingsConfig(damping = false))) should not be damped
    keyFor(big.withHeadingsConfig(HeadingsConfig(dampingThresholdPt = 60.0f))) should not be damped
  }

  "The prepared scene" should "be rebuilt when a heading size changes, and reused when nothing did" in {
    val scene     = AuthoritativeUiScene()
    val before    = MouseTargetCache.fromState(stateWith(AppConfig.default), viewport, scene).scene
    val unchanged = MouseTargetCache.fromState(stateWith(AppConfig.default), viewport, scene).scene
    val after     = MouseTargetCache.fromState(stateWith(withH1(_.copy(size = Some(3.0f)))), viewport, scene).scene

    unchanged should be theSameInstanceAs before
    after should not be theSameInstanceAs(before)
  }

  it should "measure a taller heading row after the size grows, and leave the body row alone" in {
    val plain  = titleRow(AppConfig.default)
    val larger = titleRow(withH1(_.copy(size = Some(3.0f))))

    larger.heightPx should be > plain.heightPx
    larger.widthPx should be > plain.widthPx
  }

  it should "place a heading by its configured alignment" in {
    val left     = titleRow(AppConfig.default)
    val centred  = titleRow(withH1(_.copy(align = Some(ParagraphAlignment.Center))))
    val rightSet = titleRow(withH1(_.copy(align = Some(ParagraphAlignment.Right))))

    left.xOffsetPx shouldBe 0.0f
    centred.xOffsetPx should be > 0.0f
    rightSet.xOffsetPx should be > centred.xOffsetPx
  }

  it should "let a paragraph's own alignment win over its heading style" in {
    val config   = withH1(_.copy(align = Some(ParagraphAlignment.Center)))
    val centred  = titleRow(config)
    val ownRight = titleRow(config, ParagraphAlignment.Right)

    ownRight.xOffsetPx should be > centred.xOffsetPx
  }

  it should "leave body paragraphs unaligned by a heading alignment" in {
    val scene = AuthoritativeUiScene()
    val state = stateWith(withH1(_.copy(align = Some(ParagraphAlignment.Center))))
    val rows = MouseTargetCache
      .fromState(state, viewport, scene)
      .scene
      .textSnapshot(paneId)
      .map(_.visualLines)
      .getOrElse(fail("expected prepared rows"))

    rows.filter(_.bufferLine == 1).map(_.xOffsetPx).distinct shouldBe List(0.0f)
  }

  "A heading size chosen in the settings palette" should "make the heading row taller once applied" in {
    val sizeRow = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .submenuItems("settings-heading-h1")
      .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "heading-h1-size" => item }
      .getOrElse(fail("expected the H1 size row"))
    val chosen = sizeRow.options
      .find(_.label == "2.0")
      .map(_.intent)
      .getOrElse(fail("expected a 2.0 size option"))
    val applied = chosen match
      case CommandIntent.Settings(SettingsIntent.Headings(intent)) =>
        AppConfig.default.withHeadingsConfig(HeadingsConfigEdit(intent)(AppConfig.default.headingsConfig))
      case other => fail(s"unexpected intent $other")

    titleRow(applied).heightPx should be > titleRow(AppConfig.default).heightPx
  }
