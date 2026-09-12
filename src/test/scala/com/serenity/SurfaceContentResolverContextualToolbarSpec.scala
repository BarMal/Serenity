package com.serenity

import com.serenity.config.*
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverContextualToolbarSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def singlePaneLayout(paneId: PaneId, bufferId: BufferId): Layout =
    Layout(
      editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
      activeEditorPaneId = Some(paneId),
      workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
    )

  "SurfaceContentResolver" should "wrap contextual toolbar items across multiple rows and mark active rich-text formatting" in {
    val bufferId = BufferId(0)
    val selection = Selection(
      anchor = CursorPosition(0, 6),
      focus = CursorPosition(0, 10)
    )
    val richDocument = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("alpha "),
            RichTextRun(
              "beta",
              RichTextStyle(
                marks = Set(InlineMark.Bold),
                fontFamily = Some("Serif"),
                fontSize = Some(18.0f),
                color = Some("#336699")
              )
            )
          ),
          alignment = ParagraphAlignment.Center,
          role = ParagraphRole.Heading(1)
        )
      )
    ).normalized
    val paneId = PaneId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(
          bufferId -> Buffer
            .fromString(bufferId, "alpha beta")
            .copy(
              editing = EditingState(selection = Some(selection), cursors = List(selection.focus)),
              richText = RichTextState(richTextDocument = Some(richDocument))
            )
        ),
        bufferOrder = List(bufferId),
        layout = singlePaneLayout(paneId, bufferId),
        focus = Focus.EditorPane(paneId)
      )
    )
    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      ContextualToolbarState(),
      state,
      LayoutRect(0, 0, 24, 8),
      SurfaceRenderMode.Floating
    )

    resolved.rows.length should be > 1
    resolved.rows.foreach(_.layout shouldBe OverlayRowLayout.Distributed)
    val segments = resolved.rows.flatMap(_.segments)
    segments.find(_.text.contains("Bold")).map(_.selected).shouldBe(Some(true))
    segments.exists(_.text == "Font Serif").shouldBe(true)
    segments.exists(_.text == "Family Serif").shouldBe(true)
    segments.exists(_.text == "Size 18").shouldBe(true)
    segments.exists(_.text == "Color Blue").shouldBe(true)
    segments.exists(_.text == "Hex #336699").shouldBe(true)
    segments.exists(_.text == "Role H1").shouldBe(true)
    segments.exists(_.text.contains("Center")).shouldBe(true)
  }

  it should "retain icon-font runs alongside labels in IconAndText toolbars" in {
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconAndText)
      )
    )

    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      ContextualToolbarState(displayMode = ToolbarDisplayMode.IconAndText),
      state,
      LayoutRect(0, 0, 80, 8),
      SurfaceRenderMode.Floating
    )

    val toolbarItems = ContextualToolbar.itemsFor(state)
    val segments     = resolved.rows.flatMap(_.segments)
    segments.map(_.text) shouldBe toolbarItems.map(item =>
      ContextualToolbar.displayText(item, ToolbarDisplayMode.TextOnly)
    )
    segments.zip(toolbarItems).foreach {
      case (segment, item) =>
        segment.inlineIcon.shouldBe(Some(item.icon))
        segment.inlineIconFontFamily.shouldBe(Some(FontLoader.ToolbarIconFontFamily))
    }
  }

  it should "show heading level 4 in the contextual toolbar role control" in {
    val bufferId = BufferId(0)
    val selection = Selection(
      anchor = CursorPosition(0, 6),
      focus = CursorPosition(0, 10)
    )
    val richDocument = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("alpha "),
            RichTextRun(
              "beta",
              RichTextStyle(fontFamily = Some("Serif"), fontSize = Some(18.0f), color = Some("#336699"))
            )
          ),
          role = ParagraphRole.Heading(4)
        )
      )
    ).normalized
    val paneId = PaneId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(
          bufferId -> Buffer
            .fromString(bufferId, "alpha beta")
            .copy(
              editing = EditingState(selection = Some(selection), cursors = List(selection.focus)),
              richText = RichTextState(richTextDocument = Some(richDocument))
            )
        ),
        bufferOrder = List(bufferId),
        layout = singlePaneLayout(paneId, bufferId),
        focus = Focus.EditorPane(paneId)
      )
    )
    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      ContextualToolbarState(),
      state,
      LayoutRect(0, 0, 24, 8),
      SurfaceRenderMode.Floating
    )

    resolved.rows.flatMap(_.segments).exists(_.text == "Role H4").shouldBe(true)
  }

end SurfaceContentResolverContextualToolbarSpec
