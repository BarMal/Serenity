package com.serenity.ui.layout

import com.serenity.TestWorkspaceTrees
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `ContextualToolbarContentResolver` (issue #1421). `SurfaceContentResolverSpec` covers
  * some of this indirectly through `SurfaceContentResolver.resolveContextualToolbar`, but never the dropdown/input
  * detail-row branches, and never names this object directly. Lives in this package because the resolver is
  * `private[layout]`.
  */
class ContextualToolbarContentResolverSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def proseState: AppState =
    val bufferId = BufferId(0)
    val paneId   = PaneId(0)
    val selection = Selection(
      anchor = CursorPosition(0, 0),
      focus = CursorPosition(0, 4)
    )
    val document = RichTextDocument(
      List(RichTextParagraph(runs = List(RichTextRun("alpha beta"))))
    ).normalized
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(
          bufferId -> Buffer
            .fromString(bufferId, "alpha beta")
            .copy(
              editing = EditingState(selection = Some(selection), cursors = List(selection.focus)),
              richText = RichTextState(richTextDocument = Some(document))
            )
        ),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  "resolve" should "wrap toolbar items across multiple Distributed rows when the rect is narrow" in {
    val resolved = ContextualToolbarContentResolver.resolve(
      ContextualToolbarState(),
      proseState,
      LayoutRect(0, 0, 24, 8),
      SurfaceRenderMode.Floating
    )

    resolved.rows.length should be > 1
    resolved.rows.foreach(_.layout shouldBe OverlayRowLayout.Distributed)
  }

  it should "mark the focused item's segment as selected" in {
    val resolved = ContextualToolbarContentResolver.resolve(
      ContextualToolbarState(focusedIndex = 0),
      proseState,
      LayoutRect(0, 0, 80, 8),
      SurfaceRenderMode.Floating
    )

    val allSegments = resolved.rows.flatMap(_.segments)
    allSegments.count(_.selected) should be >= 1
  }

  it should "carry no extra detail rows when no dropdown or input is open" in {
    val closed = ContextualToolbarContentResolver.resolve(
      ContextualToolbarState(),
      proseState,
      LayoutRect(0, 0, 80, 8),
      SurfaceRenderMode.Floating
    )
    val openInput = ContextualToolbarContentResolver.resolve(
      ContextualToolbarState(detailState = Some(ContextualToolbarDetailState.Input("font-family-text", "x"))),
      proseState,
      LayoutRect(0, 0, 80, 8),
      SurfaceRenderMode.Floating
    )

    // Opening an input detail appends exactly one detail row (the shared inputRow) on top of the same top-level
    // item rows -- closing it must leave nothing behind.
    openInput.rows.size shouldBe closed.rows.size + 1
    closed.rows.forall(_.layout == OverlayRowLayout.Distributed) shouldBe true
  }

  it should "render dropdown options as Distributed detail rows, marking the selected option" in {
    val rect          = LayoutRect(0, 0, 80, 8)
    val toolbarState  = ContextualToolbarState(detailState = Some(ContextualToolbarDetailState.Dropdown("font-family", 0)))
    val closed        = ContextualToolbarContentResolver.resolve(ContextualToolbarState(), proseState, rect, SurfaceRenderMode.Floating)
    val resolved      = ContextualToolbarContentResolver.resolve(toolbarState, proseState, rect, SurfaceRenderMode.Floating)

    val items    = ContextualToolbar.itemsFor(proseState)
    val dropdown = ContextualToolbar.dropdownItem("font-family", items).getOrElse(fail("Expected a font-family dropdown"))
    val expectedFirstOption = dropdown.optionItem.options.headOption.map(_.label)

    // The installed-font option list is host-dependent (as many entries as system font families) and can wrap into
    // more than one Distributed detail row, so search across every row appended beyond the closed-state's top-level
    // item rows rather than assuming a single trailing row.
    val detailRows = resolved.rows.drop(closed.rows.size)
    detailRows should not be empty
    detailRows.foreach(_.layout shouldBe OverlayRowLayout.Distributed)

    val selectedSegments = detailRows.flatMap(_.segments).filter(_.selected)
    selectedSegments.map(_.text) shouldBe expectedFirstOption.toList
  }

  it should "render an open input detail through the shared CommandPaletteContentResolver.inputRow renderer" in {
    val toolbarState = ContextualToolbarState(
      detailState = Some(ContextualToolbarDetailState.Input("font-family-text", "Custom Family"))
    )

    val resolved = ContextualToolbarContentResolver.resolve(
      toolbarState,
      proseState,
      LayoutRect(0, 0, 80, 8),
      SurfaceRenderMode.Floating
    )

    val detailRow = resolved.rows.last
    detailRow.plainText should include("Custom Family")
    detailRow.selected shouldBe true
    detailRow.segments.exists(_.text == "Custom Family") shouldBe true
  }

  it should "add a trailing separator between adjacent items from different formatting groups" in {
    val resolved = ContextualToolbarContentResolver.resolve(
      ContextualToolbarState(),
      proseState,
      LayoutRect(0, 0, 200, 8),
      SurfaceRenderMode.Floating
    )

    val boldSegment = resolved.rows.flatMap(_.segments).find(_.text.contains("Bold"))
    boldSegment.map(_.trailingSeparator) shouldBe Some(false)
    // "underline" is the last item of formatting group 0 before "font-family" (group 1) -- it must carry the
    // group-boundary separator (ContextualToolbar.hasTrailingGroupSeparator).
    val underlineSegment = resolved.rows.flatMap(_.segments).find(_.text.contains("Underline"))
    underlineSegment.map(_.trailingSeparator) shouldBe Some(true)
  }
