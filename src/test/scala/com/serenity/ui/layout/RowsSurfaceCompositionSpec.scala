package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `RowsSurfaceComposition` (issue #1683): the generic composition adapter used for every surface content
  * kind that has no bespoke `*SurfaceComposition` object of its own -- it turns whatever `SurfaceContentResolver`
  * already resolved (rows/header/footer/keyHint) into the same `ResolvedSurfaceComposition` shape every composed
  * surface paints through, so `TextPanelView`/`TextOverlayView` need no separate rows-only representation.
  */
class RowsSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private val frameRect = LayoutRect(0, 0, 20, 8)

  "forResolved" should "paint one text box per item row, positioned by the shared row-slot geometry" in {
    val resolved = ResolvedSurfaceContent(rows = List(OverlayRow("one"), OverlayRow("two"), OverlayRow("three")))

    val composition = RowsSurfaceComposition.forResolved(resolved, frameRect)

    val contentRect = SurfaceFrameLayout(frameRect).contentRect
    composition.paintBoxes.map(_.text) shouldBe List(Some("one"), Some("two"), Some("three"))
    composition.paintBoxes.map(_.kind) shouldBe List.fill(3)(SurfacePaintKind.Text)
    composition.paintBoxes.map(_.rect.y) shouldBe List(contentRect.y, contentRect.y + 1, contentRect.y + 2)
  }

  it should "tag a header row as Heading and a footer row as Footer, distinct from plain item rows" in {
    val resolved = ResolvedSurfaceContent(
      header = Some(OverlayRow("head")),
      rows = List(OverlayRow("body")),
      footer = Some(OverlayRow("foot"))
    )

    val composition = RowsSurfaceComposition.forResolved(resolved, frameRect)

    composition.paintBoxes.map(box => box.kind -> box.text) shouldBe List(
      SurfacePaintKind.Heading -> Some("head"),
      SurfacePaintKind.Text    -> Some("body"),
      SurfacePaintKind.Footer  -> Some("foot")
    )
  }

  it should "tag a key-hint row as KeyHint, positioned above the footer" in {
    val resolved = ResolvedSurfaceContent(
      rows = List(OverlayRow("body")),
      keyHintRow = Some(OverlayRow("hint")),
      footer = Some(OverlayRow("foot"))
    )

    val composition = RowsSurfaceComposition.forResolved(resolved, frameRect)

    composition.paintBoxes.map(box => box.kind -> box.text) shouldBe List(
      SurfacePaintKind.Text    -> Some("body"),
      SurfacePaintKind.KeyHint -> Some("hint"),
      SurfacePaintKind.Footer  -> Some("foot")
    )
  }

  it should "carry a row's selection, cursor column, and segments onto its paint box unchanged" in {
    val row = OverlayRow(
      plainText = "value",
      selected = true,
      cursorColumn = Some(3),
      segments = List(OverlaySegment("value")),
      layout = OverlayRowLayout.Columns
    )
    val resolved = ResolvedSurfaceContent(rows = List(row))

    val composition = RowsSurfaceComposition.forResolved(resolved, frameRect)

    val box = composition.paintBoxes.headOption.getOrElse(fail("expected one paint box"))
    box.selected shouldBe true
    box.cursorOffset shouldBe Some(3)
    box.segments shouldBe row.segments
    box.layout shouldBe SurfacePaintLayout.Columns
  }

  it should "emit no hit regions or focus order: generic content is informational, never interactive" in {
    val resolved = ResolvedSurfaceContent(rows = List(OverlayRow("one")))

    val composition = RowsSurfaceComposition.forResolved(resolved, frameRect)

    composition.hitRegions shouldBe Nil
    composition.focusOrder shouldBe Nil
  }

  it should "report an intrinsic size derived from the actual rows/header/footer/key-hint it painted" in {
    val resolved = ResolvedSurfaceContent(
      header = Some(OverlayRow("head")),
      rows = List(OverlayRow("a"), OverlayRow("b")),
      footer = Some(OverlayRow("foot"))
    )

    val composition = RowsSurfaceComposition.forResolved(resolved, frameRect)

    // header + 2 items + footer + 2 border rows (default border), same formula `SurfaceFrameLayout.frameHeightForItemRows`
    // already uses everywhere else.
    composition.intrinsicSize shouldBe SurfaceIntrinsicSize(frameRect.width, 6)
  }

  it should "match frameHeight's standalone height computation with no frame rect involved" in {
    val resolved = ResolvedSurfaceContent(rows = List(OverlayRow("a"), OverlayRow("b"), OverlayRow("c")))

    RowsSurfaceComposition.frameHeight(resolved, borderCells = 1) shouldBe 5
  }

  it should "paint nothing for entirely empty resolved content, with an intrinsic height of just the frame border" in {
    val composition = RowsSurfaceComposition.forResolved(ResolvedSurfaceContent(), frameRect)

    composition.paintBoxes shouldBe Nil
    composition.intrinsicSize shouldBe SurfaceIntrinsicSize(frameRect.width, 2)
  }

  it should "respect a zero-border content kind (e.g. the status line) when placing rows" in {
    val resolved = ResolvedSurfaceContent(rows = List(OverlayRow("12:1")))

    val composition = RowsSurfaceComposition.forResolved(resolved, frameRect, borderCells = 0)

    composition.paintBoxes.headOption.map(_.rect.y) shouldBe Some(frameRect.y)
    composition.intrinsicSize shouldBe SurfaceIntrinsicSize(frameRect.width, 1)
  }

  it should "mark the composition it builds as recoverable via contentRowSlots" in {
    val resolved = ResolvedSurfaceContent(rows = List(OverlayRow("one")))

    RowsSurfaceComposition.forResolved(resolved, frameRect).builtByRowsAdapter shouldBe true
  }

  "contentRowSlots" should "recover header/item/key-hint/footer slots for a composition this adapter built" in {
    val resolved = ResolvedSurfaceContent(
      header = Some(OverlayRow("head")),
      rows = List(OverlayRow("one"), OverlayRow("two")),
      keyHintRow = Some(OverlayRow("hint")),
      footer = Some(OverlayRow("foot"))
    )

    val composition = RowsSurfaceComposition.forResolved(resolved, frameRect)
    val slots       = RowsSurfaceComposition.contentRowSlots(composition)

    slots.map(_.kind) shouldBe List(
      SurfaceContentRowKind.Header,
      SurfaceContentRowKind.Item(0),
      SurfaceContentRowKind.Item(1),
      SurfaceContentRowKind.KeyHint,
      SurfaceContentRowKind.Footer
    )
  }

  it should "stay empty for a composition not built via this adapter, even when it carries real paint boxes" in {
    // Mirrors a bespoke composition (e.g. `CommandRunnerSurfaceComposition`, `OutlineSurfaceComposition`) -- its
    // paint boxes were never tagged by `forResolved`'s header/item/key-hint/footer rule, so inverting them the same
    // way would misreport that content as plain item rows (issue #1683 regression).
    val bespokeComposition = ResolvedSurfaceComposition(
      bounds = LogicalPixelRect(0, 0, 20, 3),
      intrinsicSize = SurfaceIntrinsicSize(20, 3),
      paintBoxes = List(
        SurfacePaintBox(kind = SurfacePaintKind.Text, rect = LogicalPixelRect(0, 0, 20, 1), text = Some("item one")),
        SurfacePaintBox(kind = SurfacePaintKind.ActionItem, rect = LogicalPixelRect(0, 1, 20, 1), text = Some("action"))
      ),
      hitRegions = Nil,
      focusOrder = Nil
    )

    bespokeComposition.builtByRowsAdapter shouldBe false
    RowsSurfaceComposition.contentRowSlots(bespokeComposition) shouldBe Nil
  }
end RowsSurfaceCompositionSpec
