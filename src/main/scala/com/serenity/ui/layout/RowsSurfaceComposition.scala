package com.serenity.ui.layout

/** Generic composition adapter for surface content that has no bespoke `*SurfaceComposition` object of its own (issue
  * #1683). Turns whatever `SurfaceContentResolver.resolve` already produced (rows/header/footer/keyHint) into the same
  * `ResolvedSurfaceComposition` shape every composed surface (`ContextMenuSurfaceComposition`,
  * `CommandRunnerSurfaceComposition`, ...) already paints through, so `TextPanelView`/`TextOverlayView` need no second,
  * independently-settable rows/header/footer representation alongside `composition` -- every surface's content,
  * composed or not, is exactly one `ResolvedSurfaceComposition`.
  *
  * Positions rows via the same `SurfaceFrameLayout.contentRowSlotsFor` geometry the pre-migration rows-only renderers
  * used, so this is a pure re-shaping of already-resolved content, not a second, independently-derived layout.
  * Non-interactive throughout: none of the content kinds this adapter serves (informational panels like `QuickInfo`,
  * `FilePreview`, `ShortcutsHelp`, ...) are focus-navigable or mouse-hit-tested through their composition today, so
  * `hitRegions`/`focusOrder` are always empty here -- unlike a bespoke composition (e.g.
  * `ContextMenuSurfaceComposition`), which builds real hit regions for its selectable rows.
  */
object RowsSurfaceComposition:

  /** The frame height `resolved`'s rows/header/footer/key-hint actually need, before any caller-side floor/maxHeight
    * clamp -- the same `SurfaceFrameLayout.frameHeightForItemRows` computation every bespoke composition's own
    * `frameHeight` uses, just driven by real resolved row counts instead of a per-content-kind formula.
    */
  def frameHeight(
    resolved: ResolvedSurfaceContent,
    borderCells: Int = SurfaceFrameLayout.DefaultBorderCells,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1
  ): Int =
    SurfaceFrameLayout.frameHeightForItemRows(
      itemRows = resolved.rows.length,
      hasHeader = resolved.header.nonEmpty,
      hasFooter = resolved.footer.nonEmpty,
      borderCells = borderCells,
      itemGapRows = itemGapRows,
      itemTargetRows = itemTargetRows,
      hasKeyHint = resolved.keyHintRow.nonEmpty
    )

  def forResolved(
    resolved: ResolvedSurfaceContent,
    frameRect: LayoutRect,
    borderCells: Int = SurfaceFrameLayout.DefaultBorderCells,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    // An already-known content rect, e.g. one a caller derived some other way than the plain border inset --
    // mirrors `TextPanelView`/`TextOverlayView`'s own long-standing `contentRect` override.
    contentRectOverride: Option[LayoutRect] = None
  ): ResolvedSurfaceComposition =
    val contentRect = contentRectOverride.getOrElse(SurfaceFrameLayout(frameRect, borderCells).contentRect)
    val bounds      = LogicalPixelRect(contentRect.x, contentRect.y, contentRect.width, contentRect.height)
    val slots = SurfaceFrameLayout.contentRowSlotsFor(
      contentRect,
      resolved.rows.length,
      resolved.header.nonEmpty,
      resolved.footer.nonEmpty,
      itemGapRows,
      itemTargetRows,
      resolved.keyHintRow.nonEmpty
    )
    // `slots`' own `y: Int` floors a fractional `itemGapRows` to a whole cell row -- fine for the row-slot lookups
    // `contentRowSlots` and mouse hit-testing need, but painting needs the true fractional offset (mirroring the
    // pre-migration `FloatingSurfaceGeometry.fromCells` item-rect math), or a sub-row gap renders as no gap at all.
    val itemRowHeight   = math.max(1, itemTargetRows) + math.max(0.0, itemGapRows)
    val itemRowsStartAt = if resolved.header.nonEmpty then 1 else 0
    val boxes = slots.flatMap { slot =>
      rowFor(slot.kind, resolved).map { row =>
        val rowOffset = slot.kind match
          case SurfaceContentRowKind.Item(index) => itemRowsStartAt + index * itemRowHeight
          case _                                 => (slot.y - contentRect.y).toDouble
        rowBox(slot.kind, row, rowRect(bounds, rowOffset))
      }
    }
    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize =
        SurfaceIntrinsicSize(frameRect.width, frameHeight(resolved, borderCells, itemGapRows, itemTargetRows)),
      paintBoxes = boxes,
      hitRegions = Nil,
      focusOrder = Nil,
      builtByRowsAdapter = true
    )

  private def rowFor(kind: SurfaceContentRowKind, resolved: ResolvedSurfaceContent): Option[OverlayRow] =
    kind match
      case SurfaceContentRowKind.Header      => resolved.header
      case SurfaceContentRowKind.Item(index) => resolved.rows.lift(index)
      case SurfaceContentRowKind.KeyHint     => resolved.keyHintRow
      case SurfaceContentRowKind.Footer      => resolved.footer

  private def rowBox(kind: SurfaceContentRowKind, row: OverlayRow, rect: LogicalPixelRect): SurfacePaintBox =
    SurfacePaintBox(
      kind = paintKindFor(kind),
      rect = rect,
      text = Some(row.plainText),
      selected = row.selected,
      cursorOffset = row.cursorColumn,
      segments = row.segments,
      layout = mapLayout(row.layout)
    )

  private def paintKindFor(kind: SurfaceContentRowKind): SurfacePaintKind =
    kind match
      case SurfaceContentRowKind.Header  => SurfacePaintKind.Heading
      case SurfaceContentRowKind.Footer  => SurfacePaintKind.Footer
      case SurfaceContentRowKind.KeyHint => SurfacePaintKind.KeyHint
      case SurfaceContentRowKind.Item(_) => SurfacePaintKind.Text

  private def mapLayout(layout: OverlayRowLayout): SurfacePaintLayout =
    layout match
      case OverlayRowLayout.Plain       => SurfacePaintLayout.Plain
      case OverlayRowLayout.Split       => SurfacePaintLayout.Split
      case OverlayRowLayout.Columns     => SurfacePaintLayout.Columns
      case OverlayRowLayout.Distributed => SurfacePaintLayout.Distributed
      // No distinct paint-side layout exists yet for the priority-columns variant -- none of the content kinds this
      // generic adapter serves ever produce it (it's `ContextualToolbarContentResolver`-only, and that content is
      // always painted via its own bespoke `ContextualToolbarSurfaceComposition` instead of this adapter).
      case OverlayRowLayout.PriorityColumns => SurfacePaintLayout.Columns

  /** Recovers the `SurfaceContentRowSlot`s a composition's paint boxes were placed at -- the inverse of the
    * header/item/key-hint/footer tagging `forResolved` (and `RowCompositionSupport.planWithRowHits`) applies, for
    * callers (contract/geometry cross-checks) that still want that shape rather than reaching into paint-box internals.
    *
    * Only meaningful for a composition tagged `composition.builtByRowsAdapter`: a menu/toolbar-shaped bespoke
    * composition's paint boxes were never tagged by this rule, so inverting them the same way would misreport their
    * content as plain item rows. Such a composition reports no row slots here.
    */
  def contentRowSlots(composition: ResolvedSurfaceComposition): List[SurfaceContentRowSlot] =
    if !composition.builtByRowsAdapter then Nil
    else
      composition.paintBoxes
        .foldLeft((0, List.empty[SurfaceContentRowSlot])) {
          case ((itemIndex, acc), box) =>
            val y = math.round(box.rect.y).toInt
            box.kind match
              case SurfacePaintKind.Heading =>
                (itemIndex, acc :+ SurfaceContentRowSlot(SurfaceContentRowKind.Header, y))
              case SurfacePaintKind.Footer =>
                (itemIndex, acc :+ SurfaceContentRowSlot(SurfaceContentRowKind.Footer, y))
              case SurfacePaintKind.KeyHint =>
                (itemIndex, acc :+ SurfaceContentRowSlot(SurfaceContentRowKind.KeyHint, y))
              case _ => (itemIndex + 1, acc :+ SurfaceContentRowSlot(SurfaceContentRowKind.Item(itemIndex), y))
        }
        ._2

  private def rowRect(bounds: LogicalPixelRect, row: Double): LogicalPixelRect =
    LogicalPixelRect(
      bounds.x,
      bounds.y + row,
      bounds.width,
      math.min(1.0, math.max(0.0, bounds.bottom - bounds.y - row))
    )
