package com.serenity.ui.layout

import com.serenity.config.CornerPosition
import com.serenity.state.models.*

/** Stacking multiple floating surfaces together: the below-cursor overlay stack, the vertical offsets that keep a
  * stack's fractional gap rows visually consistent, and the screen-corner overlay stack (issue #1310, mode 3). Split
  * out of `LayoutEngine` (600-line architecture ratchet); single-surface rect/size/anchor resolution lives in
  * [[FloatingSurfaceLayout]], which this calls back into.
  */
object OverlayStackLayout:

  final private[layout] case class BelowOverlayLayout(
      stack: List[(SurfaceId, LayoutRect)],
      collapsedSurfaceIds: Set[SurfaceId]
  )

  private[layout] def orderedBelowCursorSurfaces(state: AppState): List[UiSurface] =
    val maybeToolbar = state.contextualToolbarSurface.filter(isBelowCursorSurface).toList
    val maybeRunner  = state.commandRunnerSurface.filter(isBelowCursorSurface).toList
    if maybeToolbar.nonEmpty && maybeRunner.nonEmpty then maybeToolbar ++ maybeRunner
    else
      val belowSurfaces = state.floatingSurfaces.filter {
        _.presentation match
          case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) => true
          case _                                                             => false
      }
      state.persisted.focus match
        case Focus.Surface(surfaceId) =>
          belowSurfaces.find(_.id == surfaceId) match
            case Some(focused) => List(focused)
            case None          => belowSurfaces.headOption.toList
        case _ =>
          if belowSurfaces.nonEmpty then belowSurfaces.headOption.toList
          else
            state.cursorInfoBarSurface.filter {
              _.presentation match
                case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) => true
                case _                                                             => false
            }.toList

  private def isBelowCursorSurface(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) => true
      case _                                                             => false

  private[layout] def calculateBelowCursorOverlayStack(
    surfaces: List[UiSurface],
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout]
  ): BelowOverlayLayout =
    if surfaces.isEmpty then BelowOverlayLayout(Nil, Set.empty)
    else if surfaces.length == 1 then
      BelowOverlayLayout(
        surfaces.flatMap(surface =>
          FloatingSurfaceLayout.calculateFloatingSurfaceRect(surface, state, paneLayouts).map(surface.id -> _)
        ),
        Set.empty
      )
    else
      // The command-runner two-surface stack case (main palette + its settings-group submenu) is gone -- a settings
      // group now renders on the one `CommandPalette` surface instead of a second floating one (issue #1059), so
      // every below-cursor multi-surface case now goes through the same generic stacking.
      stackBelowCursorSurfaces(surfaces, state, paneLayouts)

  private[layout] def floatingOverlayOffsets(
    aboveSurfaces: List[UiSurface],
    aboveRects: List[(SurfaceId, LayoutRect)],
    belowSurfaces: List[UiSurface],
    belowRects: List[(SurfaceId, LayoutRect)],
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout]
  ): Map[SurfaceId, Double] =
    val aboveById = aboveSurfaces.map(surface => surface.id -> surface).toMap
    val aboveOffsets = aboveRects.flatMap { (surfaceId, rect) =>
      aboveById.get(surfaceId).flatMap { surface =>
        val gap = FloatingSurfaceLayout.floatingCursorGapRows(state, surface.content)
        FloatingSurfaceLayout.calculateFloatingAnchorFrame(surface, state, paneLayouts).map { anchorFrame =>
          surfaceId -> clampedFloatingOffset(
            rect,
            anchorFrame.contentRect,
            FloatingSurfaceLayout.wholeRowOrigin(gap).toDouble - gap
          )
        }
      }
    }
    val firstBelowDirection = for
      surface   <- belowSurfaces.headOption
      (_, rect) <- belowRects.headOption
      anchorY <- FloatingSurfaceLayout
        .calculateFloatingAnchorFrame(surface, state, paneLayouts)
        .map(_.screenPosition.y)
    yield if rect.y <= anchorY then -1.0 else 1.0
    val cursorRemainder = belowSurfaces.headOption
      .map(surface =>
        val gap = FloatingSurfaceLayout.floatingCursorGapRows(state, surface.content)
        gap - FloatingSurfaceLayout.wholeRowOrigin(gap)
      )
      .getOrElse(0.0)
    val stackGap        = FloatingSurfaceLayout.floatingStackGapRows(state)
    val stackRemainder  = stackGap - FloatingSurfaceLayout.wholeRowOrigin(stackGap)
    val direction       = firstBelowDirection.getOrElse(1.0)
    val belowSurfaceIds = belowSurfaces.map(_.id).toSet
    val belowContentRect = belowSurfaces.headOption
      .flatMap(surface => FloatingSurfaceLayout.calculateFloatingAnchorFrame(surface, state, paneLayouts))
      .map(_.contentRect)
    val belowOffsets = belowRects.zipWithIndex.collect {
      case ((surfaceId, rect), index) if belowSurfaceIds.contains(surfaceId) =>
        val desiredOffset = direction * (cursorRemainder + index * stackRemainder)
        surfaceId -> belowContentRect
          .map(contentRect => clampedFloatingOffset(rect, contentRect, desiredOffset))
          .getOrElse(desiredOffset)
    }
    (aboveOffsets ++ belowOffsets).toMap

  private def clampedFloatingOffset(rect: LayoutRect, contentRect: LayoutRect, desiredOffset: Double): Double =
    math.max(contentRect.y - rect.y, math.min(desiredOffset, contentRect.bottom - rect.bottom))

  private def stackBelowCursorSurfaces(
    surfaces: List[UiSurface],
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout]
  ): BelowOverlayLayout =
    val baseRects =
      surfaces.flatMap(surface =>
        FloatingSurfaceLayout.calculateFloatingSurfaceRect(surface, state, paneLayouts).map(surface -> _)
      )
    val anchorFrameOpt =
      surfaces.headOption.flatMap(surface =>
        FloatingSurfaceLayout.calculateFloatingAnchorFrame(surface, state, paneLayouts)
      )
    anchorFrameOpt match
      case None =>
        BelowOverlayLayout(Nil, Set.empty)
      case Some(_) if baseRects.isEmpty =>
        BelowOverlayLayout(Nil, Set.empty)
      case Some(anchorFrame) =>
        val gapRows = surfaces.headOption
          .map(surface =>
            FloatingSurfaceLayout.wholeRowOrigin(FloatingSurfaceLayout.floatingCursorGapRows(state, surface.content))
          )
          .getOrElse(0)
        val stackGapRows    = FloatingSurfaceLayout.wholeRowOrigin(FloatingSurfaceLayout.floatingStackGapRows(state))
        val availableBottom = anchorFrame.contentRect.bottom
        val totalHeight     = baseRects.map(_._2.height).sum + (stackGapRows * (baseRects.length - 1).max(0))
        val preferredBelowY = anchorFrame.screenPosition.y + 1 + gapRows
        val preferredAboveY = anchorFrame.screenPosition.y - gapRows - totalHeight
        val stackY =
          if preferredBelowY + totalHeight <= availableBottom then preferredBelowY
          else if preferredAboveY >= anchorFrame.contentRect.y then preferredAboveY
          else
            math.max(
              anchorFrame.contentRect.y,
              math.min(preferredBelowY, availableBottom - math.min(totalHeight, anchorFrame.contentRect.height))
            )
        val (_, stacked) = baseRects.foldLeft((stackY, List.empty[(SurfaceId, LayoutRect)])) {
          case ((currentY, acc), (surface, rect)) =>
            val heightBudget   = math.max(0, availableBottom - currentY)
            val adjustedHeight = math.min(rect.height, heightBudget)
            val adjustedY      = if adjustedHeight == 0 then availableBottom else currentY
            (
              adjustedY + adjustedHeight + stackGapRows,
              acc :+ (surface.id -> rect.copy(y = adjustedY, height = adjustedHeight))
            )
        }
        BelowOverlayLayout(stacked.filter(_._2.height > 0), Set.empty)

  /** One panel's slot in a corner stack (issue #1310, mode 3): an id plus preferred size, ordered from the screen edge
    * outward -- deliberately its own type rather than reusing `FloatingSurfaceLayout.FrozenPeekSlot`, since a corner
    * stack has no cursor anchor and a different overflow policy (collapse the tail, not clip each row).
    */
  final case class CornerPanelSlot(id: SurfaceId, preferredWidth: Int, preferredHeight: Int)

  final case class CornerOverlayLayout(stack: List[(SurfaceId, LayoutRect)], collapsedSurfaceIds: Set[SurfaceId])

  /** Lays out every panel assigned to one screen corner as a vertical list (issue #1310, mode 3), stacking from the
    * corner outward -- the first slot sits closest to the corner itself. Unlike `stackBelowCursorSurfaces`, which
    * shrinks each surface to fit, panels here either fit at their preferred height or collapse: once a slot's
    * cumulative height would exceed `contentRect.height`, that slot and every slot after it (in `slots` order) report
    * as `collapsedSurfaceIds` instead of a placed rect, for the caller to render as a "+N more" summary -- the same
    * idiom `SurfaceContentResolver.groupPreviewRows` already uses, applied to whole panels instead of rows.
    */
  def calculateCornerOverlayStack(
    position: CornerPosition,
    slots: List[CornerPanelSlot],
    contentRect: LayoutRect,
    gapRows: Int
  ): CornerOverlayLayout =
    if slots.isEmpty then CornerOverlayLayout(Nil, Set.empty)
    else
      val isBottom = position == CornerPosition.BottomLeft || position == CornerPosition.BottomRight
      val isRight  = position == CornerPosition.TopRight || position == CornerPosition.BottomRight

      val cumulativeHeights = slots.zipWithIndex
        .scanLeft(0) { case (used, (slot, index)) => used + slot.preferredHeight + (if index == 0 then 0 else gapRows) }
        .drop(1)
      val fitCount            = math.max(0, cumulativeHeights.lastIndexWhere(_ <= contentRect.height) + 1)
      val (visible, overflow) = slots.splitAt(fitCount)

      def xFor(width: Int): Int = if isRight then contentRect.right - width else contentRect.x

      val placed =
        if isBottom then
          visible
            .foldLeft((contentRect.bottom, List.empty[(SurfaceId, LayoutRect)])) {
              case ((bottomEdge, acc), slot) =>
                val y = bottomEdge - (if acc.isEmpty then 0 else gapRows) - slot.preferredHeight
                (
                  y,
                  acc :+ (slot.id -> LayoutRect(
                    xFor(slot.preferredWidth),
                    y,
                    slot.preferredWidth,
                    slot.preferredHeight
                  ))
                )
            }
            ._2
        else
          visible
            .foldLeft((contentRect.y, List.empty[(SurfaceId, LayoutRect)])) {
              case ((topEdge, acc), slot) =>
                val y = topEdge + (if acc.isEmpty then 0 else gapRows)
                (
                  y + slot.preferredHeight,
                  acc :+ (slot.id -> LayoutRect(
                    xFor(slot.preferredWidth),
                    y,
                    slot.preferredWidth,
                    slot.preferredHeight
                  ))
                )
            }
            ._2

      CornerOverlayLayout(placed, overflow.map(_.id).toSet)
