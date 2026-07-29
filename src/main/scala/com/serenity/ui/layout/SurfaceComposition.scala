package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

/** Stable keyboard-focus identity for an interactive surface primitive. */
case class SurfaceFocusId(value: String)

/** Stable action identity for a selectable surface item. */
case class SurfaceActionId(value: String)

/** One selectable item in a declarative surface action list. */
case class SurfaceActionItem(
    text: String,
    actionId: SurfaceActionId,
    focusId: SurfaceFocusId,
    semanticLabel: String,
    selected: Boolean = false
)

/** Minimal immutable surface content primitives used by current Serenity controls. */
enum SurfacePrimitive:
  case Row(children: List[SurfacePrimitive])
  case Column(children: List[SurfacePrimitive])
  case Text(value: String)
  case Spacer(widthPx: Double, heightPx: Double)

  case TextInput(
      value: String,
      focusId: SurfaceFocusId,
      semanticLabel: String,
      minimumWidthPx: Double = 0.0,
      cursorOffset: Option[Int] = None
  )

  case ActionList(items: List[SurfaceActionItem])

/** Device-independent intrinsic size produced by the pure surface layout pass. */
case class SurfaceIntrinsicSize(width: Double, height: Double)

/** Immutable text metrics used to resolve proportional surface content in logical pixels. */
case class SurfaceCompositionMetrics(
    font: Font,
    fontRenderContext: FontRenderContext,
    lineHeightPx: Double
):

  /** Measure text with the same proportional shaping helper used by editor and overlay rendering. */
  def textWidth(text: String): Double =
    TextLayoutSnapshot.caretXsForText(text, font, fontRenderContext).lastOption.getOrElse(0.0f).toDouble

object SurfaceCompositionMetrics:

  /** Build immutable composition metrics from a UI font and render context. */
  def fromFont(font: Font, fontRenderContext: FontRenderContext): SurfaceCompositionMetrics =
    val lineHeight = math.max(1.0, font.getLineMetrics("Mg", fontRenderContext).getHeight.toDouble)
    SurfaceCompositionMetrics(font, fontRenderContext, lineHeight)

/** Paint operation emitted by the declarative surface layout. */
enum SurfacePaintKind:
  case Text
  case Spacer
  case TextInput
  case ActionItem

/** Layout strategy for text carried by a composed paint box. */
enum SurfacePaintLayout:
  case Plain
  case Split
  case Inline

/** One clipped paint box. Interactive boxes carry the same identity and rectangle as their hit region. */
case class SurfacePaintBox(
    kind: SurfacePaintKind,
    rect: LogicalPixelRect,
    text: Option[String] = None,
    focusId: Option[SurfaceFocusId] = None,
    actionId: Option[SurfaceActionId] = None,
    semanticLabel: Option[String] = None,
    selected: Boolean = false,
    cursorOffset: Option[Int] = None,
    segments: List[OverlaySegment] = Nil,
    layout: SurfacePaintLayout = SurfacePaintLayout.Plain
)

/** One semantic pointer target emitted from the same box used for painting. */
case class SurfaceHitRegion(
    rect: LogicalPixelRect,
    focusId: SurfaceFocusId,
    actionId: Option[SurfaceActionId],
    semanticLabel: String
)

/** Validation failures that prevent an interactive primitive from entering the resolved plan. */
enum SurfaceCompositionError:
  case EmptyFocusId
  case EmptyActionId
  case EmptySemanticLabel
  case DuplicateFocusId(id: SurfaceFocusId)
  case DuplicateActionId(id: SurfaceActionId)

/** Pure, immutable surface layout output shared by rendering, focus traversal, and pointer hit testing. */
case class ResolvedSurfaceComposition(
    bounds: LogicalPixelRect,
    intrinsicSize: SurfaceIntrinsicSize,
    paintBoxes: List[SurfacePaintBox],
    hitRegions: List[SurfaceHitRegion],
    focusOrder: List[SurfaceFocusId]
):

  /** Resolve the topmost semantic hit at a logical-pixel position. */
  def hitAt(pixelX: Double, pixelY: Double): Option[SurfaceHitRegion] =
    hitRegions.reverse.find(_.rect.contains(pixelX, pixelY))

object SurfaceComposition:

  /** Validate and resolve a declarative surface tree inside a logical-pixel content rectangle. */
  def layout(
    content: SurfacePrimitive,
    bounds: LogicalPixelRect,
    metrics: SurfaceCompositionMetrics
  ): Either[List[SurfaceCompositionError], ResolvedSurfaceComposition] =
    val errors = validationErrors(content)
    if errors.nonEmpty then Left(errors)
    else
      val intrinsic = intrinsicSize(content, metrics)
      val resolved  = resolve(content, bounds, metrics)
      Right(
        ResolvedSurfaceComposition(
          bounds = bounds,
          intrinsicSize = intrinsic,
          paintBoxes = resolved.paintBoxes,
          hitRegions = resolved.hitRegions,
          focusOrder = resolved.focusOrder
        )
      )

  private case class ResolvedChildren(
      paintBoxes: List[SurfacePaintBox],
      hitRegions: List[SurfaceHitRegion],
      focusOrder: List[SurfaceFocusId]
  ):

    def appended(other: ResolvedChildren): ResolvedChildren =
      ResolvedChildren(
        paintBoxes ++ other.paintBoxes,
        hitRegions ++ other.hitRegions,
        focusOrder ++ other.focusOrder
      )

  private object ResolvedChildren:
    val empty: ResolvedChildren = ResolvedChildren(Nil, Nil, Nil)

  private def intrinsicSize(
    primitive: SurfacePrimitive,
    metrics: SurfaceCompositionMetrics
  ): SurfaceIntrinsicSize =
    primitive match
      case SurfacePrimitive.Row(children) =>
        val sizes = children.map(intrinsicSize(_, metrics))
        SurfaceIntrinsicSize(sizes.map(_.width).sum, sizes.map(_.height).maxOption.getOrElse(0.0))
      case SurfacePrimitive.Column(children) =>
        val sizes = children.map(intrinsicSize(_, metrics))
        SurfaceIntrinsicSize(sizes.map(_.width).maxOption.getOrElse(0.0), sizes.map(_.height).sum)
      case SurfacePrimitive.Text(value) =>
        SurfaceIntrinsicSize(metrics.textWidth(value), metrics.lineHeightPx)
      case SurfacePrimitive.Spacer(widthPx, heightPx) =>
        SurfaceIntrinsicSize(math.max(0.0, widthPx), math.max(0.0, heightPx))
      case SurfacePrimitive.TextInput(value, _, _, minimumWidthPx, _) =>
        SurfaceIntrinsicSize(math.max(math.max(0.0, minimumWidthPx), metrics.textWidth(value)), metrics.lineHeightPx)
      case SurfacePrimitive.ActionList(items) =>
        SurfaceIntrinsicSize(
          items.map(item => metrics.textWidth(item.text)).maxOption.getOrElse(0.0),
          items.length * metrics.lineHeightPx
        )

  private def resolve(
    primitive: SurfacePrimitive,
    bounds: LogicalPixelRect,
    metrics: SurfaceCompositionMetrics
  ): ResolvedChildren =
    primitive match
      case SurfacePrimitive.Row(children) =>
        resolveRow(children, bounds, metrics)
      case SurfacePrimitive.Column(children) =>
        resolveColumn(children, bounds, metrics)
      case SurfacePrimitive.Text(value) =>
        paintLeaf(
          SurfacePaintBox(SurfacePaintKind.Text, bounds, text = Some(value)),
          intrinsicSize(primitive, metrics),
          bounds
        )
      case SurfacePrimitive.Spacer(_, _) =>
        paintLeaf(
          SurfacePaintBox(SurfacePaintKind.Spacer, bounds),
          intrinsicSize(primitive, metrics),
          bounds
        )
      case SurfacePrimitive.TextInput(value, focusId, semanticLabel, _, cursorOffset) =>
        interactiveLeaf(
          SurfacePaintKind.TextInput,
          value,
          focusId,
          None,
          semanticLabel,
          intrinsicSize(primitive, metrics),
          bounds,
          cursorOffset = Some(cursorOffset.getOrElse(value.length).max(0).min(value.length))
        )
      case SurfacePrimitive.ActionList(items) =>
        val width = math.min(bounds.width, intrinsicSize(primitive, metrics).width)
        items.zipWithIndex.foldLeft(ResolvedChildren.empty) {
          case (resolved, (item, index)) =>
            val itemBounds = LogicalPixelRect(
              bounds.x,
              bounds.y + index * metrics.lineHeightPx,
              width,
              metrics.lineHeightPx
            )
            resolved.appended(
              interactiveLeaf(
                SurfacePaintKind.ActionItem,
                item.text,
                item.focusId,
                Some(item.actionId),
                item.semanticLabel,
                SurfaceIntrinsicSize(width, metrics.lineHeightPx),
                itemBounds.intersection(bounds).getOrElse(LogicalPixelRect(bounds.x, bounds.y, 0.0, 0.0)),
                selected = item.selected
              )
            )
        }

  private def resolveRow(
    children: List[SurfacePrimitive],
    bounds: LogicalPixelRect,
    metrics: SurfaceCompositionMetrics
  ): ResolvedChildren =
    children
      .foldLeft((bounds.x, ResolvedChildren.empty)) {
        case ((nextX, resolved), child) =>
          val size = intrinsicSize(child, metrics)
          val proposed = LogicalPixelRect(
            nextX,
            bounds.y,
            size.width,
            math.min(bounds.height, size.height)
          )
          val childBounds = proposed.intersection(bounds).getOrElse(LogicalPixelRect(nextX, bounds.y, 0.0, 0.0))
          (nextX + size.width, resolved.appended(resolve(child, childBounds, metrics)))
      }
      ._2

  private def resolveColumn(
    children: List[SurfacePrimitive],
    bounds: LogicalPixelRect,
    metrics: SurfaceCompositionMetrics
  ): ResolvedChildren =
    children
      .foldLeft((bounds.y, ResolvedChildren.empty)) {
        case ((nextY, resolved), child) =>
          val size = intrinsicSize(child, metrics)
          val proposed = LogicalPixelRect(
            bounds.x,
            nextY,
            math.min(bounds.width, size.width),
            size.height
          )
          val childBounds = proposed.intersection(bounds).getOrElse(LogicalPixelRect(bounds.x, nextY, 0.0, 0.0))
          (nextY + size.height, resolved.appended(resolve(child, childBounds, metrics)))
      }
      ._2

  private def paintLeaf(
    paint: SurfacePaintBox,
    size: SurfaceIntrinsicSize,
    bounds: LogicalPixelRect
  ): ResolvedChildren =
    LogicalPixelRect(bounds.x, bounds.y, size.width, size.height)
      .intersection(bounds)
      .fold(ResolvedChildren.empty)(rect => ResolvedChildren(List(paint.copy(rect = rect)), Nil, Nil))

  private def interactiveLeaf(
    kind: SurfacePaintKind,
    text: String,
    focusId: SurfaceFocusId,
    actionId: Option[SurfaceActionId],
    semanticLabel: String,
    size: SurfaceIntrinsicSize,
    bounds: LogicalPixelRect,
    selected: Boolean = false,
    cursorOffset: Option[Int] = None
  ): ResolvedChildren =
    LogicalPixelRect(bounds.x, bounds.y, size.width, size.height)
      .intersection(bounds)
      .fold(ResolvedChildren.empty) { rect =>
        val paint = SurfacePaintBox(
          kind = kind,
          rect = rect,
          text = Some(text),
          focusId = Some(focusId),
          actionId = actionId,
          semanticLabel = Some(semanticLabel),
          selected = selected,
          cursorOffset = cursorOffset
        )
        val hit = SurfaceHitRegion(rect, focusId, actionId, semanticLabel)
        ResolvedChildren(List(paint), List(hit), List(focusId))
      }

  private def validationErrors(content: SurfacePrimitive): List[SurfaceCompositionError] =
    val direct    = collectValidationErrors(content)
    val focusIds  = collectFocusIds(content)
    val actionIds = collectActionIds(content)
    direct ++ duplicates(focusIds).map(SurfaceCompositionError.DuplicateFocusId.apply) ++
      duplicates(actionIds).map(SurfaceCompositionError.DuplicateActionId.apply)

  private def collectValidationErrors(content: SurfacePrimitive): List[SurfaceCompositionError] =
    content match
      case SurfacePrimitive.Row(children)                           => children.flatMap(collectValidationErrors)
      case SurfacePrimitive.Column(children)                        => children.flatMap(collectValidationErrors)
      case SurfacePrimitive.Text(_) | SurfacePrimitive.Spacer(_, _) => Nil
      case SurfacePrimitive.TextInput(_, focusId, semanticLabel, _, _) =>
        emptyFocusError(focusId) ++ emptyLabelError(semanticLabel)
      case SurfacePrimitive.ActionList(items) =>
        items.flatMap(item =>
          emptyFocusError(item.focusId) ++
            Option.when(item.actionId.value.trim.isEmpty)(SurfaceCompositionError.EmptyActionId).toList ++
            emptyLabelError(item.semanticLabel)
        )

  private def collectFocusIds(content: SurfacePrimitive): List[SurfaceFocusId] =
    content match
      case SurfacePrimitive.Row(children)                           => children.flatMap(collectFocusIds)
      case SurfacePrimitive.Column(children)                        => children.flatMap(collectFocusIds)
      case SurfacePrimitive.Text(_) | SurfacePrimitive.Spacer(_, _) => Nil
      case SurfacePrimitive.TextInput(_, focusId, _, _, _)          => List(focusId)
      case SurfacePrimitive.ActionList(items)                       => items.map(_.focusId)

  private def collectActionIds(content: SurfacePrimitive): List[SurfaceActionId] =
    content match
      case SurfacePrimitive.Row(children)     => children.flatMap(collectActionIds)
      case SurfacePrimitive.Column(children)  => children.flatMap(collectActionIds)
      case SurfacePrimitive.ActionList(items) => items.map(_.actionId)
      case _                                  => Nil

  private def emptyFocusError(focusId: SurfaceFocusId): List[SurfaceCompositionError] =
    Option.when(focusId.value.trim.isEmpty)(SurfaceCompositionError.EmptyFocusId).toList

  private def emptyLabelError(label: String): List[SurfaceCompositionError] =
    Option.when(label.trim.isEmpty)(SurfaceCompositionError.EmptySemanticLabel).toList

  private def duplicates[A](values: List[A]): List[A] =
    values
      .foldLeft((Set.empty[A], Set.empty[A], List.empty[A])) {
        case ((seen, duplicated, ordered), value) if seen.contains(value) && !duplicated.contains(value) =>
          (seen, duplicated + value, ordered :+ value)
        case ((seen, duplicated, ordered), value) =>
          (seen + value, duplicated, ordered)
      }
      ._3
