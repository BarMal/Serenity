package com.serenity.ui.layout

import com.serenity.state.models.SurfaceId

/** The rectangle-containment and overlap predicates `EditorLayoutContract.violations` is built from. Kept apart from
  * the contract itself so the contract file reads as the list of ownership rules, not the arithmetic behind them.
  */
private[layout] object LayoutContractChecks:

  private[layout] def containedBy(
    ownerName: String,
    ownerRect: LayoutRect,
    children: List[(String, Option[LayoutRect])]
  ): List[LayoutContractViolation] =
    children.collect {
      case (childName, Some(childRect)) if !ownerRect.containsRect(childRect) =>
        LayoutContractViolation(ownerName, childName, ownerRect, childRect)
    }

  private[layout] def rowSlotViolations(
    ownerPrefix: String,
    ownerRects: Map[SurfaceId, LayoutRect],
    rowSlotsBySurface: Map[SurfaceId, List[SurfaceContentRowSlot]]
  ): List[LayoutContractViolation] =
    rowSlotsBySurface.toList.flatMap {
      case (surfaceId, rowSlots) =>
        ownerRects.get(surfaceId).toList.flatMap { ownerRect =>
          rowSlots.collect {
            case rowSlot
                if !ownerRect.containsRect(
                  LayoutRect(ownerRect.x, rowSlot.y, ownerRect.width.max(0), if ownerRect.height > 0 then 1 else 0)
                ) =>
              LayoutContractViolation(
                s"$ownerPrefix ${surfaceId.value} content",
                s"$ownerPrefix ${surfaceId.value} ${rowSlot.kind} row slot",
                ownerRect,
                LayoutRect(ownerRect.x, rowSlot.y, ownerRect.width.max(0), 1)
              )
          }
        }
    }

  private[layout] def titleContentOverlapViolations(
    surfaceName: String,
    titleRect: Option[LayoutRect],
    contentRect: Option[LayoutRect]
  ): List[LayoutContractViolation] =
    (titleRect, contentRect) match
      case (Some(title), Some(content)) if rectanglesOverlap(title, content) =>
        List(LayoutContractViolation(s"$surfaceName title", s"$surfaceName content", title, content))
      case _ =>
        Nil

  private def rectanglesOverlap(first: LayoutRect, second: LayoutRect): Boolean =
    first.x < second.right && second.x < first.right && first.y < second.bottom && second.y < first.bottom
