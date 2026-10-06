package com.serenity.state.models

import com.serenity.ui.layout.PanelPosition

/** What the pointer is over, as far as its cursor shape is concerned -- resolved from the same hit regions mouse hover
  * and drag already use.
  */
enum PointerHitTarget:
  case Inert
  case EditorText
  case Control
  case DockEdge(position: PanelPosition)
  case TextAreaMargin(side: PanelPosition)

/** The cursor shape a frontend with a mouse pointer should show. A terminal has no such shape to set. */
enum PointerShape:
  case Default
  case Text
  case Hand
  case ResizeHorizontal
  case ResizeVertical

object PointerShape:

  def forTarget(target: PointerHitTarget): PointerShape =
    target match
      case PointerHitTarget.Inert                  => Default
      case PointerHitTarget.EditorText             => Text
      case PointerHitTarget.Control                => Hand
      case PointerHitTarget.DockEdge(position)     => resizing(position)
      case PointerHitTarget.TextAreaMargin(margin) => resizing(margin)

  private def resizing(position: PanelPosition): PointerShape =
    position match
      case PanelPosition.Left | PanelPosition.Right => ResizeHorizontal
      case PanelPosition.Top | PanelPosition.Bottom => ResizeVertical
