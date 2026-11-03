package com.serenity.ui.renderer

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.EditorPaneLayout

/** The sides of a pane's content that another pane's content abuts. Along such a side the pixels beyond the content
  * rect are the neighbour's, so nothing of this pane may be copied, kept or bled over them (#2481).
  */
final private[renderer] case class SharedSides(left: Boolean, right: Boolean)

private[renderer] object SharedSides:

  val Unshared: SharedSides = SharedSides(left = false, right = false)

  def of(paneId: PaneId, layouts: Map[PaneId, EditorPaneLayout]): SharedSides =
    layouts.get(paneId).fold(Unshared) { own =>
      val mine = own.contentRect
      val beside = layouts
        .collect {
          case (id, layout) if id != paneId => layout.contentRect
        }
        .filter(other => other.y < mine.bottom && mine.y < other.bottom)
        .toList
      SharedSides(left = beside.exists(_.right == mine.x), right = beside.exists(_.x == mine.right))
    }
