package com.serenity.state.models

import com.serenity.command.CommandScope
import com.serenity.ui.layout.PanelPosition

/** A heading in the Arrange Panels list, in the order the list reads: the four edges, then the hidden panels. */
enum ArrangementSection(val position: Option[PanelPosition]):
  case Top    extends ArrangementSection(Some(PanelPosition.Top))
  case Left   extends ArrangementSection(Some(PanelPosition.Left))
  case Right  extends ArrangementSection(Some(PanelPosition.Right))
  case Bottom extends ArrangementSection(Some(PanelPosition.Bottom))
  case Hidden extends ArrangementSection(None)

object ArrangementSection:

  def of(position: PanelPosition): ArrangementSection =
    position match
      case PanelPosition.Top    => Top
      case PanelPosition.Left   => Left
      case PanelPosition.Right  => Right
      case PanelPosition.Bottom => Bottom

/** Where a panel should go: docked at `position`, `index` panels in from that edge's start, or hidden when `None`. */
final case class PanelPlacement(id: PanelId, position: Option[PanelPosition], index: Int)

/** Every panel grouped under the edge it is docked to, in on-screen order (top to bottom on the left and right, left to
  * right along the top and bottom), then the hidden panels the current mode and frontend offer. Built from the layout,
  * so it always shows where panels really are; a move only ever asks for a [[PanelPlacement]] and waits for the layout
  * to change.
  */
final case class PanelArrangement(sections: Vector[(ArrangementSection, Vector[PanelId])], selected: Option[PanelId]):

  def panelsIn(section: ArrangementSection): Vector[PanelId] =
    sections.collectFirst { case (`section`, panels) => panels }.getOrElse(Vector.empty)

  /** Every panel in reading order -- the rows the selection moves through. */
  def rows: Vector[PanelId] = sections.flatMap(_._2)

  def selecting(id: PanelId): PanelArrangement =
    if rows.contains(id) then copy(selected = Some(id)) else this

  def selectionMoved(delta: Int): PanelArrangement =
    if rows.isEmpty then this
    else
      val current = selected.map(rows.indexOf).filter(_ >= 0).getOrElse(if delta > 0 then -1 else rows.size)
      copy(selected = rows.lift(Math.floorMod(current + delta, rows.size)))

  /** The same arrangement read again from `state`, still on the panel it was on if that panel is still listed. */
  def resynced(state: AppState): PanelArrangement =
    val fresh = PanelArrangement.of(state)
    selected.fold(fresh)(fresh.selecting)

  /** Where the selected panel goes when moved one step earlier (`delta < 0`) or later: past its neighbour on the same
    * edge, or across into the end of the edge before or the start of the edge after. Past the last edge it is hidden; a
    * hidden panel moved earlier comes back at the end of the bottom edge.
    */
  def moved(delta: Int): Option[PanelPlacement] =
    for
      id               <- selected
      (section, index) <- locate(id)
      placement        <- if delta < 0 then earlier(id, section, index) else later(id, section, index)
    yield placement

  /** Hides the selected panel, or shows a hidden one at the end of its default edge. */
  def toggled: Option[PanelPlacement] =
    for
      id           <- selected
      (section, _) <- locate(id)
    yield section.position match
      case Some(_) => PanelPlacement(id, None, 0)
      case None =>
        val edge = PanelRegistry.registrationFor(id).defaultPosition
        PanelPlacement(id, Some(edge), panelsIn(ArrangementSection.of(edge)).size)

  private def locate(id: PanelId): Option[(ArrangementSection, Int)] =
    sections.collectFirst { case (section, panels) if panels.contains(id) => (section, panels.indexOf(id)) }

  private def earlier(id: PanelId, section: ArrangementSection, index: Int): Option[PanelPlacement] =
    section match
      case ArrangementSection.Hidden =>
        Some(PanelPlacement(id, Some(PanelPosition.Bottom), panelsIn(ArrangementSection.Bottom).size))
      case _ if index > 0 => Some(PanelPlacement(id, section.position, index - 1))
      case _ =>
        ArrangementSection.values.lift(section.ordinal - 1).map { previous =>
          PanelPlacement(id, previous.position, panelsIn(previous).size)
        }

  private def later(id: PanelId, section: ArrangementSection, index: Int): Option[PanelPlacement] =
    section match
      case ArrangementSection.Hidden               => None
      case _ if index < panelsIn(section).size - 1 => Some(PanelPlacement(id, section.position, index + 1))
      case _ =>
        ArrangementSection.values.lift(section.ordinal + 1).map(next => PanelPlacement(id, next.position, 0))

object PanelArrangement:

  /** `state` with any open Arrange Panels list read again from its layout -- run at every commit, so a move shows as
    * soon as the panel has moved, and a panel shown or hidden any other way appears there too.
    */
  def resyncedIn(state: AppState): AppState =
    val updated = state.runtime.uiSurfaces.map {
      case surface @ UiSurface(_, SurfaceContent.ModalWorkflow(Modal.PanelArrangement(arrangement)), _, _) =>
        val next = arrangement.resynced(state)
        if next == arrangement then surface
        else surface.copy(content = SurfaceContent.ModalWorkflow(Modal.PanelArrangement(next)))
      case surface => surface
    }
    if updated.corresponds(state.runtime.uiSurfaces)(_ eq _) then state
    else state.copy(runtime = state.runtime.copy(uiSurfaces = updated))

  def of(state: AppState): PanelArrangement =
    val tree = state.persisted.layout.workspaceTree
    val dockedPanels = tree.toList.flatMap { workspaceTree =>
      workspaceTree.dockedSurfaceIds.flatMap { surfaceId =>
        for
          surface  <- state.surfaceById(surfaceId)
          id       <- PanelId.forContent(surface.content)
          position <- workspaceTree.positionForSurface(surfaceId)
        yield id -> position
      }
    }
    val shown   = dockedPanels.map(_._1).toSet
    val context = state.editingContext
    val hidden = PanelId.values.toVector.filter { id =>
      val registration = PanelRegistry.registrationFor(id)
      !shown.contains(id) && CommandScope(registration.family, registration.frontend).admits(context)
    }
    val sections = ArrangementSection.values.toVector.map { section =>
      section.position match
        case Some(edge) => section -> dockedPanels.collect { case (id, `edge`) => id }.toVector
        case None       => section -> hidden
    }
    val arrangement = PanelArrangement(sections, None)
    arrangement.copy(selected = arrangement.rows.headOption)
