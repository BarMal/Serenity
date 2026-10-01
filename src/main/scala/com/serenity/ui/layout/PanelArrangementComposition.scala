package com.serenity.ui.layout

import com.serenity.config.{HotkeyTrigger, ModalKeyAction}
import com.serenity.state.models.{ArrangementSection, PanelArrangement, PanelId, PanelRegistry}

/** The Arrange Panels list: a title, each edge's heading with its panels indented under it, the hidden panels last, and
  * a footer naming the keys that move and show or hide the selected panel.
  */
object PanelArrangementComposition:

  private val ActionIdPrefix = "arrange-panel-"

  def rowActionId(id: PanelId): SurfaceActionId = SurfaceActionId(s"$ActionIdPrefix${id.key}")

  /** The panel a hit region's action id names -- only ever one [[rowActionId]] produced. */
  def rowPanel(actionId: String): Option[PanelId] =
    Option
      .when(actionId.startsWith(ActionIdPrefix))(actionId.stripPrefix(ActionIdPrefix))
      .flatMap(key => PanelId.values.find(_.key == key))

  def forArrangement(
    arrangement: PanelArrangement,
    frameRect: LayoutRect,
    modalBindings: Map[ModalKeyAction, List[HotkeyTrigger]]
  ): ResolvedSurfaceComposition =
    val content = SurfaceFrameLayout(frameRect).contentRect
    val bounds  = ModalSurfaceComposition.logicalRect(content.x, content.y, content.width, content.height)
    val lines   = (("Arrange Panels", OverlayTone.Normal, None) :: sectionLines(arrangement)) :+ footer(modalBindings)
    val boxes = lines.zipWithIndex.map {
      case ((text, tone, panel), row) =>
        ModalSurfaceComposition.textBox(
          text,
          ModalSurfaceComposition.rowRect(bounds, row),
          selected = panel.exists(arrangement.selected.contains),
          focusId = panel.map(id => SurfaceFocusId(rowActionId(id).value)),
          actionId = panel.map(rowActionId),
          tone = tone
        )
    }
    ModalSurfaceComposition.plan(bounds, boxes)

  def frameHeight(arrangement: PanelArrangement): Int =
    SurfaceFrameLayout.DefaultBorderCells * 2 + 2 + sectionLines(arrangement).size

  private def sectionLines(arrangement: PanelArrangement): List[(String, OverlayTone, Option[PanelId])] =
    arrangement.sections.toList.flatMap { (section, panels) =>
      val heading = (sectionLabel(section), OverlayTone.Accent, None)
      val rows =
        if panels.isEmpty then List(("  (none)", OverlayTone.Muted, None))
        else panels.toList.map(id => (s"  ${PanelRegistry.registrationFor(id).label}", OverlayTone.Normal, Some(id)))
      heading :: rows
    }

  private def sectionLabel(section: ArrangementSection): String =
    section match
      case ArrangementSection.Hidden => "Hidden"
      case edge                      => edge.toString

  private def footer(modalBindings: Map[ModalKeyAction, List[HotkeyTrigger]]): (String, OverlayTone, Option[PanelId]) =
    def key(action: ModalKeyAction) = modalBindings.get(action).flatMap(_.headOption).map(_.render).getOrElse("unbound")
    (
      s"${key(ModalKeyAction.MoveItemUp)}/${key(ModalKeyAction.MoveItemDown)} move · enter show/hide · esc close",
      OverlayTone.Muted,
      None
    )
